use super::Backend;
use crate::cpu::{
    flags::Flag,
    instrs::{read_instruction, Addressing, Instr, Operand},
    memory::Memory,
    Cpu, RunError,
};
use ffi_closure::Closure;
use inkwell::{
    builder::BuilderError,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    intrinsics::Intrinsic,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    values::{AnyValue, CallableValue, FunctionValue, IntValue, PointerValue, VectorValue},
    AddressSpace, IntPredicate, OptimizationLevel,
};
use std::{
    cell::{Cell, UnsafeCell},
    collections::{hash_map::Entry, HashMap},
    ffi::c_void,
    mem::offset_of,
    ops::Deref,
};

type Function = unsafe extern "C" fn(
    *mut crate::cpu::State,                // state ptr
    *mut c_void,                           // tick user_data
    unsafe extern "C" fn(u8, *mut c_void), // tick fn_ptr
    *mut c_void,                           // memory user_data
    *const c_void,                         // memory error
    unsafe extern "C" fn(*mut c_void, *const c_void, u16, *mut u8) -> i8, // memory read_u8
    unsafe extern "C" fn(*mut c_void, *const c_void, u16, *mut u16) -> i8, // memory read_u16
    unsafe extern "C" fn(*mut c_void, *const c_void, u16, u8) -> i8, // memory write_u8
    unsafe extern "C" fn(*mut c_void, *const c_void, u16, u16) -> i8, // memory write_u16
) -> u16;

const FUNTION_NAME: &str = "main";
const ACC_OFFSET: usize = offset_of!(crate::cpu::State, accumulator);
const X_OFFSET: usize = offset_of!(crate::cpu::State, x);
const Y_OFFSET: usize = offset_of!(crate::cpu::State, y);
const STACK_PTR_OFFSET: usize = offset_of!(crate::cpu::State, stack_ptr);
const FLAGS_OFFSET: usize = offset_of!(crate::cpu::State, flags);

pub struct Llvm<'cx> {
    compiled: HashMap<u16, Compiled<'cx>>,
    cx: Context,
}

impl<'cx> Llvm<'cx> {
    pub fn new() -> Self {
        return Self {
            cx: Context::create(),
            compiled: HashMap::new(),
        };
    }
}

impl<'cx> Backend for Llvm<'cx> {
    type Error = BuilderError;

    fn run<M: Memory>(
        cpu: &mut Cpu<M, Self>,
        mut pc: u16,
        mut tick: impl FnMut(&mut Cpu<M, Self>, u8),
    ) -> Result<(), RunError<M, Self>> {
        unsafe {
            let cpu = &*(cpu as *mut Cpu<M, Self> as *mut UnsafeCell<Cpu<M, Self>>);
            let tick = Closure::<dyn FnMut(u8)>::new(move |x| tick(&mut *cpu.get(), x));

            let state = std::ptr::addr_of_mut!((*cpu.get()).state);
            let memory = std::ptr::addr_of_mut!((*cpu.get()).memory);
            let backend = &mut (&mut *cpu.get()).backend;

            loop {
                pc = match backend.compiled.entry(pc) {
                    Entry::Occupied(entry) => entry.into_mut(),
                    Entry::Vacant(entry) => {
                        let mut builder =
                            Builder::new(pc, &backend.cx).map_err(RunError::Backend)?;
                        let mut prev_cycles = 0;

                        loop {
                            let prev_pc = builder.pc;
                            builder
                                .build_tick(
                                    builder.cx.i8_type().const_int(prev_cycles as u64, false),
                                )
                                .map_err(RunError::Backend)?;

                            // TODO Handle NMI interrupt

                            let Some(instr) = read_instruction(&mut *memory, &mut builder.pc)
                                .map_err(RunError::Memory)?
                            else {
                                panic!(
                                    "unknown instruction found at 0x{prev_pc:04X}: 0x{:02X}",
                                    (&mut *memory).read_u8(prev_pc).unwrap()
                                )
                            };

                            prev_cycles = instr.cycles();
                            log::trace!("{prev_pc:04X}: {instr:04X?}");

                            let is_terminating =
                                builder.translate_instr(instr).map_err(RunError::Backend)?;

                            if is_terminating {
                                builder.flush().map_err(RunError::Backend)?;
                                break;
                            }
                        }

                        optimize_module(&builder.module);
                        builder.module.print_to_stderr();
                        entry.insert(Compiled::new(builder.module))
                    }
                }
                .call(state, memory, &tick)
                .map_err(RunError::Memory)?;
            }
        }
    }
}

struct Builder<'cx> {
    pc: u16,
    prev_state: State<'cx>,

    state_ptr: PointerValue<'cx>,
    accumulator_ptr: PointerValue<'cx>,
    x_ptr: PointerValue<'cx>,
    y_ptr: PointerValue<'cx>,
    stack_ptr_ptr: PointerValue<'cx>,
    flags_ptr: PointerValue<'cx>,

    accumulator: IntValue<'cx>,
    x: IntValue<'cx>,
    y: IntValue<'cx>,
    stack_ptr: IntValue<'cx>,
    flags: VectorValue<'cx>,

    memory_data_ptr: PointerValue<'cx>,
    memory_error_ptr: PointerValue<'cx>,
    read_u8: PointerValue<'cx>,
    read_u16: PointerValue<'cx>,
    write_u8: PointerValue<'cx>,
    write_u16: PointerValue<'cx>,

    tick_data_ptr: PointerValue<'cx>,
    tick: PointerValue<'cx>,

    bswap: FunctionValue<'cx>,

    fn_value: FunctionValue<'cx>,
    builder: inkwell::builder::Builder<'cx>,
    module: Module<'cx>,
    cx: &'cx Context,
}

impl<'a> Builder<'a> {
    pub fn new(pc: u16, cx: &'a Context) -> Result<Self, BuilderError> {
        let module = cx.create_module("executable");
        let builder = cx.create_builder();

        let i8_type = cx.i8_type();
        let i16_type = cx.i16_type();
        let i64_type = cx.i64_type();
        let flags_type = cx.bool_type().vec_type(8);

        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());
        let i16_ptr_type = i16_type.ptr_type(AddressSpace::default());
        let void_ptr_type = i8_ptr_type;
        let tick_fn_ptr_type = cx
            .void_type()
            .fn_type(&[i8_type.into(), void_ptr_type.into()], false)
            .ptr_type(AddressSpace::default());
        let read_u8_fn_ptr_type = cx
            .i8_type()
            .fn_type(
                &[
                    void_ptr_type.into(),
                    void_ptr_type.into(),
                    i16_type.into(),
                    i8_ptr_type.into(),
                ],
                false,
            )
            .ptr_type(AddressSpace::default());
        let read_u16_fn_ptr_type = cx
            .i8_type()
            .fn_type(
                &[
                    void_ptr_type.into(),
                    void_ptr_type.into(),
                    i16_type.into(),
                    i16_ptr_type.into(),
                ],
                false,
            )
            .ptr_type(AddressSpace::default());
        let write_u8_fn_ptr_type = cx
            .i8_type()
            .fn_type(
                &[
                    void_ptr_type.into(),
                    void_ptr_type.into(),
                    i16_type.into(),
                    i8_type.into(),
                ],
                false,
            )
            .ptr_type(AddressSpace::default());
        let write_u16_fn_ptr_type = cx
            .i8_type()
            .fn_type(
                &[
                    void_ptr_type.into(),
                    void_ptr_type.into(),
                    i16_type.into(),
                    i16_type.into(),
                ],
                false,
            )
            .ptr_type(AddressSpace::default());

        let fn_type = i16_type.fn_type(
            &[
                i8_ptr_type.into(),           // state_ptr
                void_ptr_type.into(),         // tick user_data
                tick_fn_ptr_type.into(),      // tick fn_ptr
                void_ptr_type.into(),         // memory user_data
                void_ptr_type.into(),         // memory error_ptr
                read_u8_fn_ptr_type.into(),   // memory read_u8
                read_u16_fn_ptr_type.into(),  // memory read_u16
                write_u8_fn_ptr_type.into(),  // memory write_u8
                write_u16_fn_ptr_type.into(), // memory write_u16
            ],
            false,
        );
        let fn_value = module.add_function("main", fn_type, None);

        let entry_block = cx.append_basic_block(fn_value, "entry");
        builder.position_at_end(entry_block);

        let state_ptr = fn_value.get_first_param().unwrap().into_pointer_value();
        state_ptr.set_name("state_ptr");

        unsafe {
            let accumulator_ptr = builder.build_gep(
                state_ptr,
                &[i64_type.const_int(ACC_OFFSET as u64, false)],
                "accumulator_ptr",
            )?;

            let x_ptr = builder.build_gep(
                state_ptr,
                &[i64_type.const_int(X_OFFSET as u64, false)],
                "x_ptr",
            )?;

            let y_ptr = builder.build_gep(
                state_ptr,
                &[i64_type.const_int(Y_OFFSET as u64, false)],
                "y_ptr",
            )?;

            let stack_ptr_ptr = builder.build_gep(
                state_ptr,
                &[i64_type.const_int(STACK_PTR_OFFSET as u64, false)],
                "stack_ptr_ptr",
            )?;

            let flags_ptr = builder.build_gep(
                state_ptr,
                &[i64_type.const_int(FLAGS_OFFSET as u64, false)],
                "flags_ptr",
            )?;

            let accumulator = builder.build_load(accumulator_ptr, "")?.into_int_value();
            let x = builder.build_load(x_ptr, "")?.into_int_value();
            let y = builder.build_load(y_ptr, "")?.into_int_value();
            let stack_ptr = builder.build_load(stack_ptr_ptr, "")?.into_int_value();
            let flags = builder.build_load(flags_ptr, "")?.into_int_value();

            let mut this = Self {
                pc,
                bswap: Intrinsic::find("llvm.bswap")
                    .unwrap()
                    .get_declaration(&module, &[i16_type.into()])
                    .unwrap(),
                prev_state: State {
                    accumulator,
                    x,
                    y,
                    stack_ptr,
                    flags: flags_type.get_poison(),
                },
                state_ptr,
                accumulator_ptr,
                x_ptr,
                y_ptr,
                stack_ptr_ptr,
                flags_ptr,
                accumulator,
                x,
                y,
                stack_ptr,
                flags: flags_type.get_poison(),
                tick_data_ptr: fn_value.get_nth_param(1).unwrap().into_pointer_value(),
                tick: fn_value.get_nth_param(2).unwrap().into_pointer_value(),
                memory_data_ptr: fn_value.get_nth_param(3).unwrap().into_pointer_value(),
                memory_error_ptr: fn_value.get_nth_param(4).unwrap().into_pointer_value(),
                read_u8: fn_value.get_nth_param(5).unwrap().into_pointer_value(),
                read_u16: fn_value.get_nth_param(6).unwrap().into_pointer_value(),
                write_u8: fn_value.get_nth_param(7).unwrap().into_pointer_value(),
                write_u16: fn_value.get_nth_param(8).unwrap().into_pointer_value(),
                fn_value,
                builder,
                module,
                cx,
            };

            this.tick_data_ptr.set_name("tick_data_ptr");
            this.tick.set_name("ticks");
            this.memory_data_ptr.set_name("memory_data_ptr");
            this.memory_error_ptr.set_name("memory_error_ptr");
            this.read_u8.set_name("read_u8");
            this.read_u16.set_name("read_u16");
            this.write_u8.set_name("write_u8");
            this.write_u16.set_name("write_u16");

            this.flags_from_u8(flags)?;
            this.prev_state.flags = this.flags;

            return Ok(this);
        }
    }
}

impl<'a> Builder<'a> {
    fn translate_instr(&mut self, instr: Instr) -> Result<bool, BuilderError> {
        match instr {
            Instr::LDA(op) => {
                let (op, page_crossed) = self.translate_operand(op)?;
                self.accumulator = op;
                self.set_nz(op)?;
                self.handle_page_cross(page_crossed, 1)?;
            }
            Instr::LDX(op) => {
                let (op, page_crossed) = self.translate_operand(op)?;
                self.x = op;
                self.set_nz(op)?;
                self.handle_page_cross(page_crossed, 1)?;
            }
            Instr::LDY(op) => {
                let (op, page_crossed) = self.translate_operand(op)?;
                self.y = op;
                self.set_nz(op)?;
                self.handle_page_cross(page_crossed, 1)?;
            }
            Instr::STA(addr) => {
                let (addr, _) = self.translate_address(addr)?;
                self.build_write_u8(addr, self.accumulator)?;
            }
            Instr::STX(addr) => {
                let (addr, _) = self.translate_address(addr)?;
                self.build_write_u8(addr, self.x)?;
            }
            Instr::STY(addr) => {
                let (addr, _) = self.translate_address(addr)?;
                self.build_write_u8(addr, self.y)?;
            }
            Instr::TAX => {
                self.x = self.accumulator;
                self.set_nz(self.accumulator)?;
            }
            Instr::TAY => {
                self.y = self.accumulator;
                self.set_nz(self.accumulator)?;
            }
            Instr::TXA => {
                self.accumulator = self.x;
                self.set_nz(self.x)?;
            }
            Instr::TYA => {
                self.accumulator = self.y;
                self.set_nz(self.y)?;
            }
            Instr::TSX => {
                self.x = self.stack_ptr;
                self.set_nz(self.stack_ptr)?;
            }
            Instr::TXS => {
                self.stack_ptr = self.x;
            }
            Instr::PHA => todo!(),
            Instr::PHP => todo!(),
            Instr::PLA => todo!(),
            Instr::PLP => todo!(),
            Instr::AND(_) => todo!(),
            Instr::EOR(_) => todo!(),
            Instr::ORA(_) => todo!(),
            Instr::BIT(_) => todo!(),
            Instr::ADC(_) => todo!(),
            Instr::SBC(_) => todo!(),
            Instr::CMP(_) => todo!(),
            Instr::CPX(_) => todo!(),
            Instr::CPY(_) => todo!(),
            Instr::INC(_) => todo!(),
            Instr::INX => {
                self.x = self.build_int_add(self.x, self.cx.i8_type().const_int(1, false), "")?;
                self.set_nz(self.x)?;
            }
            Instr::INY => {
                self.y = self.build_int_add(self.y, self.cx.i8_type().const_int(1, false), "")?;
                self.set_nz(self.y)?;
            }
            Instr::DEC(_) => todo!(),
            Instr::DEX => {
                self.x = self.build_int_sub(self.x, self.cx.i8_type().const_int(1, false), "")?;
                self.set_nz(self.x)?;
            }
            Instr::DEY => {
                self.x = self.build_int_sub(self.x, self.cx.i8_type().const_int(1, false), "")?;
                self.set_nz(self.x)?;
            }
            Instr::ASL(_) => todo!(),
            Instr::LSR(_) => todo!(),
            Instr::ROL(_) => todo!(),
            Instr::ROR(_) => todo!(),
            Instr::JMP(addr) => {
                self.build_return(Some(&self.cx.i16_type().const_int(addr as u64, false)))?;
                return Ok(true);
            }
            Instr::JMPIndirect(addr) => {
                let next_pc =
                    self.build_read_u16(self.cx.i16_type().const_int(addr as u64, false))?;
                self.build_return(Some(&next_pc))?;
                return Ok(true);
            }
            Instr::JSR(addr) => {
                self.stack_push_u16(
                    self.cx
                        .i16_type()
                        .const_int(self.pc.wrapping_sub(1) as u64, false),
                )?;
                self.build_return(Some(&self.cx.i16_type().const_int(addr as u64, false)))?;
                return Ok(true);
            }
            Instr::RTS => {
                let next_pc = self.stack_pop_u16()?;
                self.build_return(Some(&self.build_int_add(
                    next_pc,
                    self.cx.i16_type().const_int(1, false),
                    "",
                )?))?;
                return Ok(true);
            }
            Instr::BCC(addr) => self.build_conditional_jump(Flag::Carry, true, addr)?,
            Instr::BCS(addr) => self.build_conditional_jump(Flag::Carry, false, addr)?,
            Instr::BEQ(addr) => self.build_conditional_jump(Flag::Zero, false, addr)?,
            Instr::BMI(addr) => self.build_conditional_jump(Flag::Negative, false, addr)?,
            Instr::BNE(addr) => self.build_conditional_jump(Flag::Zero, true, addr)?,
            Instr::BPL(addr) => self.build_conditional_jump(Flag::Negative, true, addr)?,
            Instr::BVC(addr) => self.build_conditional_jump(Flag::Overflow, true, addr)?,
            Instr::BVS(addr) => self.build_conditional_jump(Flag::Overflow, false, addr)?,
            Instr::CLC => self.remove_flag(Flag::Carry)?,
            Instr::CLD => self.remove_flag(Flag::Decimal)?,
            Instr::CLI => self.remove_flag(Flag::InterruptDisable)?,
            Instr::CLV => self.remove_flag(Flag::Overflow)?,
            Instr::SEC => self.insert_flag(Flag::Carry)?,
            Instr::SED => self.insert_flag(Flag::Decimal)?,
            Instr::SEI => self.insert_flag(Flag::InterruptDisable)?,
            Instr::BRK => todo!(),
            Instr::NOP => {}
            Instr::RTI => {
                let next_flags = self.stack_pop()?;
                self.flags_from_u8(next_flags)?;

                let next_pc = self.stack_pop_u16()?;
                self.build_return(Some(&next_pc))?;
                return Ok(true);
            }
        };
        return Ok(false);
    }

    fn flush(&mut self) -> Result<(), BuilderError> {
        if self.accumulator != self.prev_state.accumulator {
            self.build_store(self.accumulator_ptr, self.accumulator)?;
        }

        if self.x != self.prev_state.x {
            self.build_store(self.x_ptr, self.x)?;
        }

        if self.y != self.prev_state.y {
            self.build_store(self.y_ptr, self.y)?;
        }

        if self.stack_ptr != self.prev_state.stack_ptr {
            self.build_store(self.stack_ptr_ptr, self.stack_ptr)?;
        }

        if self.flags != self.prev_state.flags {
            let flags = self.flags_into_u8(false)?;
            self.build_store(self.flags_ptr, flags)?;
        }

        self.prev_state = State {
            accumulator: self.accumulator,
            x: self.x,
            y: self.y,
            stack_ptr: self.stack_ptr,
            flags: self.flags,
        };

        return Ok(());
    }

    fn handle_page_cross(
        &mut self,
        page_crossed: Option<IntValue<'a>>,
        ticks: u8,
    ) -> Result<(), BuilderError> {
        if let Some(page_crossed) = page_crossed {
            self.flush()?;

            let then_block = self.cx.append_basic_block(self.fn_value, "");
            let continue_block = self.cx.append_basic_block(self.fn_value, "");
            self.build_conditional_branch(page_crossed, then_block, continue_block)?;

            let builder = self.cx.create_builder();
            builder.position_at_end(then_block);
            builder.build_call(
                CallableValue::try_from(self.tick).unwrap(),
                &[
                    self.cx.i8_type().const_int(ticks as u64, false).into(),
                    self.tick_data_ptr.into(),
                ],
                "",
            )?;
            builder.build_unconditional_branch(continue_block)?;

            self.builder.position_at_end(continue_block);
        }

        return Ok(());
    }

    fn translate_operand(
        &mut self,
        op: Operand,
    ) -> Result<(IntValue<'a>, Option<IntValue<'a>>), BuilderError> {
        return Ok(match op {
            Operand::Accumulator => (self.accumulator, None),
            Operand::Immediate(imm) => (self.cx.i8_type().const_int(imm as u64, false), None),
            Operand::Addressing(addr) => {
                let (addr, page_crossed) = self.translate_address(addr)?;
                (self.build_read_u8(addr)?, page_crossed)
            }
        });
    }

    /// Returns the 16-bit address of the value to be read
    fn translate_address(
        &mut self,
        addr: Addressing,
    ) -> Result<(IntValue<'a>, Option<IntValue<'a>>), BuilderError> {
        let i8_type = self.cx.i8_type();
        let i16_type = self.cx.i16_type();

        return Ok(match addr {
            Addressing::ZeroPage(addr) => (i16_type.const_int(addr as u64, false), None),
            Addressing::ZeroPageX(addr) => (
                self.build_int_z_extend(
                    self.build_int_add(i8_type.const_int(addr as u64, false), self.x, "")?,
                    i16_type,
                    "",
                )?,
                None,
            ),
            Addressing::ZeroPageY(addr) => (
                self.build_int_z_extend(
                    self.build_int_add(i8_type.const_int(addr as u64, false), self.y, "")?,
                    i16_type,
                    "",
                )?,
                None,
            ),
            Addressing::Absolute(addr) => (i16_type.const_int(addr as u64, false), None),
            Addressing::AbsoluteX(base) => {
                let base = i16_type.const_int(base as u64, false);
                let addr =
                    self.build_int_add(base, self.build_int_z_extend(self.x, i16_type, "")?, "")?;
                (addr, Some(self.page_crossed(base, addr)?))
            }
            Addressing::AbsoluteY(base) => {
                let base = i16_type.const_int(base as u64, false);
                let addr =
                    self.build_int_add(base, self.build_int_z_extend(self.y, i16_type, "")?, "")?;
                (addr, Some(self.page_crossed(base, addr)?))
            }
            Addressing::IndexedIndirect(base) => {
                let subptr =
                    self.build_int_add(i8_type.const_int(base as u64, false), self.x, "")?;
                (self.build_read_u16(subptr)?, None)
            }
            Addressing::IndirectIndexed(base) => {
                let base = self.build_read_u16(i16_type.const_int(base as u64, false))?;
                let addr = self.build_int_add(base, self.y, "")?;
                (addr, Some(self.page_crossed(base, addr)?))
            }
        });
    }

    fn page_crossed(
        &self,
        addr1: IntValue<'a>,
        addr2: IntValue<'a>,
    ) -> Result<IntValue<'a>, BuilderError> {
        return self.build_int_compare(
            IntPredicate::UGT,
            self.build_xor(addr2, addr1, "")?,
            self.cx.i16_type().const_int(0xff, false),
            "",
        );
    }
}

impl<'a> Builder<'a> {
    fn build_tick(&mut self, ticks: IntValue<'a>) -> Result<(), BuilderError> {
        self.flush()?;
        self.build_call(
            CallableValue::try_from(self.tick).unwrap(),
            &[ticks.into(), self.tick_data_ptr.into()],
            "",
        )?;
        return Ok(());
    }

    fn build_conditional_jump(
        &mut self,
        flag: Flag,
        cleared: bool,
        addr: u16,
    ) -> Result<(), BuilderError> {
        let i8_type = self.cx.i8_type();
        let i16_type = self.cx.i16_type();
        let addr = i16_type.const_int(addr as u64, false);

        let mut cond = self.get_flag(flag)?;
        if cleared {
            cond = self.build_not(cond, "")?;
        }

        let then_block = self.cx.append_basic_block(self.fn_value, "");
        let continue_block = self.cx.append_basic_block(self.fn_value, "");
        self.build_conditional_branch(cond, then_block, continue_block)?;

        self.builder.position_at_end(then_block);
        let page_crossed = self.page_crossed(i16_type.const_int(self.pc as u64, false), addr)?;
        let ticks = self.build_int_add(
            self.build_int_z_extend(page_crossed, i8_type, "")?,
            i8_type.const_int(1, false),
            "",
        )?;
        self.build_tick(ticks)?;
        self.builder.build_return(Some(&addr))?;

        self.builder.position_at_end(continue_block);
        return Ok(());
    }

    pub fn build_read_u8(&self, ptr: IntValue<'a>) -> Result<IntValue<'a>, BuilderError> {
        let output = self.build_alloca(self.cx.i8_type(), "")?;
        let res = self
            .build_call(
                CallableValue::try_from(self.read_u8).unwrap(),
                &[
                    self.memory_data_ptr.into(),
                    self.memory_error_ptr.into(),
                    ptr.into(),
                    output.into(),
                ],
                "",
            )?
            .as_any_value_enum()
            .into_int_value();

        self.handle_memory_result(res)?;
        return Ok(self.build_load(output, "")?.into_int_value());
    }

    pub fn build_read_u16(&self, ptr: IntValue<'a>) -> Result<IntValue<'a>, BuilderError> {
        let output = self.build_alloca(self.cx.i16_type(), "")?;
        let res = self
            .build_call(
                CallableValue::try_from(self.read_u16).unwrap(),
                &[
                    self.memory_data_ptr.into(),
                    self.memory_error_ptr.into(),
                    ptr.into(),
                    output.into(),
                ],
                "",
            )?
            .as_any_value_enum()
            .into_int_value();

        self.handle_memory_result(res)?;
        return Ok(self.build_load(output, "")?.into_int_value());
    }

    pub fn build_write_u8(&self, ptr: IntValue<'a>, val: IntValue<'a>) -> Result<(), BuilderError> {
        let res = self
            .build_call(
                CallableValue::try_from(self.write_u8).unwrap(),
                &[
                    self.memory_data_ptr.into(),
                    self.memory_error_ptr.into(),
                    ptr.into(),
                    val.into(),
                ],
                "",
            )?
            .as_any_value_enum()
            .into_int_value();

        return self.handle_memory_result(res);
    }

    pub fn build_write_u16(
        &self,
        ptr: IntValue<'a>,
        val: IntValue<'a>,
    ) -> Result<(), BuilderError> {
        let res = self
            .build_call(
                CallableValue::try_from(self.write_u16).unwrap(),
                &[
                    self.memory_data_ptr.into(),
                    self.memory_error_ptr.into(),
                    ptr.into(),
                    val.into(),
                ],
                "",
            )?
            .as_any_value_enum()
            .into_int_value();

        return self.handle_memory_result(res);
    }

    pub fn build_bswap(&self, val: IntValue<'a>) -> Result<IntValue<'a>, BuilderError> {
        return Ok(self
            .build_call(self.bswap, &[val.into()], "")?
            .as_any_value_enum()
            .into_int_value());
    }

    #[inline]
    pub fn build_to_le(&self, val: IntValue<'a>) -> Result<IntValue<'a>, BuilderError> {
        cfg_if::cfg_if! {
            if #[cfg(target_endian = "big")] {
                return self.build_bswap(val)
            } else {
                return Ok(val)
            }
        }
    }

    #[inline]
    pub fn build_to_le_bytes(&self, val: IntValue<'a>) -> Result<[IntValue<'a>; 2], BuilderError> {
        let val = self.build_to_le(val)?;
        let vector = self
            .build_bitcast(val, self.cx.i8_type().vec_type(2), "")?
            .into_vector_value();

        return Ok([
            self.build_extract_element(vector, self.cx.i8_type().const_int(0, false), "")?
                .into_int_value(),
            self.build_extract_element(vector, self.cx.i8_type().const_int(1, false), "")?
                .into_int_value(),
        ]);
    }

    #[inline]
    pub fn build_from_le_bytes(
        &self,
        lo: IntValue<'a>,
        hi: IntValue<'a>,
    ) -> Result<IntValue<'a>, BuilderError> {
        let i8_type = self.cx.i8_type();

        let vector = i8_type.vec_type(2).get_undef();
        let vector = self.build_insert_element(vector, lo, i8_type.const_int(0, false), "")?;
        let vector = self.build_insert_element(vector, hi, i8_type.const_int(1, false), "")?;

        let int = self
            .build_bitcast(vector, self.cx.i16_type(), "")?
            .into_int_value();

        return self.build_to_le(int);
    }

    fn handle_memory_result(&self, res: IntValue<'a>) -> Result<(), BuilderError> {
        let condition =
            self.build_int_compare(IntPredicate::SLT, res, self.cx.i8_type().const_zero(), "")?;

        let then_block = self.cx.append_basic_block(self.fn_value, "");
        let continue_block = self.cx.append_basic_block(self.fn_value, "");
        self.build_conditional_branch(condition, then_block, continue_block)?;

        let builder = self.cx.create_builder();
        builder.position_at_end(then_block);
        builder.build_return(Some(&self.cx.i16_type().get_poison()))?;

        self.builder.position_at_end(continue_block);
        return Ok(());
    }
}

impl<'a> Deref for Builder<'a> {
    type Target = inkwell::builder::Builder<'a>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.builder
    }
}

struct State<'cx> {
    accumulator: IntValue<'cx>,
    x: IntValue<'cx>,
    y: IntValue<'cx>,
    stack_ptr: IntValue<'cx>,
    flags: VectorValue<'cx>,
}

// Flag ops
impl<'a> Builder<'a> {
    #[inline]
    pub fn insert_flag(&mut self, flag: Flag) -> Result<(), BuilderError> {
        self.flags = self.build_insert_element(
            self.flags,
            self.cx.bool_type().const_int(true as u64, false),
            self.cx.i8_type().const_int(flag as u64, false),
            "",
        )?;
        return Ok(());
    }

    #[inline]
    pub fn remove_flag(&mut self, flag: Flag) -> Result<(), BuilderError> {
        self.flags = self.build_insert_element(
            self.flags,
            self.cx.bool_type().const_int(false as u64, false),
            self.cx.i8_type().const_int(flag as u64, false),
            "",
        )?;
        return Ok(());
    }

    #[inline]
    pub fn set_flag(&mut self, flag: Flag, value: IntValue<'a>) -> Result<(), BuilderError> {
        self.flags = self.build_insert_element(
            self.flags,
            value,
            self.cx.i8_type().const_int(flag as u64, false),
            "",
        )?;
        return Ok(());
    }

    #[inline]
    pub fn set_nz(&mut self, val: IntValue<'a>) -> Result<(), BuilderError> {
        let i8_zero = self.cx.i8_type().const_zero();
        let zero = self.build_int_compare(IntPredicate::EQ, val, i8_zero, "")?;
        let negative = self.build_int_compare(IntPredicate::SLT, val, i8_zero, "")?;

        self.set_flag(Flag::Zero, zero)?;
        self.set_flag(Flag::Negative, negative)?;
        return Ok(());
    }

    #[inline]
    pub fn get_flag(&self, flag: Flag) -> Result<IntValue<'a>, BuilderError> {
        return Ok(self
            .build_extract_element(
                self.flags,
                self.cx.i8_type().const_int(flag as u64, false),
                "",
            )?
            .into_int_value());
    }

    // http://wiki.nesdev.com/w/index.php/CPU_status_flag_behavior
    pub fn flags_into_u8(&self, from_interrupt: bool) -> Result<IntValue<'a>, BuilderError> {
        let i8_type = self.cx.i8_type();

        let bits = self
            .build_bitcast(self.flags, i8_type, "")?
            .into_int_value();

        return self.build_or(
            bits,
            i8_type.const_int((1 << 5) | ((!from_interrupt) as u64) << 4, false),
            "",
        );
    }

    #[inline(always)]
    pub fn flags_from_u8(&mut self, val: IntValue<'a>) -> Result<(), BuilderError> {
        self.flags = self
            .build_bitcast(val, self.cx.bool_type().vec_type(8), "")?
            .into_vector_value();

        // Remove bit 4
        self.flags = self.build_insert_element(
            self.flags,
            self.cx.bool_type().const_int(false as u64, false),
            self.cx.i8_type().const_int(4, false),
            "",
        )?;

        // Insert bit 5
        self.flags = self.build_insert_element(
            self.flags,
            self.cx.bool_type().const_int(true as u64, false),
            self.cx.i8_type().const_int(5, false),
            "",
        )?;

        return Ok(());
    }
}

// Stack Ops
impl<'a> Builder<'a> {
    pub fn stack_push(&mut self, val: IntValue<'a>) -> Result<(), BuilderError> {
        self.build_write_u8(self.stack_addr()?, val)?;
        self.stack_ptr =
            self.build_int_sub(self.stack_ptr, self.cx.i8_type().const_int(1, false), "")?;
        return Ok(());
    }

    pub fn stack_push_u16(&mut self, val: IntValue<'a>) -> Result<(), BuilderError> {
        let [lo, hi] = self.build_to_le_bytes(val)?;
        self.stack_push(hi)?;
        self.stack_push(lo)?;
        return Ok(());
    }

    pub fn stack_pop(&mut self) -> Result<IntValue<'a>, BuilderError> {
        self.stack_ptr =
            self.build_int_add(self.stack_ptr, self.cx.i8_type().const_int(1, false), "")?;
        return self.build_read_u8(self.stack_addr()?);
    }

    pub fn stack_pop_u16(&mut self) -> Result<IntValue<'a>, BuilderError> {
        let lo = self.stack_pop()?;
        let hi = self.stack_pop()?;
        return self.build_from_le_bytes(lo, hi);
    }

    fn stack_addr(&self) -> Result<IntValue<'a>, BuilderError> {
        let i16_type = self.cx.i16_type();
        return self.build_int_add(
            i16_type.const_int(0x100, false),
            self.build_int_z_extend(self.stack_ptr, i16_type, "")?,
            "",
        );
    }
}

unsafe extern "C" fn read_u8<M: Memory>(
    memory: *mut c_void,
    latest_error: *const c_void,
    addr: u16,
    dst: *mut u8,
) -> i8 {
    match (&mut *memory.cast::<M>()).read_u8(addr) {
        Ok(res) => {
            *dst = res;
            return 0;
        }
        Err(e) => {
            (&*latest_error.cast::<Cell<Option<M::Error>>>()).set(Some(e));
            return -1;
        }
    }
}

unsafe extern "C" fn read_u16<M: Memory>(
    memory: *mut c_void,
    latest_error: *const c_void,
    addr: u16,
    dst: *mut u16,
) -> i8 {
    match (&mut *memory.cast::<M>()).read_u16(addr) {
        Ok(res) => {
            *dst = res;
            return 0;
        }
        Err(e) => {
            (&*latest_error.cast::<Cell<Option<M::Error>>>()).set(Some(e));
            return -1;
        }
    }
}

unsafe extern "C" fn write_u8<M: Memory>(
    memory: *mut c_void,
    latest_error: *const c_void,
    addr: u16,
    val: u8,
) -> i8 {
    match (&mut *memory.cast::<M>()).write_u8(addr, val) {
        Ok(()) => {
            return 0;
        }
        Err(e) => {
            (&*latest_error.cast::<Cell<Option<M::Error>>>()).set(Some(e));
            return -1;
        }
    }
}

unsafe extern "C" fn write_u16<M: Memory>(
    memory: *mut c_void,
    latest_error: *const c_void,
    addr: u16,
    val: u16,
) -> i8 {
    match (&mut *memory.cast::<M>()).write_u16(addr, val) {
        Ok(()) => {
            return 0;
        }
        Err(e) => {
            (&*latest_error.cast::<Cell<Option<M::Error>>>()).set(Some(e));
            return -1;
        }
    }
}

struct Compiled<'cx> {
    f: JitFunction<'cx, Function>,
    _exec: ExecutionEngine<'cx>,
}

impl<'cx> Compiled<'cx> {
    pub fn new(module: Module<'cx>) -> Self {
        let exec = module
            .create_jit_execution_engine(OptimizationLevel::Default)
            .unwrap();

        return Self {
            f: unsafe { exec.get_function(FUNTION_NAME).unwrap() },
            _exec: exec,
        };
    }

    #[inline]
    pub unsafe fn call<'a, M: Memory>(
        &mut self,
        state: *mut crate::cpu::State,
        memory: *mut M,
        tick: &Closure<dyn 'a + FnMut(u8)>,
    ) -> Result<u16, M::Error> {
        let last_memory_error = Cell::new(None::<M::Error>);
        let next_pc = self.f.call(
            state,
            tick.user_data(),
            tick.fn_ptr(),
            memory as *mut M as *mut c_void,
            std::ptr::addr_of!(last_memory_error).cast(),
            read_u8::<M>,
            read_u16::<M>,
            write_u8::<M>,
            write_u16::<M>,
        );

        return match last_memory_error.take() {
            Some(err) => Err(err),
            None => Ok(next_pc),
        };
    }
}

fn optimize_module(module: &Module) -> bool {
    let builder = PassManagerBuilder::create();
    builder.set_optimization_level(OptimizationLevel::Default);

    let manager = PassManager::create(());
    builder.populate_module_pass_manager(&manager);

    return manager.run_on(module);
}
