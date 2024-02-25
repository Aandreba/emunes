use super::Backend;
use crate::cpu::{
    flags::{Flag, Flags},
    instrs::{read_instruction, Addressing, Instr, Operand},
    memory::Memory,
    RunError,
};
use ffi_closure::Closure;
use inkwell::{
    basic_block::BasicBlock,
    builder::BuilderError,
    context::Context,
    execution_engine::ExecutionEngine,
    memory_buffer::MemoryBuffer,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    support::LLVMString,
    values::{AnyValue, FunctionValue, IntValue, PhiValue, PointerValue, StructValue, VectorValue},
    AddressSpace, IntPredicate, OptimizationLevel,
};
use std::{
    collections::{hash_map::Entry, HashMap},
    ffi::c_void,
    marker::PhantomData,
};

const SKELETON: &[u8] = include_bytes!("../../../skeleton.ll");

pub struct Llvm<'a> {
    compiled: HashMap<u16, FunctionValue<'a>>,
    ee: ExecutionEngine<'a>,
    module: Module<'a>,
    cx: &'a Context,
}

impl<'a> Llvm<'a> {
    pub fn new(cx: &'a Context) -> Result<Self, LLVMString> {
        let ir =
            MemoryBuffer::create_from_memory_range_copy(&SKELETON[..SKELETON.len() - 1], "main");
        let module = cx.create_module_from_ir(ir)?;
        let ee = module.create_jit_execution_engine(OptimizationLevel::Less)?;

        for fn_value in module.get_functions() {
            if fn_value.get_first_basic_block().is_some() {
                optimize_fn(fn_value, &module)
            }
        }

        return Ok(Self {
            compiled: HashMap::new(),
            ee,
            module,
            cx,
        });
    }

    pub fn print_to_stderr(&self) {
        self.module.print_to_stderr();
    }

    fn _run<M: crate::cpu::memory::Memory>(
        cpu: &mut crate::cpu::Cpu<M, Self>,
        mut pc: u16,
        tick: impl FnMut(u8),
    ) -> Result<u16, RunError<M, Self>> {
        let this = &mut cpu.backend;
        let input_pc = pc;

        let fn_value = match this.compiled.entry(pc) {
            Entry::Occupied(entry) => *entry.into_mut(),
            Entry::Vacant(_) => {
                let mut builder = Builder::new(pc, cpu.decimal_enabled, &this.module, this.cx)
                    .map_err(RunError::Backend)?;
                let mut prev_cycles = 0;

                let next_pc = loop {
                    let prev_pc = pc;
                    builder
                        .tick(builder.cx.i8_type().const_int(prev_cycles, false))
                        .map_err(RunError::Backend)?;

                    let instr = read_instruction(&cpu.memory, &mut pc)
                        .map_err(RunError::Memory)?
                        .expect("unknown instruction opcode");

                    prev_cycles = instr.cycles() as u64;
                    log::trace!("{prev_pc:04X}: {instr:04X?}");

                    match instr {
                        Instr::LDA(op) => builder.lda(op),
                        Instr::LDX(op) => builder.ldx(op),
                        Instr::LDY(op) => builder.ldy(op),
                        Instr::STA(addr) => {
                            let (addr, _) = builder.get_address(addr).map_err(RunError::Backend)?;
                            builder.write_u8(addr, builder.accumulator)
                        }
                        Instr::STX(addr) => {
                            let (addr, _) = builder.get_address(addr).map_err(RunError::Backend)?;
                            builder.write_u8(addr, builder.x)
                        }
                        Instr::STY(addr) => {
                            let (addr, _) = builder.get_address(addr).map_err(RunError::Backend)?;
                            builder.write_u8(addr, builder.y)
                        }
                        Instr::TAX => {
                            builder.x = builder.accumulator;
                            builder.set_nz(builder.accumulator)
                        }
                        Instr::TAY => {
                            builder.y = builder.accumulator;
                            builder.set_nz(builder.accumulator)
                        }
                        Instr::TXA => {
                            builder.accumulator = builder.x;
                            builder.set_nz(builder.x)
                        }
                        Instr::TYA => {
                            builder.accumulator = builder.y;
                            builder.set_nz(builder.y)
                        }
                        Instr::TSX => {
                            builder.x = builder.stack_ptr;
                            builder.set_nz(builder.x)
                        }
                        Instr::TXS => {
                            builder.stack_ptr = builder.x;
                            Ok(())
                        }
                        Instr::PHA => builder.push(builder.accumulator),
                        Instr::PHP => {
                            let flags = builder.flags_to_int(false).map_err(RunError::Backend)?;
                            builder.push(flags)
                        }
                        Instr::PLA => {
                            builder.accumulator = builder.pop().map_err(RunError::Backend)?;
                            builder.set_nz(builder.accumulator)
                        }
                        Instr::PLP => {
                            let flags = builder.pop().map_err(RunError::Backend)?;
                            builder.int_to_flags(flags)
                        }
                        Instr::AND(_) => todo!(),
                        Instr::EOR(_) => todo!(),
                        Instr::ORA(_) => todo!(),
                        Instr::BIT(_) => todo!(),
                        Instr::ADC(op) => builder.adc(op),
                        Instr::SBC(_) => todo!(),
                        Instr::CMP(_) => todo!(),
                        Instr::CPX(_) => todo!(),
                        Instr::CPY(_) => todo!(),
                        Instr::INC(_) => todo!(),
                        Instr::INX => {
                            builder.x = builder
                                .builder
                                .build_int_add(
                                    builder.x,
                                    builder.cx.i8_type().const_int(1, false),
                                    "",
                                )
                                .map_err(RunError::Backend)?;
                            builder.set_nz(builder.x)
                        }
                        Instr::INY => {
                            builder.y = builder
                                .builder
                                .build_int_add(
                                    builder.y,
                                    builder.cx.i8_type().const_int(1, false),
                                    "",
                                )
                                .map_err(RunError::Backend)?;
                            builder.set_nz(builder.y)
                        }
                        Instr::DEC(_) => todo!(),
                        Instr::DEX => {
                            builder.y = builder
                                .builder
                                .build_int_sub(
                                    builder.y,
                                    builder.cx.i8_type().const_int(1, false),
                                    "",
                                )
                                .map_err(RunError::Backend)?;
                            builder.set_nz(builder.y)
                        }
                        Instr::DEY => {
                            builder.y = builder
                                .builder
                                .build_int_sub(
                                    builder.y,
                                    builder.cx.i8_type().const_int(1, false),
                                    "",
                                )
                                .map_err(RunError::Backend)?;
                            builder.set_nz(builder.y)
                        }
                        Instr::ASL(_) => todo!(),
                        Instr::LSR(_) => todo!(),
                        Instr::ROL(_) => todo!(),
                        Instr::ROR(_) => todo!(),
                        Instr::JMP(addr) => {
                            break builder.cx.i16_type().const_int(addr as u64, false)
                        }
                        Instr::JMPIndirect(addr) => {
                            break builder
                                .read_u16(builder.cx.i16_type().const_int(addr as u64, false))
                                .map_err(RunError::Backend)?
                        }
                        Instr::JSR(addr) => {
                            builder
                                .push_u16(
                                    builder
                                        .cx
                                        .i16_type()
                                        .const_int(pc.wrapping_sub(1) as u64, false),
                                )
                                .map_err(RunError::Backend)?;
                            break builder.cx.i16_type().const_int(addr as u64, false);
                        }
                        Instr::RTS => todo!(),
                        Instr::BCC(_) => todo!(),
                        Instr::BCS(_) => todo!(),
                        Instr::BEQ(_) => todo!(),
                        Instr::BMI(_) => todo!(),
                        Instr::BNE(_) => todo!(),
                        Instr::BPL(_) => todo!(),
                        Instr::BVC(_) => todo!(),
                        Instr::BVS(_) => todo!(),
                        Instr::CLC => todo!(),
                        Instr::CLD => todo!(),
                        Instr::CLI => todo!(),
                        Instr::CLV => todo!(),
                        Instr::SEC => todo!(),
                        Instr::SED => todo!(),
                        Instr::SEI => todo!(),
                        Instr::BRK => break builder.brk(pc).map_err(RunError::Backend)?,
                        Instr::NOP => Ok(()),
                        Instr::RTI => todo!(),
                        Instr::ASL(Operand::Immediate(_))
                        | Instr::LSR(Operand::Immediate(_))
                        | Instr::ROL(Operand::Immediate(_))
                        | Instr::ROR(Operand::Immediate(_)) => unreachable!(),
                    }
                    .map_err(RunError::Backend)?;
                };

                builder.ret(next_pc).map_err(RunError::Backend)?;
                let fn_value = builder.fn_value;
                optimize_fn(fn_value, &this.module);
                #[cfg(debug_assertions)]
                fn_value.print_to_stderr();

                fn_value
            }
        };

        // Add tick mapping
        let mut fns = ExecFns::new(&mut cpu.memory, tick);
        let user_data = fns.setup(&this.module, &this.ee);

        unsafe {
            let f = this
                .ee
                .get_function::<unsafe extern "C" fn(
                    *mut u8,
                    *mut u8,
                    *mut u8,
                    *mut u8,
                    *mut Flags,
                    UserData,
                ) -> u16>(&input_pc.to_string())
                .unwrap();

            let res = f.call(
                &mut cpu.accumulator,
                &mut cpu.x,
                &mut cpu.y,
                &mut cpu.stack_ptr,
                &mut cpu.flags,
                user_data,
            );

            if let Some(e) = fns.take_last_error() {
                return Err(RunError::Memory(e));
            } else {
                return Ok(res);
            }
        }
    }
}

impl<'a> Backend for Llvm<'a> {
    type Error = BuilderError;

    fn run<M: crate::cpu::memory::Memory>(
        cpu: &mut crate::cpu::Cpu<M, Self>,
        pc: u16,
        tick: impl FnMut(u8),
    ) -> Result<(), RunError<M, Self>> {
        let res = Self::_run(cpu, pc, tick)?;
        println!("{res}");
        return Ok(());
    }
}

pub struct Builder<'a, 'b> {
    // Cpu
    accumulator: IntValue<'a>,
    x: IntValue<'a>,
    y: IntValue<'a>,
    stack_ptr: IntValue<'a>,
    flags: VectorValue<'a>,
    user_data: StructValue<'a>,
    // Misc
    fn_value: FunctionValue<'a>,
    common_return: CommonReturn<'a>,
    builder: inkwell::builder::Builder<'a>,
    block: BasicBlock<'a>,
    module: &'b Module<'a>,
    decimal_enabled: bool,
    cx: &'a Context,
}

impl<'a, 'b> Builder<'a, 'b> {
    pub fn new(
        pc: u16,
        decimal_enabled: bool,
        module: &'b Module<'a>,
        cx: &'a Context,
    ) -> Result<Self, BuilderError> {
        let i8_ptr = cx.i8_type().ptr_type(AddressSpace::default());
        let user_data = module.get_struct_type("UserData").unwrap();

        let fn_type = cx.i16_type().fn_type(
            &[
                i8_ptr.into(),
                i8_ptr.into(),
                i8_ptr.into(),
                i8_ptr.into(),
                i8_ptr.into(),
                user_data.into(),
            ],
            false,
        );
        let fn_value = module.add_function(&pc.to_string(), fn_type, None);

        let builder = cx.create_builder();
        let block = cx.append_basic_block(fn_value, "entry");
        builder.position_at_end(block);

        let input_accumulator = fn_value.get_nth_param(0).unwrap().into_pointer_value();
        let input_x = fn_value.get_nth_param(1).unwrap().into_pointer_value();
        let input_y = fn_value.get_nth_param(2).unwrap().into_pointer_value();
        let input_stack_ptr = fn_value.get_nth_param(3).unwrap().into_pointer_value();
        let input_flags = fn_value.get_nth_param(4).unwrap().into_pointer_value();
        let user_data = fn_value.get_nth_param(5).unwrap().into_struct_value();
        let common_return = CommonReturn::new(
            input_accumulator,
            input_x,
            input_y,
            input_stack_ptr,
            input_flags,
            fn_value,
            cx,
        )?;

        return Ok(Self {
            accumulator: builder
                .build_load(input_accumulator, "accumulator")?
                .into_int_value(),
            x: builder.build_load(input_x, "x")?.into_int_value(),
            y: builder.build_load(input_y, "y")?.into_int_value(),
            stack_ptr: builder
                .build_load(input_stack_ptr, "stack_ptr")?
                .into_int_value(),
            flags: builder
                .build_bitcast(
                    builder.build_load(input_flags, "")?.into_int_value(),
                    cx.bool_type().vec_type(8),
                    "flags",
                )?
                .into_vector_value(),
            decimal_enabled,
            user_data,
            common_return,
            block,
            builder,
            fn_value,
            module,
            cx,
        });
    }

    pub fn get_operand(
        &mut self,
        op: Operand,
    ) -> Result<(IntValue<'a>, IntValue<'a>), BuilderError> {
        Ok(match op {
            Operand::Accumulator => (self.accumulator, self.cx.bool_type().const_zero()),
            Operand::Immediate(imm) => (
                self.cx.i8_type().const_int(imm as u64, false),
                self.cx.bool_type().const_zero(),
            ),
            Operand::Addressing(addr) => {
                let (addr, page_crossed) = self.get_address(addr)?;
                (self.read_u8(addr)?, page_crossed)
            }
        })
    }

    pub fn get_address(
        &mut self,
        addr: Addressing,
    ) -> Result<(IntValue<'a>, IntValue<'a>), BuilderError> {
        let bool_type = self.cx.bool_type();
        let i8_type = self.cx.i8_type();
        let i16_type = self.cx.i16_type();

        return Ok(match addr {
            Addressing::ZeroPage(addr) => (
                i16_type.const_int(addr as u64, false),
                bool_type.const_zero(),
            ),
            Addressing::ZeroPageX(base) => {
                let ptr = self.builder.build_int_add(
                    i8_type.const_int(base as u64, false),
                    self.x,
                    "",
                )?;

                (
                    self.builder.build_int_z_extend(ptr, i16_type, "")?,
                    bool_type.const_zero(),
                )
            }
            Addressing::ZeroPageY(base) => {
                let ptr = self.builder.build_int_add(
                    i8_type.const_int(base as u64, false),
                    self.y,
                    "",
                )?;

                (
                    self.builder.build_int_z_extend(ptr, i16_type, "")?,
                    bool_type.const_zero(),
                )
            }
            Addressing::Absolute(addr) => (
                i16_type.const_int(addr as u64, false),
                bool_type.const_zero(),
            ),
            Addressing::AbsoluteX(base) => {
                let base = i16_type.const_int(base as u64, false);
                let offset = self.builder.build_int_z_extend(self.x, i16_type, "")?;
                let addr = self.builder.build_int_add(base, offset, "")?;
                (addr, self.page_crossed(base, addr)?)
            }
            Addressing::AbsoluteY(base) => {
                let base = i16_type.const_int(base as u64, false);
                let offset = self.builder.build_int_z_extend(self.y, i16_type, "")?;
                let addr = self.builder.build_int_add(base, offset, "")?;
                (addr, self.page_crossed(base, addr)?)
            }
            Addressing::IndexedIndirect(base) => {
                let addr = self.builder.build_int_z_extend(
                    self.builder.build_int_add(
                        i8_type.const_int(base as u64, false),
                        self.x,
                        "",
                    )?,
                    i16_type,
                    "",
                )?;
                (self.read_u16(addr)?, bool_type.const_zero())
            }
            Addressing::IndirectIndexed(base) => {
                let base = self.read_u16(i16_type.const_int(base as u64, false))?;
                let addr = self.builder.build_int_add(
                    base,
                    self.builder.build_int_z_extend(self.y, i16_type, "")?,
                    "",
                )?;
                (addr, self.page_crossed(base, addr)?)
            }
        });
    }

    pub fn page_crossed(
        &self,
        addr1: IntValue<'a>,
        addr2: IntValue<'a>,
    ) -> Result<IntValue<'a>, BuilderError> {
        let xor = self.builder.build_xor(addr2, addr1, "")?;
        return self.builder.build_int_compare(
            IntPredicate::UGT,
            xor,
            self.cx.i16_type().const_int(0xff, false),
            "",
        );
    }
}

// Instrs
impl<'a, 'b> Builder<'a, 'b> {
    pub fn lda(&mut self, op: Operand) -> Result<(), BuilderError> {
        let (op, page_crossed) = self.get_operand(op)?;
        self.accumulator = op;
        self.set_nz(op)?;
        self.tick_if_page_crossed(self.cx.i8_type().const_int(1, false), page_crossed)?;
        return Ok(());
    }

    pub fn ldx(&mut self, op: Operand) -> Result<(), BuilderError> {
        let (op, page_crossed) = self.get_operand(op)?;
        self.x = op;
        self.set_nz(op)?;
        self.tick_if_page_crossed(self.cx.i8_type().const_int(1, false), page_crossed)?;
        return Ok(());
    }

    pub fn ldy(&mut self, op: Operand) -> Result<(), BuilderError> {
        let (op, page_crossed) = self.get_operand(op)?;
        self.y = op;
        self.set_nz(op)?;
        self.tick_if_page_crossed(self.cx.i8_type().const_int(1, false), page_crossed)?;
        return Ok(());
    }

    pub fn adc(&mut self, op: Operand) -> Result<(), BuilderError> {
        let (op, page_crossed) = self.get_operand(op)?;
        let carry = self.get_flag(Flag::Carry)?;
        let decimal = self.builder.build_and(
            self.get_flag(Flag::Decimal)?,
            self.cx
                .bool_type()
                // .const_int(self.decimal_enabled as u64, false),
                .const_int(true as u64, false),
            "",
        )?;

        let binary_block = self.cx.append_basic_block(self.fn_value, "");
        let decimal_block = self.cx.append_basic_block(self.fn_value, "");

        self.builder
            .build_conditional_branch(decimal, decimal_block, binary_block)?;

        // Build continue block
        let continue_block = self.cx.append_basic_block(self.fn_value, "");
        self.builder.position_at_end(continue_block);
        let accumulator = self.builder.build_phi(self.cx.i8_type(), "")?;
        let next_carry = self.builder.build_phi(self.cx.bool_type(), "")?;
        let next_overflow = self.builder.build_phi(self.cx.bool_type(), "")?;

        // Build decimal block
        self.builder.position_at_end(decimal_block);
        let decimal_res = self
            .builder
            .build_call(
                self.module.get_function("decimal_adc").unwrap(),
                &[self.accumulator.into(), op.into(), carry.into()],
                "",
            )?
            .as_any_value_enum()
            .into_struct_value();

        accumulator.add_incoming(&[(
            &self.builder.build_extract_value(decimal_res, 0, "")?,
            decimal_block,
        )]);

        next_carry.add_incoming(&[(
            &self.builder.build_extract_value(decimal_res, 1, "")?,
            decimal_block,
        )]);

        next_overflow.add_incoming(&[(&self.get_flag(Flag::Overflow)?, decimal_block)]);
        self.builder.build_unconditional_branch(continue_block)?;

        // Build binary block
        self.builder.position_at_end(binary_block);
        let binary_res = self
            .builder
            .build_call(
                self.module.get_function("binary_adc").unwrap(),
                &[self.accumulator.into(), op.into(), carry.into()],
                "",
            )?
            .as_any_value_enum()
            .into_struct_value();

        accumulator.add_incoming(&[(
            &self.builder.build_extract_value(binary_res, 0, "")?,
            binary_block,
        )]);

        next_carry.add_incoming(&[(
            &self.builder.build_extract_value(binary_res, 1, "")?,
            binary_block,
        )]);

        next_overflow.add_incoming(&[(
            &self.builder.build_extract_value(binary_res, 2, "")?,
            binary_block,
        )]);

        self.builder.build_unconditional_branch(continue_block)?;

        // Return
        self.block = continue_block;
        self.builder.position_at_end(continue_block);

        self.accumulator = accumulator.as_basic_value().into_int_value();
        self.set_flag(Flag::Carry, next_carry.as_basic_value().into_int_value())?;
        self.set_flag(
            Flag::Overflow,
            next_overflow.as_basic_value().into_int_value(),
        )?;

        self.tick_if_page_crossed(self.cx.i8_type().const_int(1, false), page_crossed)?;
        return Ok(());
    }

    pub fn brk(&mut self, pc: u16) -> Result<IntValue<'a>, BuilderError> {
        self.push_u16(
            self.cx
                .i16_type()
                .const_int(pc.wrapping_add(1) as u64, false),
        )?;
        self.push(self.flags_to_int(false)?)?;
        self.set_flag(
            Flag::InterruptDisable,
            self.cx.bool_type().const_int(1, false),
        )?;

        return self.read_u16(self.cx.i16_type().const_int(0xfffe, false));
    }

    pub fn ret(&mut self, pc: IntValue<'a>) -> Result<(), BuilderError> {
        self.common_return.add_incoming(pc, self)?;
        self.builder
            .build_unconditional_branch(self.common_return.block)?;
        return Ok(());
    }
}

// Memory
impl<'a, 'b> Builder<'a, 'b> {
    pub fn tick_if_page_crossed(
        &mut self,
        ticks: IntValue<'a>,
        page_crossed: IntValue<'a>,
    ) -> Result<(), BuilderError> {
        let then_block = self.cx.append_basic_block(self.fn_value, "");
        let continue_block = self.cx.append_basic_block(self.fn_value, "");
        self.builder
            .build_conditional_branch(page_crossed, then_block, continue_block)?;

        self.block = then_block;
        self.builder.position_at_end(self.block);
        self.tick(ticks)?;
        self.builder.build_unconditional_branch(continue_block)?;

        self.block = continue_block;
        self.builder.position_at_end(self.block);
        return Ok(());
    }

    pub fn tick(&self, ticks: IntValue<'a>) -> Result<(), BuilderError> {
        let fn_value = self.module.get_function("_tick").unwrap();
        let user_data = self.builder.build_extract_value(self.user_data, 0, "")?;

        self.builder
            .build_call(fn_value, &[ticks.into(), user_data.into()], "")?;
        return Ok(());
    }

    pub fn read_u8(&mut self, addr: IntValue<'a>) -> Result<IntValue<'a>, BuilderError> {
        let fn_value = self.module.get_function("_read_u8").unwrap();
        let user_data = self.builder.build_extract_value(self.user_data, 1, "")?;

        let res = self
            .builder
            .build_call(fn_value, &[addr.into(), user_data.into()], "")?
            .as_any_value_enum()
            .into_int_value();

        let is_neg = self.builder.build_int_compare(
            IntPredicate::SLT,
            res,
            self.cx.i16_type().const_zero(),
            "",
        )?;

        let continue_block = self.cx.append_basic_block(self.fn_value, "");
        self.common_return
            .add_incoming(self.cx.i16_type().get_poison(), self)?;

        self.builder
            .build_conditional_branch(is_neg, self.common_return.block, continue_block)?;

        self.block = continue_block;
        self.builder.position_at_end(continue_block);

        return self.builder.build_int_truncate(res, self.cx.i8_type(), "");
    }

    pub fn read_u16(&mut self, addr: IntValue<'a>) -> Result<IntValue<'a>, BuilderError> {
        let fn_value = self.module.get_function("_read_u16").unwrap();
        let user_data = self.builder.build_extract_value(self.user_data, 2, "")?;

        let res = self
            .builder
            .build_call(fn_value, &[addr.into(), user_data.into()], "")?
            .as_any_value_enum()
            .into_int_value();

        let is_neg = self.builder.build_int_compare(
            IntPredicate::SLT,
            res,
            self.cx.i32_type().const_zero(),
            "",
        )?;

        let continue_block = self.cx.append_basic_block(self.fn_value, "");
        self.common_return
            .add_incoming(self.cx.i16_type().get_poison(), self)?;

        self.builder
            .build_conditional_branch(is_neg, self.common_return.block, continue_block)?;

        self.block = continue_block;
        self.builder.position_at_end(continue_block);

        return self.builder.build_int_truncate(res, self.cx.i16_type(), "");
    }

    pub fn write_u8(
        &mut self,
        addr: IntValue<'a>,
        value: IntValue<'a>,
    ) -> Result<(), BuilderError> {
        let fn_value = self.module.get_function("_write_u8").unwrap();
        let user_data = self.builder.build_extract_value(self.user_data, 3, "")?;

        let res = self
            .builder
            .build_call(fn_value, &[addr.into(), value.into(), user_data.into()], "")?
            .as_any_value_enum()
            .into_int_value();

        let is_neg = self.builder.build_int_compare(
            IntPredicate::SLT,
            res,
            self.cx.i8_type().const_zero(),
            "",
        )?;

        let continue_block = self.cx.append_basic_block(self.fn_value, "");
        self.common_return
            .add_incoming(self.cx.i16_type().get_poison(), self)?;

        self.builder
            .build_conditional_branch(is_neg, self.common_return.block, continue_block)?;

        self.block = continue_block;
        self.builder.position_at_end(continue_block);

        return Ok(());
    }

    pub fn write_u16(
        &mut self,
        addr: IntValue<'a>,
        value: IntValue<'a>,
    ) -> Result<(), BuilderError> {
        let fn_value = self.module.get_function("_write_u16").unwrap();
        let user_data = self.builder.build_extract_value(self.user_data, 4, "")?;

        let res = self
            .builder
            .build_call(fn_value, &[addr.into(), value.into(), user_data.into()], "")?
            .as_any_value_enum()
            .into_int_value();

        let is_neg = self.builder.build_int_compare(
            IntPredicate::SLT,
            res,
            self.cx.i8_type().const_zero(),
            "",
        )?;

        let continue_block = self.cx.append_basic_block(self.fn_value, "");
        self.common_return
            .add_incoming(self.cx.i16_type().get_poison(), self)?;

        self.builder
            .build_conditional_branch(is_neg, self.common_return.block, continue_block)?;

        self.block = continue_block;
        self.builder.position_at_end(continue_block);

        return Ok(());
    }
}

// Stack
impl<'a, 'b> Builder<'a, 'b> {
    pub fn push(&mut self, value: IntValue<'a>) -> Result<(), BuilderError> {
        let addr = self.stack_addr()?;
        self.write_u8(addr, value)?;

        self.stack_ptr = self.builder.build_int_sub(
            self.stack_ptr,
            self.cx.i8_type().const_int(1, false),
            "",
        )?;
        return Ok(());
    }

    pub fn push_u16(&mut self, value: IntValue<'a>) -> Result<(), BuilderError> {
        let [lo, hi] = self.to_le_bytes(value)?;
        self.push(hi)?;
        self.push(lo)?;
        return Ok(());
    }

    pub fn pop(&mut self) -> Result<IntValue<'a>, BuilderError> {
        self.stack_ptr = self.builder.build_int_add(
            self.stack_ptr,
            self.cx.i8_type().const_int(1, false),
            "",
        )?;

        let addr = self.stack_addr()?;
        return self.read_u8(addr);
    }

    pub fn pop_u16(&mut self) -> Result<IntValue<'a>, BuilderError> {
        let lo = self.pop()?;
        let hi = self.pop()?;
        return self.from_le_bytes(lo, hi);
    }

    pub fn stack_addr(&self) -> Result<IntValue<'a>, BuilderError> {
        return self.builder.build_int_add(
            self.cx.i16_type().const_int(0x100, false),
            self.builder
                .build_int_z_extend(self.stack_ptr, self.cx.i16_type(), "")?,
            "",
        );
    }
}

// Flags
impl<'a, 'b> Builder<'a, 'b> {
    pub fn flags_to_int(&self, from_interrupt: bool) -> Result<IntValue<'a>, BuilderError> {
        let bool_type = self.cx.bool_type();
        let i8_type = self.cx.i8_type();

        let always_set = self.builder.build_insert_element(
            self.flags,
            bool_type.const_int(1, false),
            i8_type.const_int(5, false),
            "",
        )?;

        let break_ = self.builder.build_insert_element(
            always_set,
            bool_type.const_int((!from_interrupt) as u64, false),
            i8_type.const_int(4, false),
            "",
        )?;

        return Ok(self
            .builder
            .build_bitcast(break_, i8_type, "")?
            .into_int_value());
    }

    pub fn int_to_flags(&mut self, value: IntValue<'a>) -> Result<(), BuilderError> {
        let bool_type = self.cx.bool_type();
        let i8_type = self.cx.i8_type();

        self.flags = self
            .builder
            .build_bitcast(value, bool_type.vec_type(8), "")?
            .into_vector_value();

        self.flags = self.builder.build_insert_element(
            self.flags,
            bool_type.const_int(0, false),
            i8_type.const_int(4, false),
            "",
        )?;

        self.flags = self.builder.build_insert_element(
            self.flags,
            bool_type.const_int(1, false),
            i8_type.const_int(5, false),
            "",
        )?;

        return Ok(());
    }

    pub fn set_nz(&mut self, val: IntValue<'a>) -> Result<(), BuilderError> {
        let zero = self.cx.i8_type().const_zero();

        self.set_flag(
            Flag::Zero,
            self.builder
                .build_int_compare(IntPredicate::EQ, val, zero, "")?,
        )?;

        self.set_flag(
            Flag::Negative,
            self.builder
                .build_int_compare(IntPredicate::SLT, val, zero, "")?,
        )?;

        return Ok(());
    }

    pub fn set_flag(&mut self, flag: Flag, val: IntValue<'a>) -> Result<(), BuilderError> {
        self.flags = self.builder.build_insert_element(
            self.flags,
            val,
            self.cx.i8_type().const_int(flag as u64, false),
            "",
        )?;
        return Ok(());
    }

    pub fn get_flag(&self, flag: Flag) -> Result<IntValue<'a>, BuilderError> {
        return Ok(self
            .builder
            .build_extract_element(
                self.flags,
                self.cx.i8_type().const_int(flag as u64, false),
                "",
            )?
            .into_int_value());
    }
}

// Misc
impl<'a, 'b> Builder<'a, 'b> {
    pub fn to_le_bytes(&self, value: IntValue<'a>) -> Result<[IntValue<'a>; 2], BuilderError> {
        let vector = self
            .builder
            .build_bitcast(value, self.cx.i8_type().vec_type(2), "")?
            .into_vector_value();

        return Ok(match cfg!(target_endian = "big") {
            true => [
                self.builder
                    .build_extract_element(vector, self.cx.i8_type().const_int(1, false), "")?
                    .into_int_value(),
                self.builder
                    .build_extract_element(vector, self.cx.i8_type().const_int(0, false), "")?
                    .into_int_value(),
            ],

            false => [
                self.builder
                    .build_extract_element(vector, self.cx.i8_type().const_int(0, false), "")?
                    .into_int_value(),
                self.builder
                    .build_extract_element(vector, self.cx.i8_type().const_int(1, false), "")?
                    .into_int_value(),
            ],
        });
    }

    pub fn from_le_bytes(
        &self,
        lo: IntValue<'a>,
        hi: IntValue<'a>,
    ) -> Result<IntValue<'a>, BuilderError> {
        let lo = self.builder.build_insert_element(
            self.cx.i8_type().vec_type(2).get_undef(),
            lo,
            self.cx
                .i8_type()
                .const_int(if cfg!(target_endian = "big") { 1 } else { 0 }, false),
            "",
        )?;

        let hi = self.builder.build_insert_element(
            lo,
            hi,
            self.cx
                .i8_type()
                .const_int(if cfg!(target_endian = "big") { 0 } else { 1 }, false),
            "",
        )?;

        return Ok(self
            .builder
            .build_bitcast(hi, self.cx.i16_type(), "")?
            .into_int_value());
    }
}

#[cfg(test)]
mod tests {
    use super::Llvm;
    use inkwell::context::Context;

    #[test]
    fn init() {
        let cx = Context::create();
        let llvm = Llvm::new(&cx).unwrap();
        llvm.print_to_stderr();
    }
}

#[repr(C)]
struct UserData {
    pub tick: *mut c_void,
    pub read_u8: *mut c_void,
    pub read_u16: *mut c_void,
    pub write_u8: *mut c_void,
    pub write_u16: *mut c_void,
}

struct ExecInner<'a, M: Memory> {
    last_error: Option<M::Error>,
    memory: &'a mut M,
}

struct ExecFns<'a, M: Memory> {
    pub tick: Closure<dyn 'a + FnMut(u8)>,
    pub read_u8: unsafe extern "C" fn(u16, *mut c_void) -> i16,
    pub read_u16: unsafe extern "C" fn(u16, *mut c_void) -> i32,
    pub write_u8: unsafe extern "C" fn(u16, u8, *mut c_void) -> i8,
    pub write_u16: unsafe extern "C" fn(u16, u16, *mut c_void) -> i8,
    pub user_data: Box<ExecInner<'a, M>>,
    _phtm: PhantomData<&'a mut &'a ()>,
}

impl<'a, M: Memory> ExecFns<'a, M> {
    pub fn new(memory: &'a mut M, tick: impl 'a + FnMut(u8)) -> Self {
        unsafe extern "C" fn read_u8<M: Memory>(addr: u16, user_data: *mut c_void) -> i16 {
            let this = &mut *user_data.cast::<ExecInner<M>>();
            return match this.memory.read_u8(addr) {
                Ok(res) => res as i16,
                Err(e) => {
                    this.last_error = Some(e);
                    -1
                }
            };
        }

        unsafe extern "C" fn read_u16<M: Memory>(addr: u16, user_data: *mut c_void) -> i32 {
            let this = &mut *user_data.cast::<ExecInner<M>>();
            return match this.memory.read_u16(addr) {
                Ok(res) => res as i32,
                Err(e) => {
                    this.last_error = Some(e);
                    -1
                }
            };
        }

        unsafe extern "C" fn write_u8<M: Memory>(addr: u16, val: u8, user_data: *mut c_void) -> i8 {
            let this = &mut *user_data.cast::<ExecInner<M>>();
            return match this.memory.write_u8(addr, val) {
                Ok(_) => 0,
                Err(e) => {
                    this.last_error = Some(e);
                    -1
                }
            };
        }

        unsafe extern "C" fn write_u16<M: Memory>(
            addr: u16,
            val: u16,
            user_data: *mut c_void,
        ) -> i8 {
            let this = &mut *user_data.cast::<ExecInner<M>>();
            return match this.memory.write_u16(addr, val) {
                Ok(_) => 0,
                Err(e) => {
                    this.last_error = Some(e);
                    -1
                }
            };
        }

        return Self {
            tick: Closure::new(tick),
            read_u8: read_u8::<M>,
            read_u16: read_u16::<M>,
            write_u8: write_u8::<M>,
            write_u16: write_u16::<M>,
            user_data: Box::new(ExecInner {
                last_error: None,
                memory,
            }),
            _phtm: PhantomData,
        };
    }

    pub fn take_last_error(&mut self) -> Option<M::Error> {
        return self.user_data.last_error.take();
    }

    pub fn setup<'cx>(&self, module: &Module<'cx>, ee: &ExecutionEngine<'cx>) -> UserData {
        ee.add_global_mapping(
            &module.get_function("_tick").unwrap(),
            self.tick.fn_ptr() as usize,
        );

        ee.add_global_mapping(
            &module.get_function("_read_u8").unwrap(),
            self.read_u8 as usize,
        );

        ee.add_global_mapping(
            &module.get_function("_read_u16").unwrap(),
            self.read_u16 as usize,
        );

        ee.add_global_mapping(
            &module.get_function("_write_u8").unwrap(),
            self.write_u8 as usize,
        );

        ee.add_global_mapping(
            &module.get_function("_write_u16").unwrap(),
            self.write_u16 as usize,
        );

        let user_data = std::ptr::addr_of!(*self.user_data) as *mut c_void;
        return UserData {
            tick: self.tick.user_data(),
            read_u8: user_data,
            read_u16: user_data,
            write_u8: user_data,
            write_u16: user_data,
        };
    }
}

struct CommonReturn<'a> {
    block: BasicBlock<'a>,
    accumulator: PhiValue<'a>,
    x: PhiValue<'a>,
    y: PhiValue<'a>,
    stack_ptr: PhiValue<'a>,
    flags: PhiValue<'a>,
    pc: PhiValue<'a>,
}

impl<'a> CommonReturn<'a> {
    pub fn new(
        input_accumulator: PointerValue<'a>,
        input_x: PointerValue<'a>,
        input_y: PointerValue<'a>,
        input_stack_ptr: PointerValue<'a>,
        input_flags: PointerValue<'a>,
        fn_value: FunctionValue<'a>,
        cx: &'a Context,
    ) -> Result<Self, BuilderError> {
        let builder = cx.create_builder();
        let block = cx.append_basic_block(fn_value, "common.ret");
        builder.position_at_end(block);

        let accumulator = builder.build_phi(cx.i8_type(), "output_acc")?;
        let x = builder.build_phi(cx.i8_type(), "output_x")?;
        let y = builder.build_phi(cx.i8_type(), "output_y")?;
        let stack_ptr = builder.build_phi(cx.i8_type(), "output_stack_ptr")?;
        let flags = builder.build_phi(cx.i8_type(), "output_flags")?;
        let pc = builder.build_phi(cx.i16_type(), "output_pc")?;

        // Store cpu state
        builder.build_store(
            input_accumulator,
            accumulator.as_basic_value().into_int_value(),
        )?;
        builder.build_store(input_x, x.as_basic_value().into_int_value())?;
        builder.build_store(input_y, y.as_basic_value().into_int_value())?;
        builder.build_store(input_stack_ptr, stack_ptr.as_basic_value().into_int_value())?;
        builder.build_store(input_flags, flags.as_basic_value().into_int_value())?;
        builder.build_return(Some(&pc.as_basic_value()))?;

        return Ok(Self {
            block,
            pc,
            accumulator,
            x,
            y,
            stack_ptr,
            flags,
        });
    }

    pub fn add_incoming(
        &self,
        next_pc: IntValue<'a>,
        b: &Builder<'a, '_>,
    ) -> Result<(), BuilderError> {
        let flags = b.builder.build_bitcast(b.flags, b.cx.i8_type(), "")?;

        self.accumulator.add_incoming(&[(&b.accumulator, b.block)]);
        self.x.add_incoming(&[(&b.x, b.block)]);
        self.y.add_incoming(&[(&b.y, b.block)]);
        self.stack_ptr.add_incoming(&[(&b.stack_ptr, b.block)]);
        self.flags.add_incoming(&[(&flags, b.block)]);
        self.pc.add_incoming(&[(&next_pc, b.block)]);
        return Ok(());
    }
}

fn optimize_fn<'a>(fn_value: FunctionValue<'a>, module: &Module<'a>) {
    let manager = PassManager::<FunctionValue>::create(module);
    manager.add_instruction_simplify_pass();
    manager.add_instruction_combining_pass();
    manager.add_ind_var_simplify_pass();
    manager.add_cfg_simplification_pass();
    manager.add_aggressive_dce_pass();
    manager.add_cfg_simplification_pass();
    manager.add_bit_tracking_dce_pass();
    manager.add_correlated_value_propagation_pass();
    manager.add_instruction_simplify_pass();
    manager.add_instruction_combining_pass();
    manager.add_new_gvn_pass();
    manager.add_aggressive_dce_pass();
    // builder.populate_function_pass_manager(&manager);
    manager.run_on(&fn_value);
}
