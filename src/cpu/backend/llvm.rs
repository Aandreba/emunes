use super::Backend;
use crate::cpu::{
    flags::Flag,
    instrs::{read_instruction, Addressing, Instr, Operand},
    memory::Memory,
    Cpu, RunError,
};
use bitvec::prelude::*;
use ffi_closure::Closure;
use inkwell::{
    basic_block::BasicBlock,
    builder::BuilderError,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    memory_buffer::MemoryBuffer,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    types::StructType,
    values::{
        AnyValue, BasicValue, CallableValue, FunctionValue, IntValue, PointerValue, StructValue,
        VectorValue,
    },
    AddressSpace, IntPredicate, OptimizationLevel,
};
use std::{
    cell::{Cell, UnsafeCell},
    collections::{hash_map::Entry, HashMap},
    ffi::c_void,
    mem::offset_of,
    ops::{Deref, Range},
    sync::{Arc, Condvar, Mutex, PoisonError},
    time::Instant,
};
use utils_atomics::{
    channel::once::{channel, Receiver},
    AtomicCell,
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
    *mut u8,                               // prev cycles
) -> u16;

type CodeRegionArray = BitArr!(for 0x10000);

const MAX_CACHE_SIZE: usize = 1024;
const SKELETON: &str = include_str!("../../../skeleton.ll");
const FUNTION_NAME: &str = "main";
const ACC_OFFSET: usize = offset_of!(crate::cpu::State, accumulator);
const X_OFFSET: usize = offset_of!(crate::cpu::State, x);
const Y_OFFSET: usize = offset_of!(crate::cpu::State, y);
const STACK_PTR_OFFSET: usize = offset_of!(crate::cpu::State, stack_ptr);
const FLAGS_OFFSET: usize = offset_of!(crate::cpu::State, flags);
const DECIMAL_ENABLED_OFFSET: usize = offset_of!(crate::cpu::State, decimal_enabled);
const NMI_INTERRUPT_OFFSET: usize = offset_of!(crate::cpu::State, nmi_interrupt);

pub struct Llvm<'cx> {
    compiled: HashMap<u16, Compiled<'cx>>,
    oldest_code: Option<u16>,
    code_regions: CodeRegionArray,
    cx: Context,
}

impl<'cx> Llvm<'cx> {
    pub fn new() -> Self {
        return Self {
            cx: Context::create(),
            code_regions: CodeRegionArray::ZERO,
            compiled: HashMap::new(),
            oldest_code: None,
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
            let initial_pc = pc;

            let state = std::ptr::addr_of_mut!((*cpu.get()).state);
            let memory = std::ptr::addr_of_mut!((*cpu.get()).memory);
            let backend = &mut (&mut *cpu.get()).backend;

            let mut prev_cycles = 0;
            loop {
                log::trace!("Next PC: 0x{pc:X}\n{:?}", &*state);
                let cache_size = backend.compiled.len();

                let (next_pc, clear_code_cache) = match backend.compiled.entry(pc) {
                    Entry::Occupied(entry) => entry.into_mut(),
                    Entry::Vacant(entry) => {
                        let mut builder =
                            Builder::new(pc, &backend.cx).map_err(RunError::Backend)?;

                        let mut prev_cycles = builder
                            .build_load(builder.prev_cycles, "")
                            .map_err(RunError::Backend)?
                            .into_int_value();

                        loop {
                            let prev_pc = builder.pc;
                            builder.build_tick(prev_cycles).map_err(RunError::Backend)?;
                            builder.build_handle_nmi().map_err(RunError::Backend)?;

                            let Some(instr) = read_instruction(&mut *memory, &mut builder.pc)
                                .map_err(RunError::Memory)?
                            else {
                                panic!(
                                    "unknown instruction found at 0x{prev_pc:04X}: 0x{:02X}",
                                    (&mut *memory).read_u8(prev_pc).unwrap()
                                )
                            };

                            prev_cycles =
                                builder.cx.i8_type().const_int(instr.cycles() as u64, false);
                            log::trace!("{prev_pc:04X}: {instr:04X?}");

                            let is_terminating =
                                builder.translate_instr(instr).map_err(RunError::Backend)?;

                            if is_terminating {
                                break;
                            }
                        }

                        backend.code_regions[builder.start_pc as usize..builder.end_pc as usize]
                            .fill(true);
                        // builder.fn_value.print_to_stderr();
                        // optimize_module(&builder.module);
                        entry.insert(Compiled::new(
                            builder.module,
                            builder.start_pc..builder.end_pc,
                        ))
                    }
                }
                .call(
                    state,
                    memory,
                    &tick,
                    &backend.code_regions,
                    &mut prev_cycles,
                )
                .map_err(RunError::Memory)?;

                // Remove modified code (if any) from cache
                if !clear_code_cache.is_empty() {
                    log::warn!("Found self-modifying code, clearing modified code");
                    for addr in clear_code_cache {
                        let mut regions = Vec::with_capacity(backend.compiled.len());

                        for (&key, value) in backend.compiled.iter() {
                            if value.range.contains(&addr) {
                                regions.push(key)
                            }
                        }

                        for addr in regions {
                            backend.compiled.remove(&addr);
                        }

                        backend.code_regions.set(addr as usize, false);
                    }
                }

                // If cache is too big, remove oldest code
                if cache_size >= MAX_CACHE_SIZE {
                    if let Some(key) = backend.oldest_code.take() {
                        log::debug!("Removing code cached @ 0x{:4X}", key);
                        let _ = backend.compiled.remove(&key);
                    }
                }

                // Update oldest code (if necessary)
                if !backend.oldest_code.is_some_and(|x| x != initial_pc) {
                    // Find new oldest code
                    backend.oldest_code = backend
                        .compiled
                        .iter()
                        .min_by(|(_, x), (_, y)| x.last_exec.cmp(&y.last_exec))
                        .map(|(&k, _)| k)
                }

                pc = next_pc
            }
        }
    }
}

struct Builder<'cx> {
    entry_pc: u16,
    start_pc: u16,
    end_pc: u16,
    entry_block: BasicBlock<'cx>,
    prev_state: State<'cx>,

    inline_count: u32,
    pc: u16,

    state_ptr: PointerValue<'cx>,
    accumulator_ptr: PointerValue<'cx>,
    x_ptr: PointerValue<'cx>,
    y_ptr: PointerValue<'cx>,
    stack_ptr_ptr: PointerValue<'cx>,
    flags_ptr: PointerValue<'cx>,
    nmi_interrupt_ptr: PointerValue<'cx>,

    accumulator: IntValue<'cx>,
    x: IntValue<'cx>,
    y: IntValue<'cx>,
    stack_ptr: IntValue<'cx>,
    flags: VectorValue<'cx>,
    decimal_enabled: IntValue<'cx>,
    prev_cycles: PointerValue<'cx>,

    memory_data_ptr: PointerValue<'cx>,
    memory_error_ptr: PointerValue<'cx>,
    read_u8: PointerValue<'cx>,
    read_u16: PointerValue<'cx>,
    write_u8: PointerValue<'cx>,
    write_u16: PointerValue<'cx>,

    tick_data_ptr: PointerValue<'cx>,
    tick: PointerValue<'cx>,

    #[cfg(target_endian = "big")]
    bswap: FunctionValue<'cx>,
    adc: FunctionValue<'cx>,
    sbc: FunctionValue<'cx>,
    adc_output_type: StructType<'cx>,

    fn_value: FunctionValue<'cx>,
    builder: inkwell::builder::Builder<'cx>,
    module: Module<'cx>,
    cx: &'cx Context,
}

impl<'a> Builder<'a> {
    pub fn new(pc: u16, cx: &'a Context) -> Result<Self, BuilderError> {
        let module = cx
            .create_module_from_ir(MemoryBuffer::create_from_memory_range_copy(
                SKELETON.trim_end().as_bytes(),
                "skeleton",
            ))
            .unwrap();
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
                i8_ptr_type.into(),           // prev_cycles
            ],
            false,
        );
        let fn_value = module.add_function(FUNTION_NAME, fn_type, None);

        let entry_block = cx.append_basic_block(fn_value, "");
        builder.position_at_end(entry_block);
        let new_entry_block = cx.append_basic_block(fn_value, "entry");
        builder.build_unconditional_branch(new_entry_block)?;
        let entry_block = new_entry_block;
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

            let decimal_enabled_ptr = builder.build_gep(
                state_ptr,
                &[i64_type.const_int(DECIMAL_ENABLED_OFFSET as u64, false)],
                "decimal_enabled_ptr",
            )?;

            let nmi_interrupt_ptr = builder.build_gep(
                state_ptr,
                &[i64_type.const_int(NMI_INTERRUPT_OFFSET as u64, false)],
                "nmi_interrupt_ptr",
            )?;

            let accumulator = builder.build_load(accumulator_ptr, "")?.into_int_value();
            let x = builder.build_load(x_ptr, "")?.into_int_value();
            let y = builder.build_load(y_ptr, "")?.into_int_value();
            let stack_ptr = builder.build_load(stack_ptr_ptr, "")?.into_int_value();
            let flags = builder.build_load(flags_ptr, "")?.into_int_value();
            let decimal_enabled = builder.build_int_compare(
                IntPredicate::NE,
                builder
                    .build_load(decimal_enabled_ptr, "")?
                    .into_int_value(),
                i8_type.const_zero(),
                "",
            )?;

            let mut this = Self {
                inline_count: 32,
                entry_block,
                entry_pc: pc,
                start_pc: pc,
                end_pc: pc,
                pc,
                #[cfg(target_endian = "big")]
                bswap: Intrinsic::find("llvm.bswap")
                    .unwrap()
                    .get_declaration(&module, &[i16_type.into()])
                    .unwrap(),
                adc: module.get_function("adc").unwrap(),
                sbc: module.get_function("sbc").unwrap(),
                adc_output_type: module.get_struct_type("AdcOutput").unwrap(),
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
                nmi_interrupt_ptr,
                accumulator,
                x,
                y,
                stack_ptr,
                decimal_enabled,
                flags: flags_type.get_poison(),
                tick_data_ptr: fn_value.get_nth_param(1).unwrap().into_pointer_value(),
                tick: fn_value.get_nth_param(2).unwrap().into_pointer_value(),
                memory_data_ptr: fn_value.get_nth_param(3).unwrap().into_pointer_value(),
                memory_error_ptr: fn_value.get_nth_param(4).unwrap().into_pointer_value(),
                read_u8: fn_value.get_nth_param(5).unwrap().into_pointer_value(),
                read_u16: fn_value.get_nth_param(6).unwrap().into_pointer_value(),
                write_u8: fn_value.get_nth_param(7).unwrap().into_pointer_value(),
                write_u16: fn_value.get_nth_param(8).unwrap().into_pointer_value(),
                prev_cycles: fn_value.get_nth_param(9).unwrap().into_pointer_value(),
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
            this.prev_cycles.set_name("prev_cycles");

            this.flags_from_u8(flags)?;
            this.prev_state.flags = this.flags;

            return Ok(this);
        }
    }
}

impl<'a> Builder<'a> {
    fn translate_instr(&mut self, instr: Instr) -> Result<bool, BuilderError> {
        let cycles = instr.cycles();
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
                self.build_write_u8(addr, self.accumulator, cycles)?;
            }
            Instr::STX(addr) => {
                let (addr, _) = self.translate_address(addr)?;
                self.build_write_u8(addr, self.x, cycles)?;
            }
            Instr::STY(addr) => {
                let (addr, _) = self.translate_address(addr)?;
                self.build_write_u8(addr, self.y, cycles)?;
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
            Instr::PHA => self.stack_push(self.accumulator, cycles)?,
            Instr::PHP => self.stack_push(self.flags_into_u8(false)?, cycles)?,
            Instr::PLA => {
                self.accumulator = self.stack_pop()?;
                self.set_nz(self.accumulator)?;
            }
            Instr::PLP => {
                let flags = self.stack_pop()?;
                self.flags_from_u8(flags)?;
            }
            Instr::AND(op) => {
                let (op, page_crossed) = self.translate_operand(op)?;
                self.accumulator = self.build_and(self.accumulator, op, "")?;
                self.set_nz(self.accumulator)?;
                self.handle_page_cross(page_crossed, 1)?;
            }
            Instr::EOR(op) => {
                let (op, page_crossed) = self.translate_operand(op)?;
                self.accumulator = self.build_xor(self.accumulator, op, "")?;
                self.set_nz(self.accumulator)?;
                self.handle_page_cross(page_crossed, 1)?;
            }
            Instr::ORA(op) => {
                let (op, page_crossed) = self.translate_operand(op)?;
                self.accumulator = self.build_or(self.accumulator, op, "")?;
                self.set_nz(self.accumulator)?;
                self.handle_page_cross(page_crossed, 1)?;
            }
            Instr::BIT(addr) => {
                let i8_type = self.cx.i8_type();
                let i8_zero = i8_type.const_zero();

                let (addr, _) = self.translate_address(addr)?;
                let op = self.build_read_u8(addr)?;

                let zero = self.build_int_compare(
                    IntPredicate::EQ,
                    self.build_and(self.accumulator, op, "")?,
                    i8_zero,
                    "",
                )?;

                let overflow = self.build_int_truncate(
                    self.build_and(
                        self.build_right_shift(op, i8_type.const_int(6, false), false, "")?,
                        i8_type.const_int(1, false),
                        "",
                    )?,
                    self.cx.bool_type(),
                    "",
                )?;

                let negative = self.build_int_compare(IntPredicate::SLT, op, i8_zero, "")?;

                self.set_flag(Flag::Zero, zero)?;
                self.set_flag(Flag::Negative, negative)?;
                self.set_flag(Flag::Overflow, overflow)?;
            }
            Instr::ADC(op) => {
                let (op, page_crossed) = self.translate_operand(op)?;

                let lhs = self.accumulator;
                let rhs = op;
                let carry = self.get_flag(Flag::Carry)?;
                let decimal =
                    self.build_and(self.get_flag(Flag::Decimal)?, self.decimal_enabled, "")?;

                let output = self.build_alloca(self.adc_output_type, "")?;
                let output_void = self.build_pointer_cast(
                    output,
                    self.cx.i8_type().ptr_type(AddressSpace::default()),
                    "",
                )?;

                self.build_call(
                    self.adc,
                    &[
                        lhs.into(),
                        rhs.into(),
                        carry.into(),
                        decimal.into(),
                        output_void.into(),
                    ],
                    "",
                )?;

                let output = self.build_load(output, "")?.into_struct_value();
                self.accumulator = self.build_extract_value(output, 0, "")?.into_int_value();
                self.set_flag(Flag::Carry, self.extract_adc_bit(output, 1)?)?;
                self.set_flag(
                    Flag::Overflow,
                    self.build_select(
                        self.extract_adc_bit(output, 5)?,
                        self.extract_adc_bit(output, 2)?,
                        self.get_flag(Flag::Overflow)?,
                        "",
                    )?
                    .into_int_value(),
                )?;
                self.set_flag(Flag::Zero, self.extract_adc_bit(output, 3)?)?;
                self.set_flag(Flag::Negative, self.extract_adc_bit(output, 4)?)?;

                self.handle_page_cross(page_crossed, 1)?;
            }
            Instr::SBC(op) => {
                let (op, page_crossed) = self.translate_operand(op)?;

                let lhs = self.accumulator;
                let rhs = op;
                let carry = self.get_flag(Flag::Carry)?;
                let decimal =
                    self.build_and(self.get_flag(Flag::Decimal)?, self.decimal_enabled, "")?;

                let output = self.build_alloca(self.adc_output_type, "")?;
                let output_void = self.build_pointer_cast(
                    output,
                    self.cx.i8_type().ptr_type(AddressSpace::default()),
                    "",
                )?;

                self.build_call(
                    self.sbc,
                    &[
                        lhs.into(),
                        rhs.into(),
                        carry.into(),
                        decimal.into(),
                        output_void.into(),
                    ],
                    "",
                )?;

                let output = self.build_load(output, "")?.into_struct_value();
                self.accumulator = self.build_extract_value(output, 0, "")?.into_int_value();
                self.set_flag(Flag::Carry, self.extract_adc_bit(output, 1)?)?;
                self.set_flag(
                    Flag::Overflow,
                    self.build_select(
                        self.extract_adc_bit(output, 5)?,
                        self.extract_adc_bit(output, 2)?,
                        self.get_flag(Flag::Overflow)?,
                        "",
                    )?
                    .into_int_value(),
                )?;
                self.set_flag(Flag::Zero, self.extract_adc_bit(output, 3)?)?;
                self.set_flag(Flag::Negative, self.extract_adc_bit(output, 4)?)?;

                self.handle_page_cross(page_crossed, 1)?;
            }
            Instr::CMP(op) => {
                let (op, page_crossed) = self.translate_operand(op)?;
                let ge = self.build_int_compare(IntPredicate::UGE, self.accumulator, op, "")?;
                let eq = self.build_int_compare(IntPredicate::EQ, self.accumulator, op, "")?;
                let lt = self.build_int_compare(IntPredicate::ULT, self.accumulator, op, "")?;

                self.set_flag(Flag::Carry, ge)?;
                self.set_flag(Flag::Zero, eq)?;
                self.set_flag(Flag::Negative, lt)?;
                self.handle_page_cross(page_crossed, 1)?;
            }
            Instr::CPX(op) => {
                let (op, _) = self.translate_operand(op)?;
                let ge = self.build_int_compare(IntPredicate::UGE, self.x, op, "")?;
                let eq = self.build_int_compare(IntPredicate::EQ, self.x, op, "")?;
                let lt = self.build_int_compare(IntPredicate::ULT, self.x, op, "")?;

                self.set_flag(Flag::Carry, ge)?;
                self.set_flag(Flag::Zero, eq)?;
                self.set_flag(Flag::Negative, lt)?;
            }
            Instr::CPY(op) => {
                let (op, _) = self.translate_operand(op)?;
                let ge = self.build_int_compare(IntPredicate::UGE, self.y, op, "")?;
                let eq = self.build_int_compare(IntPredicate::EQ, self.y, op, "")?;
                let lt = self.build_int_compare(IntPredicate::ULT, self.y, op, "")?;

                self.set_flag(Flag::Carry, ge)?;
                self.set_flag(Flag::Zero, eq)?;
                self.set_flag(Flag::Negative, lt)?;
            }
            Instr::INC(addr) => {
                let (addr, _) = self.translate_address(addr)?;
                let op = self.build_read_u8(addr)?;
                let res = self.build_int_add(op, self.cx.i8_type().const_int(1, false), "")?;

                self.set_nz(res)?;
                self.build_write_u8(addr, res, cycles)?;
            }
            Instr::INX => {
                self.x = self.build_int_add(self.x, self.cx.i8_type().const_int(1, false), "")?;
                self.set_nz(self.x)?;
            }
            Instr::INY => {
                self.y = self.build_int_add(self.y, self.cx.i8_type().const_int(1, false), "")?;
                self.set_nz(self.y)?;
            }
            Instr::DEC(addr) => {
                let (addr, _) = self.translate_address(addr)?;
                let op = self.build_read_u8(addr)?;
                let res = self.build_int_sub(op, self.cx.i8_type().const_int(1, false), "")?;

                self.set_nz(res)?;
                self.build_write_u8(addr, res, cycles)?;
            }
            Instr::DEX => {
                self.x = self.build_int_sub(self.x, self.cx.i8_type().const_int(1, false), "")?;
                self.set_nz(self.x)?;
            }
            Instr::DEY => {
                self.y = self.build_int_sub(self.y, self.cx.i8_type().const_int(1, false), "")?;
                self.set_nz(self.y)?;
            }
            Instr::ASL(Operand::Accumulator) => {
                let i8_type = self.cx.i8_type();

                let carry = self.build_int_compare(
                    IntPredicate::SLT,
                    self.accumulator,
                    i8_type.const_zero(),
                    "",
                )?;
                self.set_flag(Flag::Carry, carry)?;

                self.accumulator =
                    self.build_left_shift(self.accumulator, i8_type.const_int(1, false), "")?;
                self.set_nz(self.accumulator)?;
            }
            Instr::ASL(Operand::Addressing(addr)) => {
                let i8_type = self.cx.i8_type();

                let (addr, _) = self.translate_address(addr)?;
                let op = self.build_read_u8(addr)?;
                let res = self.build_left_shift(op, i8_type.const_int(1, false), "")?;

                let carry = self.build_int_compare(
                    IntPredicate::SLT,
                    self.accumulator,
                    i8_type.const_zero(),
                    "",
                )?;
                self.set_flag(Flag::Carry, carry)?;
                self.set_nz(res)?;

                self.build_write_u8(addr, res, cycles)?;
            }
            Instr::LSR(Operand::Accumulator) => {
                let i8_type = self.cx.i8_type();

                let carry = self.build_int_truncate(self.accumulator, self.cx.bool_type(), "")?;
                self.set_flag(Flag::Carry, carry)?;

                self.accumulator = self.build_right_shift(
                    self.accumulator,
                    i8_type.const_int(1, false),
                    false,
                    "",
                )?;
                self.set_nz(self.accumulator)?;
            }
            Instr::LSR(Operand::Addressing(addr)) => {
                let i8_type = self.cx.i8_type();

                let (addr, _) = self.translate_address(addr)?;
                let op = self.build_read_u8(addr)?;
                let res = self.build_right_shift(op, i8_type.const_int(1, false), false, "")?;

                let carry = self.build_int_truncate(self.accumulator, self.cx.bool_type(), "")?;
                self.set_flag(Flag::Carry, carry)?;
                self.set_nz(res)?;

                self.build_write_u8(addr, res, cycles)?;
            }
            Instr::ROL(Operand::Accumulator) => {
                let i8_type = self.cx.i8_type();
                let prev_acc = self.accumulator;

                let ext = self.build_int_z_extend(self.get_flag(Flag::Carry)?, i8_type, "")?;
                self.accumulator = self.build_or(
                    self.build_left_shift(self.accumulator, i8_type.const_int(1, false), "")?,
                    ext,
                    "",
                )?;

                let carry =
                    self.build_int_compare(IntPredicate::SLT, prev_acc, i8_type.const_zero(), "")?;
                self.set_flag(Flag::Carry, carry)?;
                self.set_nz(self.accumulator)?;
            }
            Instr::ROL(Operand::Addressing(addr)) => {
                let (addr, _) = self.translate_address(addr)?;

                let i8_type = self.cx.i8_type();
                let prev = self.build_read_u8(addr)?;

                let ext = self.build_int_z_extend(self.get_flag(Flag::Carry)?, i8_type, "")?;
                let res = self.build_or(
                    self.build_left_shift(prev, i8_type.const_int(1, false), "")?,
                    ext,
                    "",
                )?;

                let carry =
                    self.build_int_compare(IntPredicate::SLT, prev, i8_type.const_zero(), "")?;
                self.set_flag(Flag::Carry, carry)?;
                self.set_nz(res)?;
                self.build_write_u8(addr, res, cycles)?;
            }
            Instr::ROR(Operand::Accumulator) => {
                let i8_type = self.cx.i8_type();
                let zero = i8_type.const_zero();
                let prev_acc = self.accumulator;

                let ext = self
                    .build_select(
                        self.get_flag(Flag::Carry)?,
                        i8_type.const_int(1 << 7, false),
                        zero,
                        "",
                    )?
                    .into_int_value();

                self.accumulator = self.build_or(
                    self.build_right_shift(
                        self.accumulator,
                        i8_type.const_int(1, false),
                        false,
                        "",
                    )?,
                    ext,
                    "",
                )?;

                let carry = self.build_int_truncate(prev_acc, self.cx.bool_type(), "")?;
                self.set_flag(Flag::Carry, carry)?;
                self.set_nz(self.accumulator)?;
            }
            Instr::ROR(Operand::Addressing(addr)) => {
                let (addr, _) = self.translate_address(addr)?;

                let i8_type = self.cx.i8_type();
                let zero = i8_type.const_zero();
                let prev = self.build_read_u8(addr)?;

                let ext = self
                    .build_select(
                        self.get_flag(Flag::Carry)?,
                        i8_type.const_int(1 << 7, false),
                        zero,
                        "",
                    )?
                    .into_int_value();

                let res = self.build_or(
                    self.build_right_shift(prev, i8_type.const_int(1, false), false, "")?,
                    ext,
                    "",
                )?;

                let carry = self.build_int_truncate(prev, self.cx.bool_type(), "")?;
                self.set_flag(Flag::Carry, carry)?;
                self.set_nz(res)?;
                self.build_write_u8(addr, res, cycles)?;
            }
            Instr::ASL(Operand::Immediate(_))
            | Instr::LSR(Operand::Immediate(_))
            | Instr::ROL(Operand::Immediate(_))
            | Instr::ROR(Operand::Immediate(_)) => unreachable!(),
            Instr::JMP(addr) => {
                return self.build_jump(
                    self.cx.i16_type().const_int(addr as u64, false),
                    cycles,
                    true,
                );
            }
            Instr::JMPIndirect(addr) => {
                let next_pc =
                    self.build_read_u16(self.cx.i16_type().const_int(addr as u64, false))?;
                return self.build_jump(next_pc, cycles, false);
            }
            Instr::JSR(addr) => {
                self.stack_push_u16(
                    self.cx
                        .i16_type()
                        .const_int(self.pc.wrapping_sub(1) as u64, false),
                )?;
                return self.build_jump(
                    self.cx.i16_type().const_int(addr as u64, false),
                    cycles,
                    true,
                );
            }
            Instr::RTS => {
                let next_pc = self.stack_pop_u16()?;
                return self.build_jump(
                    self.build_int_add(next_pc, self.cx.i16_type().const_int(1, false), "")?,
                    cycles,
                    true,
                );
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
            Instr::BRK => {
                let i16_type = self.cx.i16_type();

                self.stack_push_u16(i16_type.const_int(self.pc.wrapping_add(1) as u64, false))?;
                let flags = self.flags_into_u8(false)?;
                self.stack_push(flags, cycles)?;
                self.insert_flag(Flag::InterruptDisable)?;

                let brk_addr = self.build_read_u16(i16_type.const_int(0xfffe, false))?;
                return self.build_jump(brk_addr, cycles, true);
            }
            Instr::NOP => {}
            Instr::RTI => {
                let next_flags = self.stack_pop()?;
                self.flags_from_u8(next_flags)?;

                let next_pc = self.stack_pop_u16()?;
                return self.build_jump(next_pc, cycles, true);
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
                (
                    self.build_read_u16(self.build_int_z_extend(subptr, i16_type, "")?)?,
                    None,
                )
            }
            Addressing::IndirectIndexed(base) => {
                let base = self.build_read_u16(i16_type.const_int(base as u64, false))?;
                let addr =
                    self.build_int_add(base, self.build_int_z_extend(self.y, i16_type, "")?, "")?;
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
        // we must preserve the same prev_state for both branches
        let prev_state = self.prev_state.clone();

        let page_crossed = self.page_crossed(i16_type.const_int(self.pc as u64, false), addr)?;
        let ticks = self.build_int_add(
            self.build_int_z_extend(page_crossed, i8_type, "")?,
            i8_type.const_int(1, false),
            "",
        )?;
        self.build_tick(ticks)?;
        self.builder.build_return(Some(&addr))?;

        self.builder.position_at_end(continue_block);
        self.prev_state = prev_state;
        return Ok(());
    }

    fn build_jump(
        &mut self,
        addr: IntValue<'a>,
        cycles: u8,
        inline_jump: bool,
    ) -> Result<bool, BuilderError> {
        if let Some(addr) = addr.get_zero_extended_constant() {
            let addr = addr as u16;

            if addr == self.entry_pc {
                self.flush()?;
                self.build_store(
                    self.prev_cycles,
                    self.cx.i8_type().const_int(cycles as u64, false),
                )?;
                self.build_unconditional_branch(self.entry_block)?;
                return Ok(true);
            } else if inline_jump && false {
                if let Some(next_count) = self.inline_count.checked_sub(1) {
                    self.inline_count = next_count;
                    self.build_tick(self.cx.i8_type().const_int(cycles as u64, false))?;
                    self.pc = addr;
                    self.start_pc = self.start_pc.min(self.pc);
                    self.end_pc = self.end_pc.max(self.pc);
                    return Ok(false);
                }
            }
        }

        self.flush()?;
        self.build_store(
            self.prev_cycles,
            self.cx.i8_type().const_int(cycles as u64, false),
        )?;
        self.build_return(Some(&addr))?;
        return Ok(true);
    }

    pub fn build_read_u8(&mut self, ptr: IntValue<'a>) -> Result<IntValue<'a>, BuilderError> {
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

        self.handle_memory_result(res, 0)?;
        return Ok(self.build_load(output, "")?.into_int_value());
    }

    pub fn build_read_u16(&mut self, ptr: IntValue<'a>) -> Result<IntValue<'a>, BuilderError> {
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

        self.handle_memory_result(res, 0)?;
        return Ok(self.build_load(output, "")?.into_int_value());
    }

    pub fn build_write_u8(
        &mut self,
        ptr: IntValue<'a>,
        val: IntValue<'a>,
        cycles: u8,
    ) -> Result<(), BuilderError> {
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

        return self.handle_memory_result(res, cycles);
    }

    pub fn build_write_u16(
        &mut self,
        ptr: IntValue<'a>,
        val: IntValue<'a>,
        cycles: u8,
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

        return self.handle_memory_result(res, cycles);
    }

    #[cfg(target_endian = "big")]
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

    pub fn build_handle_nmi(&mut self) -> Result<(), BuilderError> {
        let i8_type = self.cx.i8_type();
        let i16_type = self.cx.i16_type();

        let nmi_interrupt = self
            .build_load(self.nmi_interrupt_ptr, "")?
            .into_int_value();

        nmi_interrupt
            .as_instruction_value()
            .unwrap()
            .set_volatile(true)
            .unwrap();

        self.build_store(self.nmi_interrupt_ptr, self.cx.i8_type().const_zero())?
            .set_volatile(true)
            .unwrap();

        let nmi_interrupt =
            self.build_int_compare(IntPredicate::NE, nmi_interrupt, i8_type.const_zero(), "")?;

        let then_block = self.cx.append_basic_block(self.fn_value, "");
        let continue_block = self.cx.append_basic_block(self.fn_value, "");
        self.build_conditional_branch(nmi_interrupt, then_block, continue_block)?;

        // we must preserve the same prev state for both branches
        let prev_prev_state = self.prev_state.clone();
        let prev_state = State {
            accumulator: self.accumulator,
            x: self.x,
            y: self.y,
            stack_ptr: self.stack_ptr,
            flags: self.flags,
        };

        // Handle NMI interrupt
        self.position_at_end(then_block);
        self.stack_push_u16(i16_type.const_int(self.pc as u64, false))?;
        let flags = self.flags_into_u8(true)?;
        self.stack_push(flags, 0)?;
        self.insert_flag(Flag::InterruptDisable)?;
        let next_pc = self.build_read_u16(i16_type.const_int(0xfffa, false))?;
        self.build_jump(next_pc, 2, false)?;

        self.position_at_end(continue_block);
        // Restore state
        self.accumulator = prev_state.accumulator;
        self.x = prev_state.x;
        self.y = prev_state.y;
        self.stack_ptr = prev_state.stack_ptr;
        self.flags = prev_state.flags;
        self.prev_state = prev_prev_state;

        return Ok(());
    }

    fn extract_adc_bit(
        &self,
        adc: StructValue<'a>,
        index: u32,
    ) -> Result<IntValue<'a>, BuilderError> {
        let byte = self.build_extract_value(adc, index, "")?.into_int_value();
        return self.build_int_truncate(byte, self.cx.bool_type(), "");
    }

    fn handle_memory_result(&mut self, res: IntValue<'a>, cycles: u8) -> Result<(), BuilderError> {
        let condition =
            self.build_int_compare(IntPredicate::SLT, res, self.cx.i8_type().const_zero(), "")?;

        let then_block = self.cx.append_basic_block(self.fn_value, "");
        let continue_block = self.cx.append_basic_block(self.fn_value, "");
        self.build_conditional_branch(condition, then_block, continue_block)?;

        self.builder.position_at_end(then_block);
        // we must preserve the same prev state for both branches
        let prev_state = self.prev_state.clone();
        // return next pc, perhaps the error is just a code cache invalidation
        self.build_jump(
            self.cx.i16_type().const_int(self.pc as u64, false),
            cycles,
            false,
        )?;

        self.builder.position_at_end(continue_block);
        self.prev_state = prev_state;
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

#[derive(Clone)]
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
    pub fn stack_push(&mut self, val: IntValue<'a>, cycles: u8) -> Result<(), BuilderError> {
        let addr = self.stack_addr()?;
        self.stack_ptr =
            self.build_int_sub(self.stack_ptr, self.cx.i8_type().const_int(1, false), "")?;

        self.build_write_u8(addr, val, cycles)?; // TODO may need to set cycles
        return Ok(());
    }

    pub fn stack_push_u16(&mut self, val: IntValue<'a>) -> Result<(), BuilderError> {
        let [lo, hi] = self.build_to_le_bytes(val)?;
        self.stack_push(hi, 0)?; // TODO do we set cycles?
        self.stack_push(lo, 0)?; // TODO do we set cycles?
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
        return self.build_or(
            i16_type.const_int(0x100, false),
            self.build_int_z_extend(self.stack_ptr, i16_type, "")?,
            "",
        );
    }
}

unsafe extern "C" fn read_u8<M: Memory>(
    memory: *mut c_void,
    info: *const c_void,
    addr: u16,
    dst: *mut u8,
) -> i8 {
    let info = &*info.cast::<MemoryInfo<M>>();
    match (&mut *memory.cast::<M>()).read_u8(addr) {
        Ok(res) => {
            *dst = res;
            return 0;
        }
        Err(e) => {
            info.set_error(e);
            return -1;
        }
    }
}

unsafe extern "C" fn read_u16<M: Memory>(
    memory: *mut c_void,
    info: *const c_void,
    addr: u16,
    dst: *mut u16,
) -> i8 {
    let info = &*info.cast::<MemoryInfo<M>>();
    match (&mut *memory.cast::<M>()).read_u16(addr) {
        Ok(res) => {
            *dst = res;
            return 0;
        }
        Err(e) => {
            info.set_error(e);
            return -1;
        }
    }
}

unsafe extern "C" fn write_u8<M: Memory>(
    memory: *mut c_void,
    info: *const c_void,
    addr: u16,
    val: u8,
) -> i8 {
    let info = &*info.cast::<MemoryInfo<M>>();
    match (&mut *memory.cast::<M>()).write_u8(addr, val) {
        Ok(()) => return info.check_code_region(addr),
        Err(e) => {
            info.set_error(e);
            return -1;
        }
    }
}

unsafe extern "C" fn write_u16<M: Memory>(
    memory: *mut c_void,
    info: *const c_void,
    addr: u16,
    val: u16,
) -> i8 {
    let info = &*info.cast::<MemoryInfo<M>>();
    match (&mut *memory.cast::<M>()).write_u16(addr, val) {
        Ok(()) => return info.check_code_region(addr) | info.check_code_region(addr + 1),
        Err(e) => {
            info.set_error(e);
            return -1;
        }
    }
}

struct Compiled<'cx> {
    f: Result<Function, (FunctionValue<'cx>, Arc<AtomicCell<Function>>)>,
    range: Range<u16>,
    last_exec: Instant,
    _exec: ExecutionEngine<'cx>,
}

impl<'cx> Compiled<'cx> {
    pub fn new(module: Module<'cx>, range: Range<u16>, sem: Arc<Semaphore>) -> Self {
        #[repr(transparent)]
        struct AssertSend<T: ?Sized>(pub T);
        unsafe impl<T: ?Sized> Send for AssertSend<T> {}

        let res = Arc::new(AtomicCell::new(None));
        let exec = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let res2 = res.clone();
        std::thread::spawn(move || unsafe {
            let _guard = sem.acquire();
            let f = exec.get_function(FUNTION_NAME).unwrap();
            let _ = res2.replace(Some(f.into_raw()));
        });

        return Self {
            f: Err((module.get_function(FUNTION_NAME).unwrap(), res)),
            last_exec: Instant::now(),
            range,
            _exec: exec,
        };
    }

    #[inline]
    pub unsafe fn call<'a, M: Memory>(
        &mut self,
        state: *mut crate::cpu::State,
        memory: *mut M,
        tick: &Closure<dyn 'a + FnMut(u8)>,
        code_regions: &CodeRegionArray,
        prev_cycles: &mut u8,
    ) -> Result<(u16, Vec<u16>), M::Error> {
        self.last_exec = Instant::now();

        let info = MemoryInfo::<M> {
            code_regions,
            latest_error: Cell::new(None::<M::Error>),
            clear_code_cache: Cell::new(Vec::new()),
        };

        let next_pc = match self.get_compiled() {
            Ok(f) => f(
                state,
                tick.user_data(),
                tick.fn_ptr(),
                memory as *mut M as *mut c_void,
                std::ptr::addr_of!(info).cast(),
                read_u8::<M>,
                read_u16::<M>,
                write_u8::<M>,
                write_u16::<M>,
                prev_cycles,
            ),
            Err(e) => todo!(),
        };

        return match info.latest_error.take() {
            Some(err) => Err(err),
            None => Ok((next_pc, info.clear_code_cache.take())),
        };
    }

    fn get_compiled(&mut self) -> Result<Function, FunctionValue<'cx>> {
        match self.f.as_mut() {
            Ok(f) => Ok(*f),
            Err(recv) => match recv.take() {
                Some(f) => unsafe {
                    self.f = Ok(f);
                    Ok(*self.f.as_ref().unwrap_unchecked())
                },
                None => todo!(),
            },
        }
    }
}

struct MemoryInfo<'a, M: Memory> {
    code_regions: &'a CodeRegionArray,
    latest_error: Cell<Option<M::Error>>,
    clear_code_cache: Cell<Vec<u16>>,
}

impl<'a, M: Memory> MemoryInfo<'a, M> {
    pub fn set_error(&self, err: M::Error) {
        self.latest_error.set(Some(err));
    }

    pub fn check_code_region(&self, addr: u16) -> i8 {
        if *self.code_regions.get(addr as usize).unwrap() {
            self.clear_code_cache.set({
                let mut prev = self.clear_code_cache.take();
                prev.push(addr);
                prev
            });
            return -1;
        } else {
            return 0;
        }
    }
}

struct Semaphore {
    permits: Mutex<usize>,
    condvar: Condvar,
}

impl Semaphore {
    pub const fn new(permits: usize) -> Self {
        return Self {
            permits: Mutex::new(permits),
            condvar: Condvar::new(),
        };
    }

    pub fn acquire(&self) -> SemaphoreGuard<'_> {
        let mut permits = self.permits.lock().unwrap_or_else(PoisonError::into_inner);

        loop {
            if let Some(new_permits) = permits.checked_sub(1) {
                *permits = new_permits;
                return SemaphoreGuard { parent: self };
            }

            permits = self
                .condvar
                .wait(permits)
                .unwrap_or_else(PoisonError::into_inner);
        }
    }
}

pub struct SemaphoreGuard<'a> {
    parent: &'a Semaphore,
}

impl<'a> Drop for SemaphoreGuard<'a> {
    fn drop(&mut self) {
        *self
            .parent
            .permits
            .lock()
            .unwrap_or_else(PoisonError::into_inner) += 1;
        self.parent.condvar.notify_one();
    }
}
