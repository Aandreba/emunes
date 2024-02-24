use super::Backend;
use crate::cpu::{
    flags::{Flag, Flags},
    instrs::{read_instruction, Addressing, Instr, Operand},
    RunError,
};
use inkwell::{
    basic_block::BasicBlock,
    builder::BuilderError,
    context::Context,
    execution_engine::ExecutionEngine,
    intrinsics::Intrinsic,
    memory_buffer::MemoryBuffer,
    module::Module,
    support::LLVMString,
    values::{AnyValue, FunctionValue, IntValue, PointerValue, StructValue, VectorValue},
    AddressSpace, IntPredicate, OptimizationLevel,
};
use std::{
    collections::{hash_map::Entry, HashMap},
    ffi::c_void,
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
}

impl<'a> Backend for Llvm<'a> {
    type Error = BuilderError;

    fn run<M: crate::cpu::memory::Memory>(
        cpu: &mut crate::cpu::Cpu<M, Self>,
        mut pc: u16,
        tick: impl FnMut(u8),
    ) -> Result<(), RunError<M, Self>> {
        let this = &mut cpu.backend;

        let fn_value = match this.compiled.entry(pc) {
            Entry::Occupied(entry) => *entry.into_mut(),
            Entry::Vacant(_) => {
                let mut builder =
                    Builder::new(pc, &this.module, this.cx).map_err(RunError::Backend)?;
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
                        Instr::STA(addr) => builder.get_address(addr),
                        Instr::STX(addr) => builder.get_address(addr),
                        Instr::STY(addr) => builder.get_address(addr),
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
                        Instr::ADC(_) => todo!(),
                        Instr::SBC(_) => todo!(),
                        Instr::CMP(_) => todo!(),
                        Instr::CPX(_) => todo!(),
                        Instr::CPY(_) => todo!(),
                        Instr::INC(_) => todo!(),
                        Instr::INX => todo!(),
                        Instr::INY => todo!(),
                        Instr::DEC(_) => todo!(),
                        Instr::DEX => todo!(),
                        Instr::DEY => todo!(),
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
                        Instr::BRK => todo!(),
                        Instr::NOP => todo!(),
                        Instr::RTI => todo!(),
                        Instr::ASL(Operand::Immediate(_))
                        | Instr::LSR(Operand::Immediate(_))
                        | Instr::ROL(Operand::Immediate(_))
                        | Instr::ROR(Operand::Immediate(_)) => unreachable!(),
                    }
                    .map_err(RunError::Backend)?;
                };

                todo!()
            }
        };

        todo!()
    }
}

pub struct Builder<'a, 'b> {
    // Input
    input_accumulator: PointerValue<'a>,
    input_x: PointerValue<'a>,
    input_y: PointerValue<'a>,
    input_stack_ptr: PointerValue<'a>,
    input_flags: PointerValue<'a>,
    // Cpu
    accumulator: IntValue<'a>,
    x: IntValue<'a>,
    y: IntValue<'a>,
    stack_ptr: IntValue<'a>,
    flags: VectorValue<'a>,
    user_data: StructValue<'a>,
    // Misc
    fn_value: FunctionValue<'a>,
    builder: inkwell::builder::Builder<'a>,
    block: BasicBlock<'a>,
    module: &'b Module<'a>,
    cx: &'a Context,
}

impl<'a, 'b> Builder<'a, 'b> {
    pub fn new(pc: u16, module: &'b Module<'a>, cx: &'a Context) -> Result<Self, BuilderError> {
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
            input_accumulator,
            input_x,
            input_y,
            input_stack_ptr,
            input_flags,
            user_data,
            block,
            builder,
            fn_value,
            module,
            cx,
        });
    }

    pub fn get_operand(&self, op: Operand) -> Result<(IntValue<'a>, IntValue<'a>), BuilderError> {
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
        &self,
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

    pub fn sta(&mut self, addr: Addressing) -> Result<(), BuilderError> {
        let (addr, _) = self.get_address(addr)?;
        self.write_u8(addr, self.accumulator)?;
        return Ok(());
    }

    pub fn stx(&mut self, addr: Addressing) -> Result<(), BuilderError> {
        let (addr, _) = self.get_address(addr)?;
        self.write_u8(addr, self.x)?;
        return Ok(());
    }

    pub fn sty(&mut self, addr: Addressing) -> Result<(), BuilderError> {
        let (addr, _) = self.get_address(addr)?;
        self.write_u8(addr, self.y)?;
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

    pub fn read_u8(&self, addr: IntValue<'a>) -> Result<IntValue<'a>, BuilderError> {
        let fn_value = self.module.get_function("_read_u8").unwrap();
        let user_data = self.builder.build_extract_value(self.user_data, 1, "")?;

        return Ok(self
            .builder
            .build_call(fn_value, &[addr.into(), user_data.into()], "")?
            .as_any_value_enum()
            .into_int_value());
    }

    pub fn read_u16(&self, addr: IntValue<'a>) -> Result<IntValue<'a>, BuilderError> {
        let fn_value = self.module.get_function("_read_u16").unwrap();
        let user_data = self.builder.build_extract_value(self.user_data, 2, "")?;

        return Ok(self
            .builder
            .build_call(fn_value, &[addr.into(), user_data.into()], "")?
            .as_any_value_enum()
            .into_int_value());
    }

    pub fn write_u8(&self, addr: IntValue<'a>, value: IntValue<'a>) -> Result<(), BuilderError> {
        let fn_value = self.module.get_function("_write_u8").unwrap();
        let user_data = self.builder.build_extract_value(self.user_data, 3, "")?;

        self.builder
            .build_call(fn_value, &[addr.into(), value.into(), user_data.into()], "")?;

        return Ok(());
    }

    pub fn write_u16(&self, addr: IntValue<'a>, value: IntValue<'a>) -> Result<(), BuilderError> {
        let fn_value = self.module.get_function("_write_u16").unwrap();
        let user_data = self.builder.build_extract_value(self.user_data, 4, "")?;

        self.builder
            .build_call(fn_value, &[addr.into(), value.into(), user_data.into()], "")?;

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
struct ExternCpu {
    pub accumulator: u8,
    pub x: u8,
    pub y: u8,
    pub stack_ptr: u8,
    pub flags: Flags,
    pub user_data: UserData,
}

#[repr(C)]
struct UserData {
    pub tick: *mut c_void,
    pub read_u8: *mut c_void,
    pub read_u16: *mut c_void,
    pub write_u8: *mut c_void,
    pub write_u16: *mut c_void,
}
