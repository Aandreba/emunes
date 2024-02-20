use self::{
    flags::Flags,
    instrs::{page_crossed, Addressing, Instr, Operand},
    memory::Memory,
};
use std::{fmt::Debug, num::Wrapping};

pub mod flags;
pub mod instrs;
pub mod memory;

pub struct Cpu<'a> {
    accumulator: u8,
    x: u8,
    y: u8,
    stack_ptr: u8,
    flags: Flags,
    memory: Memory<'a>,
}

impl<'a> Cpu<'a> {
    pub fn new(memory: Memory<'a>) -> Self {
        return Self {
            accumulator: 0,
            x: 0,
            y: 0,
            stack_ptr: 0xfe,
            flags: Flags::default(),
            memory,
        };
    }

    pub fn restart(&mut self, tick: impl FnMut(&mut Self, u8)) {
        let pc = self.memory.read_u16(0xfffc);
        self.run(pc, tick)
    }

    pub fn run(&mut self, pc: u16, mut tick: impl FnMut(&mut Self, u8)) {
        let mut pc = Wrapping(pc);
        let mut prev_cycles = 0;

        loop {
            tick(self, prev_cycles);
            let instr = self
                .read_instruction(&mut pc)
                .expect(&format!("unknown instruction found at 0x{:04X}", pc.0 - 1));
            prev_cycles = instr.cycles();

            log::trace!("{instr:?}");

            match instr {
                Instr::LDA(op) => {
                    let (op, page_crossed) = self.get_operand(op);
                    self.accumulator = op;
                    self.flags.set_nz(self.accumulator);

                    if page_crossed {
                        tick(self, 1);
                    }
                }
                Instr::LDX(op) => {
                    let (op, page_crossed) = self.get_operand(op);
                    self.x = op;
                    self.flags.set_nz(self.x);

                    if page_crossed {
                        tick(self, 1);
                    }
                }
                Instr::LDY(op) => {
                    let (op, page_crossed) = self.get_operand(op);
                    self.y = op;
                    self.flags.set_nz(self.y);

                    if page_crossed {
                        tick(self, 1);
                    }
                }
                Instr::STA(addr) => {
                    let (addr, _) = self.get_addressing(addr);
                    self.memory.write_u8(addr, self.accumulator);
                }
                Instr::STX(addr) => {
                    let (addr, _) = self.get_addressing(addr);
                    self.memory.write_u8(addr, self.x);
                }
                Instr::STY(addr) => {
                    let (addr, _) = self.get_addressing(addr);
                    self.memory.write_u8(addr, self.y);
                }
                Instr::TAX => todo!(),
                Instr::TAY => todo!(),
                Instr::TXA => todo!(),
                Instr::TYA => todo!(),
                Instr::TSX => todo!(),
                Instr::TXS => todo!(),
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
                Instr::INX => todo!(),
                Instr::INY => todo!(),
                Instr::DEC(_) => todo!(),
                Instr::DEX => todo!(),
                Instr::DEY => todo!(),
                Instr::ASL(_) => todo!(),
                Instr::LSR(_) => todo!(),
                Instr::ROL(_) => todo!(),
                Instr::ROR(_) => todo!(),
                Instr::JMP(_) => todo!(),
                Instr::JMPIndirect(_) => todo!(),
                Instr::JSR(_) => todo!(),
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
                Instr::NOP => {}
                Instr::RTI => todo!(),
            }
        }
    }
}

// Stack Ops
impl<'a> Cpu<'a> {
    pub fn push(&mut self, val: u8) {
        self.memory.write_u8(self.stack_addr(), val);
        self.stack_ptr = self.stack_ptr.wrapping_sub(1);
    }

    pub fn pop(&mut self) -> u8 {
        self.stack_ptr = self.stack_ptr.wrapping_add(1);
        return self.memory.read_u8(self.stack_addr());
    }

    fn stack_addr(&self) -> u16 {
        return 0x100 + self.stack_ptr as u16;
    }
}

// Operand ops
impl<'a> Cpu<'a> {
    pub fn get_operand(&self, op: Operand) -> (u8, bool) {
        match op {
            Operand::Accumulator => (self.accumulator, false),
            Operand::Immediate(val) => (val, false),
            Operand::Addressing(addr) => {
                let (addr, page_crossed) = self.get_addressing(addr);
                (self.memory.read_u8(addr), page_crossed)
            }
        }
    }

    pub fn get_addressing(&self, addr: Addressing) -> (u16, bool) {
        match addr {
            Addressing::ZeroPage(addr) => (addr as u16, false),
            Addressing::ZeroPageX(base) => ((base as u16).wrapping_add(self.x as u16), false),
            Addressing::ZeroPageY(base) => ((base as u16).wrapping_add(self.y as u16), false),
            Addressing::Absolute(addr) => (addr, false),
            Addressing::AbsoluteX(base) => {
                let addr = base.wrapping_add(self.x as u16);
                (addr, page_crossed(base, addr))
            }
            Addressing::AbsoluteY(base) => {
                let addr = base.wrapping_add(self.y as u16);
                (addr, page_crossed(base, addr))
            }
            Addressing::IndexedIndirect(base) => (
                self.memory
                    .read_u16((base as u16).wrapping_add(self.x as u16)),
                false,
            ),
            Addressing::IndirectIndexed(base) => {
                let base = self.memory.read_u16(base as u16);
                let addr = base.wrapping_add(self.y as u16);
                (addr, page_crossed(base, addr))
            }
        }
    }
}

impl Debug for Cpu<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cpu")
            .field("accumulator", &self.accumulator)
            .field("x", &self.x)
            .field("y", &self.y)
            .field("stack_ptr", &self.stack_ptr)
            .field("flags", &self.flags)
            .finish_non_exhaustive()
    }
}
