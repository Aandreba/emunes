use self::{
    flags::Flags,
    instrs::{page_crossed, Addressing, Instr, Operand},
    memory::Memory,
};
use crate::cpu::flags::Flag;
use std::fmt::Debug;

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

    pub fn run(&mut self, mut pc: u16, mut tick: impl FnMut(&mut Self, u8)) {
        let mut prev_cycles = 0;

        loop {
            let prev_pc = pc;
            tick(self, prev_cycles);

            let instr = self
                .read_instruction(&mut pc)
                .expect(&format!("unknown instruction found at 0x{prev_pc:04X}"));
            prev_cycles = instr.cycles();

            log::trace!("{prev_pc:04X}: {instr:04X?}");

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
                Instr::TAX => {
                    self.x = self.accumulator;
                    self.flags.set_nz(self.accumulator);
                }
                Instr::TAY => {
                    self.y = self.accumulator;
                    self.flags.set_nz(self.accumulator);
                }
                Instr::TXA => {
                    self.accumulator = self.x;
                    self.flags.set_nz(self.x);
                }
                Instr::TYA => {
                    self.accumulator = self.y;
                    self.flags.set_nz(self.y);
                }
                Instr::TSX => {
                    self.x = self.stack_ptr;
                    self.flags.set_nz(self.x)
                }
                Instr::TXS => self.stack_ptr = self.x,
                Instr::PHA => self.push(self.accumulator),
                Instr::PHP => self.push(self.flags.into_u8(false)),
                Instr::PLA => {
                    self.accumulator = self.pop();
                    self.flags.set_nz(self.accumulator)
                }
                Instr::PLP => self.flags = Flags::from_u8(self.pop()),
                Instr::AND(op) => {
                    let (op, page_crossed) = self.get_operand(op);
                    self.accumulator &= op;
                    self.flags.set_nz(self.accumulator);

                    if page_crossed {
                        tick(self, 1);
                    }
                }
                Instr::EOR(op) => {
                    let (op, page_crossed) = self.get_operand(op);
                    self.accumulator ^= op;
                    self.flags.set_nz(self.accumulator);

                    if page_crossed {
                        tick(self, 1);
                    }
                }
                Instr::ORA(op) => {
                    let (op, page_crossed) = self.get_operand(op);
                    self.accumulator |= op;
                    self.flags.set_nz(self.accumulator);

                    if page_crossed {
                        tick(self, 1);
                    }
                }
                Instr::BIT(addr) => {
                    let op = self.memory.read_u8(self.get_addressing(addr).0);
                    self.flags.set(Flag::Zero, (self.accumulator & op) == 0);
                    self.flags.set(Flag::Negative, (op as i8).is_negative());
                    self.flags.set(Flag::Overflow, (op >> 6) & 1 == 1);
                }
                Instr::ADC(op) => {
                    let (op, page_crossed) = self.get_operand(op);
                    let carry = self.flags.contains(Flag::Carry);

                    let (unsigned, next_carry) = self.accumulator.carrying_add(op, carry);
                    let (signed, next_overflow) =
                        (self.accumulator as i8).carrying_add(op as i8, carry);
                    debug_assert_eq!(unsigned, signed as u8);

                    self.accumulator = unsigned;
                    self.flags.set_nz(self.accumulator);
                    self.flags.set(Flag::Carry, next_carry);
                    self.flags.set(Flag::Overflow, next_overflow);

                    if page_crossed {
                        tick(self, 1)
                    }
                }
                Instr::SBC(op) => {
                    let (op, page_crossed) = self.get_operand(op);
                    let carry = !self.flags.contains(Flag::Carry);

                    let (unsigned, next_carry) = self.accumulator.borrowing_sub(op, carry);
                    let (signed, next_overflow) =
                        (self.accumulator as i8).borrowing_sub(op as i8, carry);
                    debug_assert_eq!(unsigned, signed as u8);

                    self.accumulator = unsigned;
                    self.flags.set_nz(self.accumulator);
                    self.flags.set(Flag::Carry, next_carry);
                    self.flags.set(Flag::Overflow, next_overflow);

                    if page_crossed {
                        tick(self, 1)
                    }
                }
                Instr::CMP(op) => {
                    let (op, page_crossed) = self.get_operand(op);
                    self.flags.set(Flag::Carry, self.accumulator >= op);
                    self.flags.set(Flag::Zero, self.accumulator == op);
                    self.flags.set(Flag::Negative, self.accumulator < op);

                    if page_crossed {
                        tick(self, 1)
                    }
                }
                Instr::CPX(op) => {
                    let (op, _) = self.get_operand(op);
                    self.flags.set(Flag::Carry, self.x >= op);
                    self.flags.set(Flag::Zero, self.x == op);
                    self.flags.set(Flag::Negative, self.x < op);
                }
                Instr::CPY(op) => {
                    let (op, _) = self.get_operand(op);
                    self.flags.set(Flag::Carry, self.y >= op);
                    self.flags.set(Flag::Zero, self.y == op);
                    self.flags.set(Flag::Negative, self.y < op);
                }
                Instr::INC(addr) => {
                    let (addr, _) = self.get_addressing(addr);
                    let res = self.memory.read_u8(addr).wrapping_add(1);
                    self.memory.write_u8(addr, res);
                    self.flags.set_nz(res)
                }
                Instr::INX => {
                    self.x = self.x.wrapping_add(1);
                    self.flags.set_nz(self.x);
                }
                Instr::INY => {
                    self.y = self.y.wrapping_add(1);
                    self.flags.set_nz(self.y);
                }
                Instr::DEC(addr) => {
                    let (addr, _) = self.get_addressing(addr);
                    let res = self.memory.read_u8(addr).wrapping_sub(1);
                    self.memory.write_u8(addr, res);
                    self.flags.set_nz(res)
                }
                Instr::DEX => {
                    self.x = self.x.wrapping_sub(1);
                    self.flags.set_nz(self.x);
                }
                Instr::DEY => {
                    self.y = self.y.wrapping_sub(1);
                    self.flags.set_nz(self.y);
                }
                Instr::ASL(Operand::Accumulator) => {
                    self.flags
                        .set(Flag::Carry, (self.accumulator as i8).is_negative());
                    self.accumulator <<= 1;
                    self.flags.set_nz(self.accumulator);
                }
                Instr::ASL(Operand::Addressing(addr)) => {
                    let (addr, _) = self.get_addressing(addr);
                    let op = self.memory.read_u8(addr);
                    let res = op << 1;
                    self.memory.write_u8(addr, res);

                    self.flags.set(Flag::Carry, (op as i8).is_negative());
                    self.flags.set_nz(self.accumulator);
                }
                Instr::LSR(Operand::Accumulator) => {
                    self.flags.set(Flag::Carry, self.accumulator & 1 == 1);
                    self.accumulator >>= 1;
                    self.flags.set_nz(self.accumulator);
                }
                Instr::LSR(Operand::Addressing(addr)) => {
                    let (addr, _) = self.get_addressing(addr);
                    let op = self.memory.read_u8(addr);
                    let res = op >> 1;
                    self.memory.write_u8(addr, res);

                    self.flags.set(Flag::Carry, op & 1 == 1);
                    self.flags.set_nz(self.accumulator);
                }
                Instr::ROL(Operand::Accumulator) => {
                    self.flags
                        .set(Flag::Carry, (self.accumulator as i8).is_negative());
                    self.accumulator = self.accumulator.rotate_left(1);
                    self.flags.set_nz(self.accumulator);
                }
                Instr::ROL(Operand::Addressing(addr)) => {
                    let (addr, _) = self.get_addressing(addr);
                    let op = self.memory.read_u8(addr);
                    let res = op.rotate_left(1);
                    self.memory.write_u8(addr, res);

                    self.flags.set(Flag::Carry, (op as i8).is_negative());
                    self.flags.set_nz(self.accumulator);
                }
                Instr::ROR(Operand::Accumulator) => {
                    self.flags.set(Flag::Carry, self.accumulator & 1 == 1);
                    self.accumulator = self.accumulator.rotate_right(1);
                    self.flags.set_nz(self.accumulator);
                }
                Instr::ROR(Operand::Addressing(addr)) => {
                    let (addr, _) = self.get_addressing(addr);
                    let op = self.memory.read_u8(addr);
                    let res = op.rotate_right(1);
                    self.memory.write_u8(addr, res);

                    self.flags.set(Flag::Carry, op & 1 == 1);
                    self.flags.set_nz(self.accumulator);
                }
                Instr::ASL(Operand::Immediate(_))
                | Instr::LSR(Operand::Immediate(_))
                | Instr::ROL(Operand::Immediate(_))
                | Instr::ROR(Operand::Immediate(_)) => unreachable!(),
                Instr::JMP(addr) => pc = addr,
                Instr::JMPIndirect(base) => pc = self.memory.read_u16(base),
                Instr::JSR(addr) => {
                    self.push_u16(pc.wrapping_sub(1));
                    pc = addr
                }
                Instr::BCC(addr) => {
                    if !self.flags.contains(Flag::Carry) {
                        tick(self, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BCS(addr) => {
                    if self.flags.contains(Flag::Carry) {
                        tick(self, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BEQ(addr) => {
                    if self.flags.contains(Flag::Zero) {
                        tick(self, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BMI(addr) => {
                    if self.flags.contains(Flag::Negative) {
                        tick(self, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BNE(addr) => {
                    if !self.flags.contains(Flag::Zero) {
                        #[cfg(debug_assertions)]
                        if prev_pc == addr {
                            return;
                        }

                        tick(self, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BPL(addr) => {
                    if !self.flags.contains(Flag::Negative) {
                        tick(self, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BVC(addr) => {
                    if !self.flags.contains(Flag::Overflow) {
                        tick(self, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BVS(addr) => {
                    if self.flags.contains(Flag::Overflow) {
                        tick(self, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::CLC => self.flags.remove(Flag::Carry),
                Instr::CLD => self.flags.remove(Flag::Decimal),
                Instr::CLI => self.flags.remove(Flag::InterruptDisable),
                Instr::CLV => self.flags.remove(Flag::Overflow),
                Instr::SEC => self.flags.insert(Flag::Carry),
                Instr::SED => self.flags.insert(Flag::Decimal),
                Instr::SEI => self.flags.insert(Flag::InterruptDisable),
                // https://github.com/bugzmanov/nes_ebook/blob/master/code/ch8/src/cpu.rs#L692
                Instr::BRK => {
                    pc = pc.wrapping_add(1);
                    if !self.flags.contains(Flag::InterruptDisable) {
                        self.push_u16(pc);
                        self.push(self.flags.into_u8(false));
                        self.flags.insert(Flag::InterruptDisable);
                        pc = self.memory.read_u16(0xfffe)
                    }
                }
                Instr::NOP => {}
                Instr::RTS => {
                    pc = self.pop_u16().wrapping_add(1);
                }
                Instr::RTI => {
                    self.flags = Flags::from_u8(self.pop());
                    pc = self.pop_u16();
                }
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

    pub fn push_u16(&mut self, val: u16) {
        let [lo, hi] = val.to_le_bytes();
        self.push(hi);
        self.push(lo);
    }

    pub fn pop(&mut self) -> u8 {
        self.stack_ptr = self.stack_ptr.wrapping_add(1);
        return self.memory.read_u8(self.stack_addr());
    }

    pub fn pop_u16(&mut self) -> u16 {
        let lo = self.pop();
        let hi = self.pop();
        return u16::from_le_bytes([lo, hi]);
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
