use crate::cpu::memory::Memory;

use super::Cpu;
use std::num::Wrapping;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Instr {
    LDA(Operand),
    LDX(Operand),
    LDY(Operand),
    STA(Addressing),
    STX(Addressing),
    STY(Addressing),
    TAX,
    TAY,
    TXA,
    TYA,
    TSX,
    TXS,
    PHA,
    PHP,
    PLA,
    PLP,
    AND(Operand),
    EOR(Operand),
    ORA(Operand),
    BIT(Operand),
    ADC(Operand),
    SBC(Operand),
    CMP(Operand),
    CPX(Operand),
    CPY(Operand),
    INC(Addressing),
    INX,
    INY,
    DEC(Addressing),
    DEX,
    DEY,
    ASL(Operand),
    LSR(Operand),
    ROL(Operand),
    ROR(Operand),
    JMP(u16),
    JMPIndirect(u16),
    JSR(u16),
    RTS,
    BCC(u16),
    BCS(u16),
    BEQ(u16),
    BMI(u16),
    BNE(u16),
    BPL(u16),
    BVC(u16),
    BVS(u16),
    CLC,
    CLD,
    CLI,
    CLV,
    SEC,
    SED,
    SEI,
    BRK,
    NOP,
    RTI,
}

impl Instr {
    pub fn cycles(&self) -> u8 {
        match self {
            Instr::LDA(op) | Instr::LDX(op) | Instr::LDY(op) => op.memory_cycles(),
            Instr::STA(addr) | Instr::STX(addr) | Instr::STY(addr) => addr.memory_cycles(),
            Instr::TAX => 2,
            Instr::TAY => 2,
            Instr::TXA => 2,
            Instr::TYA => 2,
            Instr::TSX => 2,
            Instr::TXS => 2,
            Instr::PHA => 3,
            Instr::PHP => 3,
            Instr::PLA => 4,
            Instr::PLP => 4,
            Instr::AND(op) | Instr::EOR(op) | Instr::ORA(op) => op.memory_cycles(),
            Instr::BIT(_) => todo!(),
            Instr::ADC(_) => todo!(),
            Instr::SBC(_) => todo!(),
            Instr::CMP(_) => todo!(),
            Instr::CPX(_) => todo!(),
            Instr::CPY(_) => todo!(),
            Instr::INC(_) => todo!(),
            Instr::INX => 2,
            Instr::INY => 2,
            Instr::DEC(_) => todo!(),
            Instr::DEX => 2,
            Instr::DEY => 2,
            Instr::ASL(op) | Instr::LSR(op) | Instr::ROL(op) | Instr::ROR(op) => op.shift_cycles(),
            Instr::JMP(_) => 3,
            Instr::JMPIndirect(_) => 5,
            Instr::JSR(_) => 6,
            Instr::RTS => 6,
            Instr::BCC(_) => todo!(),
            Instr::BCS(_) => todo!(),
            Instr::BEQ(_) => todo!(),
            Instr::BMI(_) => todo!(),
            Instr::BNE(_) => todo!(),
            Instr::BPL(_) => todo!(),
            Instr::BVC(_) => todo!(),
            Instr::BVS(_) => todo!(),
            Instr::CLC => 2,
            Instr::CLD => 2,
            Instr::CLI => 2,
            Instr::CLV => 2,
            Instr::SEC => 2,
            Instr::SED => 2,
            Instr::SEI => 2,
            Instr::BRK => 7,
            Instr::NOP => 2,
            Instr::RTI => 6,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Addressing {
    ZeroPage(u8),
    ZeroPageX(u8),
    ZeroPageY(u8),
    Absolute(u16),
    AbsoluteX(u16),
    AbsoluteY(u16),
    IndexedIndirect(u8),
    IndirectIndexed(u8),
}

impl Addressing {
    fn memory_cycles(self) -> u8 {
        match self {
            Addressing::ZeroPage(_) => 3,
            Addressing::ZeroPageX(_) => 4,
            Addressing::ZeroPageY(_) => 4,
            Addressing::Absolute(_) => 4,
            Addressing::AbsoluteX(_) => 4,
            Addressing::AbsoluteY(_) => 4,
            Addressing::IndexedIndirect(_) => 6,
            Addressing::IndirectIndexed(_) => 5,
        }
    }

    fn shift_cycles(self) -> u8 {
        match self {
            Addressing::ZeroPage(_) => 5,
            Addressing::ZeroPageX(_) => 6,
            Addressing::ZeroPageY(_) => 6,
            Addressing::Absolute(_) => 6,
            Addressing::AbsoluteX(_) => 7,
            Addressing::AbsoluteY(_) => 7,
            Addressing::IndexedIndirect(_) | Addressing::IndirectIndexed(_) => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Operand {
    Accumulator,
    Immediate(u8),
    Addressing(Addressing),
}

impl Operand {
    fn memory_cycles(self) -> u8 {
        match self {
            Operand::Accumulator => unreachable!(),
            Operand::Immediate(_) => 2,
            Operand::Addressing(addr) => addr.memory_cycles(),
        }
    }

    fn shift_cycles(self) -> u8 {
        match self {
            Operand::Accumulator => 2,
            Operand::Immediate(_) => unreachable!(),
            Operand::Addressing(addr) => addr.shift_cycles(),
        }
    }
}

impl<'a> Cpu<'a> {
    // http://www.6502.org/users/obelisk/6502/reference.html
    pub fn read_instruction(&self, pc: &mut Wrapping<u16>) -> Option<Instr> {
        struct Reader<'a, 'b>(&'a Memory<'b>, &'a mut Wrapping<u16>);

        impl<'a, 'b> Reader<'a, 'b> {
            pub fn next_u8(&mut self) -> u8 {
                let res = self.0.read_u8(self.1 .0);
                *self.1 += 1;
                return res;
            }

            pub fn next_u16(&mut self) -> u16 {
                let res = self.0.read_u16(self.1 .0);
                *self.1 += 2;
                return res;
            }
        }

        let mut reader = Reader(&self.memory, pc);
        let opcode = reader.next_u8();

        return Some(match opcode {
            // ADC
            0x69 => Instr::ADC(Operand::Immediate(reader.next_u8())),
            0x65 => Instr::ADC(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()))),
            0x75 => Instr::ADC(Operand::Addressing(Addressing::ZeroPageX(reader.next_u8()))),
            0x6D => Instr::ADC(Operand::Addressing(Addressing::Absolute(reader.next_u16()))),
            0x7D => Instr::ADC(Operand::Addressing(Addressing::AbsoluteX(
                reader.next_u16(),
            ))),
            0x79 => Instr::ADC(Operand::Addressing(Addressing::AbsoluteY(
                reader.next_u16(),
            ))),
            0x61 => Instr::ADC(Operand::Addressing(Addressing::IndexedIndirect(
                reader.next_u8(),
            ))),
            0x71 => Instr::ADC(Operand::Addressing(Addressing::IndirectIndexed(
                reader.next_u8(),
            ))),
            // AND
            0x29 => Instr::AND(Operand::Immediate(reader.next_u8())),
            0x25 => Instr::AND(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()))),
            0x35 => Instr::AND(Operand::Addressing(Addressing::ZeroPageX(reader.next_u8()))),
            0x2D => Instr::AND(Operand::Addressing(Addressing::Absolute(reader.next_u16()))),
            0x3D => Instr::AND(Operand::Addressing(Addressing::AbsoluteX(
                reader.next_u16(),
            ))),
            0x39 => Instr::AND(Operand::Addressing(Addressing::AbsoluteY(
                reader.next_u16(),
            ))),
            0x21 => Instr::AND(Operand::Addressing(Addressing::IndexedIndirect(
                reader.next_u8(),
            ))),
            0x31 => Instr::AND(Operand::Addressing(Addressing::IndirectIndexed(
                reader.next_u8(),
            ))),
            _ => return None,
        });
    }
}

#[inline(always)]
pub fn page_crossed(addr1: u16, addr2: u16) -> bool {
    // return (addr1 & !0xff) != (addr2 & !0xff);
    return (addr2 ^ addr1) > 0xff;
}
