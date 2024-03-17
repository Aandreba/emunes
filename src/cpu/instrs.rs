use crate::cpu::memory::Memory;

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
    BIT(Addressing),
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
    pub fn cycles(self) -> u8 {
        match self {
            Instr::ADC(op)
            | Instr::SBC(op)
            | Instr::CMP(op)
            | Instr::LDA(op)
            | Instr::LDX(op)
            | Instr::LDY(op)
            | Instr::CPX(op)
            | Instr::CPY(op) => op.memory_cycles(),
            Instr::STA(Addressing::AbsoluteX(_) | Addressing::AbsoluteY(_)) => 5,
            Instr::STA(Addressing::IndirectIndexed(_)) => 6,
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
            Instr::BIT(Addressing::ZeroPage(_)) => 3,
            Instr::BIT(Addressing::Absolute(_)) => 4,
            Instr::BIT(_) => unreachable!(),
            Instr::INC(Addressing::ZeroPage(_)) | Instr::DEC(Addressing::ZeroPage(_)) => 5,
            Instr::INC(Addressing::ZeroPageX(_)) | Instr::DEC(Addressing::ZeroPageX(_)) => 6,
            Instr::INC(Addressing::Absolute(_)) | Instr::DEC(Addressing::Absolute(_)) => 6,
            Instr::INC(Addressing::AbsoluteX(_)) | Instr::DEC(Addressing::AbsoluteX(_)) => 7,
            Instr::INC(_) | Instr::DEC(_) => unreachable!(),
            Instr::INX => 2,
            Instr::INY => 2,
            Instr::DEX => 2,
            Instr::DEY => 2,
            Instr::ASL(op) | Instr::LSR(op) | Instr::ROL(op) | Instr::ROR(op) => op.shift_cycles(),
            Instr::JMP(_) => 3,
            Instr::JMPIndirect(_) => 5,
            Instr::JSR(_) => 6,
            Instr::RTS => 6,
            Instr::BCC(_)
            | Instr::BCS(_)
            | Instr::BEQ(_)
            | Instr::BMI(_)
            | Instr::BNE(_)
            | Instr::BPL(_)
            | Instr::BVC(_)
            | Instr::BVS(_) => 2,
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

// http://www.6502.org/users/obelisk/6502/reference.html
pub fn read_instruction<M: Memory>(this: &mut M, pc: &mut u16) -> Result<Option<Instr>, M::Error> {
    struct Reader<'a, M>(&'a mut M, &'a mut u16);

    impl<'a, M: Memory> Reader<'a, M> {
        pub fn next_u8(&mut self) -> Result<u8, M::Error> {
            let res = self.0.read_u8(*self.1)?;
            *self.1 = self.1.wrapping_add(1);
            return Ok(res);
        }

        pub fn next_u16(&mut self) -> Result<u16, M::Error> {
            let res = self.0.read_u16(*self.1)?;
            *self.1 = self.1.wrapping_add(2);
            return Ok(res);
        }

        pub fn next_relative(&mut self) -> Result<u16, M::Error> {
            let delta = self.next_u8()? as i8;
            return Ok(self.1.wrapping_add_signed(delta as i16));
        }
    }

    let mut reader = Reader(this, pc);
    let opcode = reader.next_u8()?;

    return Ok(Some(match opcode {
        // ADC
        0x69 => Instr::ADC(Operand::Immediate(reader.next_u8()?)),
        0x65 => Instr::ADC(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0x75 => Instr::ADC(Operand::Addressing(Addressing::ZeroPageX(
            reader.next_u8()?,
        ))),
        0x6D => Instr::ADC(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        0x7D => Instr::ADC(Operand::Addressing(Addressing::AbsoluteX(
            reader.next_u16()?,
        ))),
        0x79 => Instr::ADC(Operand::Addressing(Addressing::AbsoluteY(
            reader.next_u16()?,
        ))),
        0x61 => Instr::ADC(Operand::Addressing(Addressing::IndexedIndirect(
            reader.next_u8()?,
        ))),
        0x71 => Instr::ADC(Operand::Addressing(Addressing::IndirectIndexed(
            reader.next_u8()?,
        ))),
        // AND
        0x29 => Instr::AND(Operand::Immediate(reader.next_u8()?)),
        0x25 => Instr::AND(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0x35 => Instr::AND(Operand::Addressing(Addressing::ZeroPageX(
            reader.next_u8()?,
        ))),
        0x2D => Instr::AND(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        0x3D => Instr::AND(Operand::Addressing(Addressing::AbsoluteX(
            reader.next_u16()?,
        ))),
        0x39 => Instr::AND(Operand::Addressing(Addressing::AbsoluteY(
            reader.next_u16()?,
        ))),
        0x21 => Instr::AND(Operand::Addressing(Addressing::IndexedIndirect(
            reader.next_u8()?,
        ))),
        0x31 => Instr::AND(Operand::Addressing(Addressing::IndirectIndexed(
            reader.next_u8()?,
        ))),
        // ASL
        0x0A => Instr::ASL(Operand::Accumulator),
        0x06 => Instr::ASL(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0x16 => Instr::ASL(Operand::Addressing(Addressing::ZeroPageX(
            reader.next_u8()?,
        ))),
        0x0E => Instr::ASL(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        0x1E => Instr::ASL(Operand::Addressing(Addressing::AbsoluteX(
            reader.next_u16()?,
        ))),
        // Branching
        0x90 => Instr::BCC(reader.next_relative()?),
        0xB0 => Instr::BCS(reader.next_relative()?),
        0xF0 => Instr::BEQ(reader.next_relative()?),
        // BIT
        0x24 => Instr::BIT(Addressing::ZeroPage(reader.next_u8()?)),
        0x2c => Instr::BIT(Addressing::Absolute(reader.next_u16()?)),
        // Branching
        0x30 => Instr::BMI(reader.next_relative()?),
        0xd0 => Instr::BNE(reader.next_relative()?),
        0x10 => Instr::BPL(reader.next_relative()?),
        // BRK
        0x00 => Instr::BRK,
        // Branching
        0x50 => Instr::BVC(reader.next_relative()?),
        0x70 => Instr::BVS(reader.next_relative()?),
        // Clear flags
        0x18 => Instr::CLC,
        0xd8 => Instr::CLD,
        0x58 => Instr::CLI,
        0xb8 => Instr::CLV,
        // CMP
        0xC9 => Instr::CMP(Operand::Immediate(reader.next_u8()?)),
        0xC5 => Instr::CMP(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0xD5 => Instr::CMP(Operand::Addressing(Addressing::ZeroPageX(
            reader.next_u8()?,
        ))),
        0xCD => Instr::CMP(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        0xDD => Instr::CMP(Operand::Addressing(Addressing::AbsoluteX(
            reader.next_u16()?,
        ))),
        0xD9 => Instr::CMP(Operand::Addressing(Addressing::AbsoluteY(
            reader.next_u16()?,
        ))),
        0xC1 => Instr::CMP(Operand::Addressing(Addressing::IndexedIndirect(
            reader.next_u8()?,
        ))),
        0xD1 => Instr::CMP(Operand::Addressing(Addressing::IndirectIndexed(
            reader.next_u8()?,
        ))),
        // CPX
        0xE0 => Instr::CPX(Operand::Immediate(reader.next_u8()?)),
        0xE4 => Instr::CPX(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0xEc => Instr::CPX(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        // CPY
        0xC0 => Instr::CPY(Operand::Immediate(reader.next_u8()?)),
        0xC4 => Instr::CPY(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0xCc => Instr::CPY(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        // DEC
        0xc6 => Instr::DEC(Addressing::ZeroPage(reader.next_u8()?)),
        0xd6 => Instr::DEC(Addressing::ZeroPageX(reader.next_u8()?)),
        0xce => Instr::DEC(Addressing::Absolute(reader.next_u16()?)),
        0xde => Instr::DEC(Addressing::AbsoluteX(reader.next_u16()?)),
        // DEX
        0xca => Instr::DEX,
        // DEY
        0x88 => Instr::DEY,
        // EOR
        0x49 => Instr::EOR(Operand::Immediate(reader.next_u8()?)),
        0x45 => Instr::EOR(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0x55 => Instr::EOR(Operand::Addressing(Addressing::ZeroPageX(
            reader.next_u8()?,
        ))),
        0x4D => Instr::EOR(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        0x5D => Instr::EOR(Operand::Addressing(Addressing::AbsoluteX(
            reader.next_u16()?,
        ))),
        0x59 => Instr::EOR(Operand::Addressing(Addressing::AbsoluteY(
            reader.next_u16()?,
        ))),
        0x41 => Instr::EOR(Operand::Addressing(Addressing::IndexedIndirect(
            reader.next_u8()?,
        ))),
        0x51 => Instr::EOR(Operand::Addressing(Addressing::IndirectIndexed(
            reader.next_u8()?,
        ))),
        // INC
        0xe6 => Instr::INC(Addressing::ZeroPage(reader.next_u8()?)),
        0xf6 => Instr::INC(Addressing::ZeroPageX(reader.next_u8()?)),
        0xee => Instr::INC(Addressing::Absolute(reader.next_u16()?)),
        0xfe => Instr::INC(Addressing::AbsoluteX(reader.next_u16()?)),
        // INX
        0xe8 => Instr::INX,
        // INY
        0xc8 => Instr::INY,
        // JMP
        0x4c => Instr::JMP(reader.next_u16()?),
        0x6c => Instr::JMPIndirect(reader.next_u16()?),
        // JSR
        0x20 => Instr::JSR(reader.next_u16()?),
        // LDA
        0xa9 => Instr::LDA(Operand::Immediate(reader.next_u8()?)),
        0xa5 => Instr::LDA(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0xb5 => Instr::LDA(Operand::Addressing(Addressing::ZeroPageX(
            reader.next_u8()?,
        ))),
        0xaD => Instr::LDA(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        0xbD => Instr::LDA(Operand::Addressing(Addressing::AbsoluteX(
            reader.next_u16()?,
        ))),
        0xb9 => Instr::LDA(Operand::Addressing(Addressing::AbsoluteY(
            reader.next_u16()?,
        ))),
        0xa1 => Instr::LDA(Operand::Addressing(Addressing::IndexedIndirect(
            reader.next_u8()?,
        ))),
        0xb1 => Instr::LDA(Operand::Addressing(Addressing::IndirectIndexed(
            reader.next_u8()?,
        ))),
        // LDX
        0xa2 => Instr::LDX(Operand::Immediate(reader.next_u8()?)),
        0xa6 => Instr::LDX(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0xb6 => Instr::LDX(Operand::Addressing(Addressing::ZeroPageY(
            reader.next_u8()?,
        ))),
        0xae => Instr::LDX(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        0xbe => Instr::LDX(Operand::Addressing(Addressing::AbsoluteY(
            reader.next_u16()?,
        ))),
        // LDY
        0xa0 => Instr::LDY(Operand::Immediate(reader.next_u8()?)),
        0xa4 => Instr::LDY(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0xb4 => Instr::LDY(Operand::Addressing(Addressing::ZeroPageX(
            reader.next_u8()?,
        ))),
        0xac => Instr::LDY(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        0xbc => Instr::LDY(Operand::Addressing(Addressing::AbsoluteX(
            reader.next_u16()?,
        ))),
        // LSR
        0x4a => Instr::LSR(Operand::Accumulator),
        0x46 => Instr::LSR(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0x56 => Instr::LSR(Operand::Addressing(Addressing::ZeroPageX(
            reader.next_u8()?,
        ))),
        0x4e => Instr::LSR(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        0x5e => Instr::LSR(Operand::Addressing(Addressing::AbsoluteX(
            reader.next_u16()?,
        ))),
        // NOP
        0xea => Instr::NOP,
        // ORA
        0x09 => Instr::ORA(Operand::Immediate(reader.next_u8()?)),
        0x05 => Instr::ORA(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0x15 => Instr::ORA(Operand::Addressing(Addressing::ZeroPageX(
            reader.next_u8()?,
        ))),
        0x0D => Instr::ORA(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        0x1D => Instr::ORA(Operand::Addressing(Addressing::AbsoluteX(
            reader.next_u16()?,
        ))),
        0x19 => Instr::ORA(Operand::Addressing(Addressing::AbsoluteY(
            reader.next_u16()?,
        ))),
        0x01 => Instr::ORA(Operand::Addressing(Addressing::IndexedIndirect(
            reader.next_u8()?,
        ))),
        0x11 => Instr::ORA(Operand::Addressing(Addressing::IndirectIndexed(
            reader.next_u8()?,
        ))),
        // Stack Push/Pull
        0x48 => Instr::PHA,
        0x08 => Instr::PHP,
        0x68 => Instr::PLA,
        0x28 => Instr::PLP,
        // ROL
        0x2A => Instr::ROL(Operand::Accumulator),
        0x26 => Instr::ROL(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0x36 => Instr::ROL(Operand::Addressing(Addressing::ZeroPageX(
            reader.next_u8()?,
        ))),
        0x2E => Instr::ROL(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        0x3E => Instr::ROL(Operand::Addressing(Addressing::AbsoluteX(
            reader.next_u16()?,
        ))),
        // ROR
        0x6A => Instr::ROR(Operand::Accumulator),
        0x66 => Instr::ROR(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0x76 => Instr::ROR(Operand::Addressing(Addressing::ZeroPageX(
            reader.next_u8()?,
        ))),
        0x6E => Instr::ROR(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        0x7E => Instr::ROR(Operand::Addressing(Addressing::AbsoluteX(
            reader.next_u16()?,
        ))),
        // RTI
        0x40 => Instr::RTI,
        // RTS
        0x60 => Instr::RTS,
        // SBC
        0xE9 => Instr::SBC(Operand::Immediate(reader.next_u8()?)),
        0xE5 => Instr::SBC(Operand::Addressing(Addressing::ZeroPage(reader.next_u8()?))),
        0xF5 => Instr::SBC(Operand::Addressing(Addressing::ZeroPageX(
            reader.next_u8()?,
        ))),
        0xED => Instr::SBC(Operand::Addressing(Addressing::Absolute(
            reader.next_u16()?,
        ))),
        0xFD => Instr::SBC(Operand::Addressing(Addressing::AbsoluteX(
            reader.next_u16()?,
        ))),
        0xF9 => Instr::SBC(Operand::Addressing(Addressing::AbsoluteY(
            reader.next_u16()?,
        ))),
        0xE1 => Instr::SBC(Operand::Addressing(Addressing::IndexedIndirect(
            reader.next_u8()?,
        ))),
        0xF1 => Instr::SBC(Operand::Addressing(Addressing::IndirectIndexed(
            reader.next_u8()?,
        ))),
        // Set flags
        0x38 => Instr::SEC,
        0xf8 => Instr::SED,
        0x78 => Instr::SEI,
        // STA
        0x85 => Instr::STA(Addressing::ZeroPage(reader.next_u8()?)),
        0x95 => Instr::STA(Addressing::ZeroPageX(reader.next_u8()?)),
        0x8D => Instr::STA(Addressing::Absolute(reader.next_u16()?)),
        0x9D => Instr::STA(Addressing::AbsoluteX(reader.next_u16()?)),
        0x99 => Instr::STA(Addressing::AbsoluteY(reader.next_u16()?)),
        0x81 => Instr::STA(Addressing::IndexedIndirect(reader.next_u8()?)),
        0x91 => Instr::STA(Addressing::IndirectIndexed(reader.next_u8()?)),
        // STX
        0x86 => Instr::STX(Addressing::ZeroPage(reader.next_u8()?)),
        0x96 => Instr::STX(Addressing::ZeroPageY(reader.next_u8()?)),
        0x8e => Instr::STX(Addressing::Absolute(reader.next_u16()?)),
        // STY
        0x84 => Instr::STY(Addressing::ZeroPage(reader.next_u8()?)),
        0x94 => Instr::STY(Addressing::ZeroPageX(reader.next_u8()?)),
        0x8c => Instr::STY(Addressing::Absolute(reader.next_u16()?)),
        // Transfers
        0xaa => Instr::TAX,
        0xa8 => Instr::TAY,
        0xba => Instr::TSX,
        0x8a => Instr::TXA,
        0x9a => Instr::TXS,
        0x98 => Instr::TYA,
        _ => return Ok(None),
    }));
}

#[inline(always)]
pub fn page_crossed(addr1: u16, addr2: u16) -> bool {
    // return (addr1 & !0xff) != (addr2 & !0xff);
    return (addr2 ^ addr1) > 0xff;
}

#[test]
fn test_cycles() {
    use std::collections::HashMap;

    #[derive(Debug)]
    #[allow(non_camel_case_types)]
    pub enum AddressingMode {
        Immediate,
        ZeroPage,
        ZeroPage_X,
        ZeroPage_Y,
        Absolute,
        Absolute_X,
        Absolute_Y,
        Indirect_X,
        Indirect_Y,
        NoneAddressing,
    }

    #[derive(Debug)]
    pub struct OpCode {
        pub code: u8,
        pub mnemonic: &'static str,
        pub len: u8,
        pub cycles: u8,
        pub mode: AddressingMode,
    }

    impl OpCode {
        fn new(
            code: u8,
            mnemonic: &'static str,
            len: u8,
            cycles: u8,
            mode: AddressingMode,
        ) -> Self {
            OpCode {
                code: code,
                mnemonic: mnemonic,
                len: len,
                cycles: cycles,
                mode: mode,
            }
        }
    }

    let CPU_OPS_CODES: Vec<OpCode> = vec![
        OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing),
        OpCode::new(0xea, "NOP", 1, 2, AddressingMode::NoneAddressing),
        /* Arithmetic */
        OpCode::new(0x69, "ADC", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x65, "ADC", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x75, "ADC", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x6d, "ADC", 3, 4, AddressingMode::Absolute),
        OpCode::new(
            0x7d,
            "ADC",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_X,
        ),
        OpCode::new(
            0x79,
            "ADC",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_Y,
        ),
        OpCode::new(0x61, "ADC", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(
            0x71,
            "ADC",
            2,
            5, /*+1 if page crossed*/
            AddressingMode::Indirect_Y,
        ),
        OpCode::new(0xe9, "SBC", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xe5, "SBC", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xf5, "SBC", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xed, "SBC", 3, 4, AddressingMode::Absolute),
        OpCode::new(
            0xfd,
            "SBC",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_X,
        ),
        OpCode::new(
            0xf9,
            "SBC",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_Y,
        ),
        OpCode::new(0xe1, "SBC", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(
            0xf1,
            "SBC",
            2,
            5, /*+1 if page crossed*/
            AddressingMode::Indirect_Y,
        ),
        OpCode::new(0x29, "AND", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x25, "AND", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x35, "AND", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x2d, "AND", 3, 4, AddressingMode::Absolute),
        OpCode::new(
            0x3d,
            "AND",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_X,
        ),
        OpCode::new(
            0x39,
            "AND",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_Y,
        ),
        OpCode::new(0x21, "AND", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(
            0x31,
            "AND",
            2,
            5, /*+1 if page crossed*/
            AddressingMode::Indirect_Y,
        ),
        OpCode::new(0x49, "EOR", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x45, "EOR", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x55, "EOR", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x4d, "EOR", 3, 4, AddressingMode::Absolute),
        OpCode::new(
            0x5d,
            "EOR",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_X,
        ),
        OpCode::new(
            0x59,
            "EOR",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_Y,
        ),
        OpCode::new(0x41, "EOR", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(
            0x51,
            "EOR",
            2,
            5, /*+1 if page crossed*/
            AddressingMode::Indirect_Y,
        ),
        OpCode::new(0x09, "ORA", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x05, "ORA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x15, "ORA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x0d, "ORA", 3, 4, AddressingMode::Absolute),
        OpCode::new(
            0x1d,
            "ORA",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_X,
        ),
        OpCode::new(
            0x19,
            "ORA",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_Y,
        ),
        OpCode::new(0x01, "ORA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(
            0x11,
            "ORA",
            2,
            5, /*+1 if page crossed*/
            AddressingMode::Indirect_Y,
        ),
        /* Shifts */
        OpCode::new(0x0a, "ASL", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x06, "ASL", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x16, "ASL", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x0e, "ASL", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x1e, "ASL", 3, 7, AddressingMode::Absolute_X),
        OpCode::new(0x4a, "LSR", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x46, "LSR", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x56, "LSR", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x4e, "LSR", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x5e, "LSR", 3, 7, AddressingMode::Absolute_X),
        OpCode::new(0x2a, "ROL", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x26, "ROL", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x36, "ROL", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x2e, "ROL", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x3e, "ROL", 3, 7, AddressingMode::Absolute_X),
        OpCode::new(0x6a, "ROR", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x66, "ROR", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x76, "ROR", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x6e, "ROR", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x7e, "ROR", 3, 7, AddressingMode::Absolute_X),
        OpCode::new(0xe6, "INC", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0xf6, "INC", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0xee, "INC", 3, 6, AddressingMode::Absolute),
        OpCode::new(0xfe, "INC", 3, 7, AddressingMode::Absolute_X),
        OpCode::new(0xe8, "INX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xc8, "INY", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xc6, "DEC", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0xd6, "DEC", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0xce, "DEC", 3, 6, AddressingMode::Absolute),
        OpCode::new(0xde, "DEC", 3, 7, AddressingMode::Absolute_X),
        OpCode::new(0xca, "DEX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x88, "DEY", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xc9, "CMP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xc5, "CMP", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xd5, "CMP", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xcd, "CMP", 3, 4, AddressingMode::Absolute),
        OpCode::new(
            0xdd,
            "CMP",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_X,
        ),
        OpCode::new(
            0xd9,
            "CMP",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_Y,
        ),
        OpCode::new(0xc1, "CMP", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(
            0xd1,
            "CMP",
            2,
            5, /*+1 if page crossed*/
            AddressingMode::Indirect_Y,
        ),
        OpCode::new(0xc0, "CPY", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xc4, "CPY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xcc, "CPY", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xe0, "CPX", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xe4, "CPX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xec, "CPX", 3, 4, AddressingMode::Absolute),
        /* Branching */
        OpCode::new(0x4c, "JMP", 3, 3, AddressingMode::NoneAddressing), //AddressingMode that acts as Immidiate
        OpCode::new(0x6c, "JMP", 3, 5, AddressingMode::NoneAddressing), //AddressingMode:Indirect with 6502 bug
        OpCode::new(0x20, "JSR", 3, 6, AddressingMode::NoneAddressing),
        OpCode::new(0x60, "RTS", 1, 6, AddressingMode::NoneAddressing),
        OpCode::new(0x40, "RTI", 1, 6, AddressingMode::NoneAddressing),
        OpCode::new(
            0xd0,
            "BNE",
            2,
            2, /*(+1 if branch succeeds +2 if to a new page)*/
            AddressingMode::NoneAddressing,
        ),
        OpCode::new(
            0x70,
            "BVS",
            2,
            2, /*(+1 if branch succeeds +2 if to a new page)*/
            AddressingMode::NoneAddressing,
        ),
        OpCode::new(
            0x50,
            "BVC",
            2,
            2, /*(+1 if branch succeeds +2 if to a new page)*/
            AddressingMode::NoneAddressing,
        ),
        OpCode::new(
            0x30,
            "BMI",
            2,
            2, /*(+1 if branch succeeds +2 if to a new page)*/
            AddressingMode::NoneAddressing,
        ),
        OpCode::new(
            0xf0,
            "BEQ",
            2,
            2, /*(+1 if branch succeeds +2 if to a new page)*/
            AddressingMode::NoneAddressing,
        ),
        OpCode::new(
            0xb0,
            "BCS",
            2,
            2, /*(+1 if branch succeeds +2 if to a new page)*/
            AddressingMode::NoneAddressing,
        ),
        OpCode::new(
            0x90,
            "BCC",
            2,
            2, /*(+1 if branch succeeds +2 if to a new page)*/
            AddressingMode::NoneAddressing,
        ),
        OpCode::new(
            0x10,
            "BPL",
            2,
            2, /*(+1 if branch succeeds +2 if to a new page)*/
            AddressingMode::NoneAddressing,
        ),
        OpCode::new(0x24, "BIT", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x2c, "BIT", 3, 4, AddressingMode::Absolute),
        /* Stores, Loads */
        OpCode::new(0xa9, "LDA", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xa5, "LDA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xb5, "LDA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xad, "LDA", 3, 4, AddressingMode::Absolute),
        OpCode::new(
            0xbd,
            "LDA",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_X,
        ),
        OpCode::new(
            0xb9,
            "LDA",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_Y,
        ),
        OpCode::new(0xa1, "LDA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(
            0xb1,
            "LDA",
            2,
            5, /*+1 if page crossed*/
            AddressingMode::Indirect_Y,
        ),
        OpCode::new(0xa2, "LDX", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xa6, "LDX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xb6, "LDX", 2, 4, AddressingMode::ZeroPage_Y),
        OpCode::new(0xae, "LDX", 3, 4, AddressingMode::Absolute),
        OpCode::new(
            0xbe,
            "LDX",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_Y,
        ),
        OpCode::new(0xa0, "LDY", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xa4, "LDY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xb4, "LDY", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xac, "LDY", 3, 4, AddressingMode::Absolute),
        OpCode::new(
            0xbc,
            "LDY",
            3,
            4, /*+1 if page crossed*/
            AddressingMode::Absolute_X,
        ),
        OpCode::new(0x85, "STA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x8d, "STA", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x9d, "STA", 3, 5, AddressingMode::Absolute_X),
        OpCode::new(0x99, "STA", 3, 5, AddressingMode::Absolute_Y),
        OpCode::new(0x81, "STA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x91, "STA", 2, 6, AddressingMode::Indirect_Y),
        OpCode::new(0x86, "STX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x96, "STX", 2, 4, AddressingMode::ZeroPage_Y),
        OpCode::new(0x8e, "STX", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x84, "STY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x94, "STY", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x8c, "STY", 3, 4, AddressingMode::Absolute),
        /* Flags clear */
        OpCode::new(0xD8, "CLD", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x58, "CLI", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xb8, "CLV", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x18, "CLC", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x38, "SEC", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x78, "SEI", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xf8, "SED", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xaa, "TAX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xa8, "TAY", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xba, "TSX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x8a, "TXA", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x9a, "TXS", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x98, "TYA", 1, 2, AddressingMode::NoneAddressing),
        /* Stack */
        OpCode::new(0x48, "PHA", 1, 3, AddressingMode::NoneAddressing),
        OpCode::new(0x68, "PLA", 1, 4, AddressingMode::NoneAddressing),
        OpCode::new(0x08, "PHP", 1, 3, AddressingMode::NoneAddressing),
        OpCode::new(0x28, "PLP", 1, 4, AddressingMode::NoneAddressing),
        /* unofficial */
        OpCode::new(0xc7, "*DCP", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0xd7, "*DCP", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0xCF, "*DCP", 3, 6, AddressingMode::Absolute),
        OpCode::new(0xdF, "*DCP", 3, 7, AddressingMode::Absolute_X),
        OpCode::new(0xdb, "*DCP", 3, 7, AddressingMode::Absolute_Y),
        OpCode::new(0xd3, "*DCP", 2, 8, AddressingMode::Indirect_Y),
        OpCode::new(0xc3, "*DCP", 2, 8, AddressingMode::Indirect_X),
        OpCode::new(0x27, "*RLA", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x37, "*RLA", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x2F, "*RLA", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x3F, "*RLA", 3, 7, AddressingMode::Absolute_X),
        OpCode::new(0x3b, "*RLA", 3, 7, AddressingMode::Absolute_Y),
        OpCode::new(0x33, "*RLA", 2, 8, AddressingMode::Indirect_Y),
        OpCode::new(0x23, "*RLA", 2, 8, AddressingMode::Indirect_X),
        OpCode::new(0x07, "*SLO", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x17, "*SLO", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x0F, "*SLO", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x1f, "*SLO", 3, 7, AddressingMode::Absolute_X),
        OpCode::new(0x1b, "*SLO", 3, 7, AddressingMode::Absolute_Y),
        OpCode::new(0x03, "*SLO", 2, 8, AddressingMode::Indirect_X),
        OpCode::new(0x13, "*SLO", 2, 8, AddressingMode::Indirect_Y),
        OpCode::new(0x47, "*SRE", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x57, "*SRE", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x4F, "*SRE", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x5f, "*SRE", 3, 7, AddressingMode::Absolute_X),
        OpCode::new(0x5b, "*SRE", 3, 7, AddressingMode::Absolute_Y),
        OpCode::new(0x43, "*SRE", 2, 8, AddressingMode::Indirect_X),
        OpCode::new(0x53, "*SRE", 2, 8, AddressingMode::Indirect_Y),
        OpCode::new(0x80, "*NOP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x82, "*NOP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x89, "*NOP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xc2, "*NOP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xe2, "*NOP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xCB, "*AXS", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x6B, "*ARR", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xeb, "*SBC", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x0b, "*ANC", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x2b, "*ANC", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x4b, "*ALR", 2, 2, AddressingMode::Immediate),
        // OpCode::new(0xCB, "IGN", 3,4 /* or 5*/, AddressingMode::Absolute_X),
        OpCode::new(0x04, "*NOP", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x44, "*NOP", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x64, "*NOP", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x14, "*NOP", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x34, "*NOP", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x54, "*NOP", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x74, "*NOP", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xd4, "*NOP", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xf4, "*NOP", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x0c, "*NOP", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x1c, "*NOP", 3, 4 /*or 5*/, AddressingMode::Absolute_X),
        OpCode::new(0x3c, "*NOP", 3, 4 /*or 5*/, AddressingMode::Absolute_X),
        OpCode::new(0x5c, "*NOP", 3, 4 /*or 5*/, AddressingMode::Absolute_X),
        OpCode::new(0x7c, "*NOP", 3, 4 /*or 5*/, AddressingMode::Absolute_X),
        OpCode::new(
            0xdc,
            "*NOP",
            3,
            4, /* or 5*/
            AddressingMode::Absolute_X,
        ),
        OpCode::new(
            0xfc,
            "*NOP",
            3,
            4, /* or 5*/
            AddressingMode::Absolute_X,
        ),
        OpCode::new(0x67, "*RRA", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x77, "*RRA", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x6f, "*RRA", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x7f, "*RRA", 3, 7, AddressingMode::Absolute_X),
        OpCode::new(0x7b, "*RRA", 3, 7, AddressingMode::Absolute_Y),
        OpCode::new(0x63, "*RRA", 2, 8, AddressingMode::Indirect_X),
        OpCode::new(0x73, "*RRA", 2, 8, AddressingMode::Indirect_Y),
        OpCode::new(0xe7, "*ISB", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0xf7, "*ISB", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0xef, "*ISB", 3, 6, AddressingMode::Absolute),
        OpCode::new(0xff, "*ISB", 3, 7, AddressingMode::Absolute_X),
        OpCode::new(0xfb, "*ISB", 3, 7, AddressingMode::Absolute_Y),
        OpCode::new(0xe3, "*ISB", 2, 8, AddressingMode::Indirect_X),
        OpCode::new(0xf3, "*ISB", 2, 8, AddressingMode::Indirect_Y),
        OpCode::new(0x02, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x12, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x22, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x32, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x42, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x52, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x62, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x72, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x92, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xb2, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xd2, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xf2, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x1a, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x3a, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x5a, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x7a, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xda, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        // OpCode::new(0xea, "NOP", 1,2, AddressingMode::NoneAddressing),
        OpCode::new(0xfa, "*NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xab, "*LXA", 2, 3, AddressingMode::Immediate), //todo: highly unstable and not used
        //http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_%28XAA,_ANE%29
        OpCode::new(0x8b, "*XAA", 2, 3, AddressingMode::Immediate), //todo: highly unstable and not used
        OpCode::new(0xbb, "*LAS", 3, 2, AddressingMode::Absolute_Y), //todo: highly unstable and not used
        OpCode::new(0x9b, "*TAS", 3, 2, AddressingMode::Absolute_Y), //todo: highly unstable and not used
        OpCode::new(
            0x93,
            "*AHX",
            2,
            /* guess */ 8,
            AddressingMode::Indirect_Y,
        ), //todo: highly unstable and not used
        OpCode::new(
            0x9f,
            "*AHX",
            3,
            /* guess */ 4, /* or 5*/
            AddressingMode::Absolute_Y,
        ), //todo: highly unstable and not used
        OpCode::new(
            0x9e,
            "*SHX",
            3,
            /* guess */ 4, /* or 5*/
            AddressingMode::Absolute_Y,
        ), //todo: highly unstable and not used
        OpCode::new(
            0x9c,
            "*SHY",
            3,
            /* guess */ 4, /* or 5*/
            AddressingMode::Absolute_X,
        ), //todo: highly unstable and not used
        OpCode::new(0xa7, "*LAX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xb7, "*LAX", 2, 4, AddressingMode::ZeroPage_Y),
        OpCode::new(0xaf, "*LAX", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xbf, "*LAX", 3, 4, AddressingMode::Absolute_Y),
        OpCode::new(0xa3, "*LAX", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0xb3, "*LAX", 2, 5, AddressingMode::Indirect_Y),
        OpCode::new(0x87, "*SAX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x97, "*SAX", 2, 4, AddressingMode::ZeroPage_Y),
        OpCode::new(0x8f, "*SAX", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x83, "*SAX", 2, 6, AddressingMode::Indirect_X),
    ];

    let OPCODES_MAP: HashMap<u8, OpCode> = {
        let mut map = HashMap::new();
        for cpuop in CPU_OPS_CODES {
            map.insert(cpuop.code, cpuop);
        }
        map
    };

    let mut mem = crate::cpu::memory::create_linear_memory();
    mem[1] = 0;
    mem[2] = 0;

    let mut incorrect = Vec::with_capacity(255);
    for opcode in 0..=255 {
        let target = OPCODES_MAP.get(&opcode).unwrap();
        if target.mnemonic.starts_with('*') {
            continue;
        }

        mem[0] = opcode;
        let mut pc = 0;
        let instr = read_instruction(&mut mem, &mut pc).unwrap().unwrap();

        if target.cycles != instr.cycles() {
            incorrect.push(format!(
                "{instr:?} (expected {}, found {})",
                target.cycles,
                instr.cycles()
            ))
        }
    }

    for incorrect in &incorrect {
        println!("{incorrect}")
    }
    assert_eq!(incorrect.len(), 0);
}
