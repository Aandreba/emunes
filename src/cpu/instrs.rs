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
