use super::Backend;
use crate::cpu::{
    bcd::{bcd_to_u8, u8_to_bcd},
    flags::{Flag, Flags},
    instrs::{page_crossed, Instr, Operand},
    memory::Memory,
    Cpu,
};

pub struct Interpreter;

impl Backend for Interpreter {
    fn run<M: Memory>(
        cpu: &mut Cpu<M, Self>,
        mut pc: u16,
        mut tick: impl FnMut(&mut Cpu<M, Self>, u8),
    ) -> Result<(), M::Error> {
        let mut prev_cycles = 0;

        loop {
            let prev_pc = pc;
            tick(cpu, prev_cycles);

            let instr = cpu
                .read_instruction(&mut pc)?
                .expect(&format!("unknown instruction found at 0x{prev_pc:04X}"));

            prev_cycles = instr.cycles();

            log::trace!("{prev_pc:04X}: {instr:04X?}");

            match instr {
                Instr::LDA(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    cpu.accumulator = op;
                    cpu.flags.set_nz(cpu.accumulator);

                    if page_crossed {
                        tick(cpu, 1);
                    }
                }
                Instr::LDX(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    cpu.x = op;
                    cpu.flags.set_nz(cpu.x);

                    if page_crossed {
                        tick(cpu, 1);
                    }
                }
                Instr::LDY(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    cpu.y = op;
                    cpu.flags.set_nz(cpu.y);

                    if page_crossed {
                        tick(cpu, 1);
                    }
                }
                Instr::STA(addr) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    cpu.memory.write_u8(addr, cpu.accumulator)?;
                }
                Instr::STX(addr) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    cpu.memory.write_u8(addr, cpu.x)?;
                }
                Instr::STY(addr) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    cpu.memory.write_u8(addr, cpu.y)?;
                }
                Instr::TAX => {
                    cpu.x = cpu.accumulator;
                    cpu.flags.set_nz(cpu.accumulator);
                }
                Instr::TAY => {
                    cpu.y = cpu.accumulator;
                    cpu.flags.set_nz(cpu.accumulator);
                }
                Instr::TXA => {
                    cpu.accumulator = cpu.x;
                    cpu.flags.set_nz(cpu.x);
                }
                Instr::TYA => {
                    cpu.accumulator = cpu.y;
                    cpu.flags.set_nz(cpu.y);
                }
                Instr::TSX => {
                    cpu.x = cpu.stack_ptr;
                    cpu.flags.set_nz(cpu.x)
                }
                Instr::TXS => cpu.stack_ptr = cpu.x,
                Instr::PHA => cpu.push(cpu.accumulator)?,
                Instr::PHP => cpu.push(cpu.flags.into_u8(false))?,
                Instr::PLA => {
                    cpu.accumulator = cpu.pop()?;
                    cpu.flags.set_nz(cpu.accumulator)
                }
                Instr::PLP => cpu.flags = Flags::from_u8(cpu.pop()?),
                Instr::AND(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    cpu.accumulator &= op;
                    cpu.flags.set_nz(cpu.accumulator);

                    if page_crossed {
                        tick(cpu, 1);
                    }
                }
                Instr::EOR(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    cpu.accumulator ^= op;
                    cpu.flags.set_nz(cpu.accumulator);

                    if page_crossed {
                        tick(cpu, 1);
                    }
                }
                Instr::ORA(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    cpu.accumulator |= op;
                    cpu.flags.set_nz(cpu.accumulator);

                    if page_crossed {
                        tick(cpu, 1);
                    }
                }
                Instr::BIT(addr) => {
                    let op = cpu.memory.read_u8(cpu.get_addressing(addr)?.0)?;
                    cpu.flags.set(Flag::Zero, (cpu.accumulator & op) == 0);
                    cpu.flags.set(Flag::Negative, (op as i8).is_negative());
                    cpu.flags.set(Flag::Overflow, (op >> 6) & 1 == 1);
                }
                // https://github.com/kromych/yamos6502/blob/main/src/yamos6502.rs#L578
                Instr::ADC(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    let carry = cpu.flags.contains(Flag::Carry);
                    let decimal = cpu.flags.contains(Flag::Decimal);

                    let res = if decimal && cpu.decimal_enabled {
                        // Decimal
                        let mut res = bcd_to_u8(cpu.accumulator)
                            .wrapping_add(bcd_to_u8(op))
                            .wrapping_add(carry as u8);

                        if res > 99 {
                            res -= 100;
                            cpu.flags.insert(Flag::Carry);
                        } else {
                            cpu.flags.remove(Flag::Carry);
                        }

                        u8_to_bcd(res) as u16
                    } else {
                        // Binary
                        let op = op as u16;
                        let acc = cpu.accumulator as u16;
                        let res = acc.wrapping_add(op).wrapping_add(carry as u16);

                        cpu.flags
                            .set(Flag::Overflow, (acc ^ res) & (op ^ res) & 0x0080 != 0);
                        cpu.flags.set(Flag::Carry, res & 0xff00 != 0);
                        res
                    };

                    cpu.accumulator = res as u8;
                    cpu.flags.set(Flag::Zero, res & 0xff == 0);
                    cpu.flags.set(Flag::Negative, res & 0x80 != 0);

                    if page_crossed {
                        tick(cpu, 1)
                    }
                }
                // https://github.com/kromych/yamos6502/blob/main/src/yamos6502.rs#L628
                Instr::SBC(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    let borrow = !cpu.flags.contains(Flag::Carry);
                    let decimal = cpu.flags.contains(Flag::Decimal);

                    let res = if decimal && cpu.decimal_enabled {
                        // Decimal
                        let mut res = bcd_to_u8(cpu.accumulator)
                            .wrapping_sub(bcd_to_u8(op))
                            .wrapping_sub(borrow as u8) as i8;

                        if res.is_negative() {
                            res += 100;
                            cpu.flags.remove(Flag::Carry);
                        } else {
                            cpu.flags.insert(Flag::Carry);
                        }

                        u8_to_bcd(res as u8) as u16
                    } else {
                        // Binary
                        let op = op as u16;
                        let acc = cpu.accumulator as u16;
                        let res = acc.wrapping_sub(op).wrapping_sub(borrow as u16);

                        cpu.flags
                            .set(Flag::Overflow, (acc ^ res) & (!op ^ res) & 0x0080 != 0);
                        cpu.flags.set(Flag::Carry, res & 0xff00 == 0);
                        res
                    };

                    cpu.accumulator = res as u8;
                    cpu.flags.set(Flag::Zero, res & 0xff == 0);
                    cpu.flags.set(Flag::Negative, res & 0x80 != 0);

                    if page_crossed {
                        tick(cpu, 1)
                    }
                }
                Instr::CMP(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    cpu.flags.set(Flag::Carry, cpu.accumulator >= op);
                    cpu.flags.set(Flag::Zero, cpu.accumulator == op);
                    cpu.flags.set(Flag::Negative, cpu.accumulator < op);

                    if page_crossed {
                        tick(cpu, 1)
                    }
                }
                Instr::CPX(op) => {
                    let (op, _) = cpu.get_operand(op)?;
                    cpu.flags.set(Flag::Carry, cpu.x >= op);
                    cpu.flags.set(Flag::Zero, cpu.x == op);
                    cpu.flags.set(Flag::Negative, cpu.x < op);
                }
                Instr::CPY(op) => {
                    let (op, _) = cpu.get_operand(op)?;
                    cpu.flags.set(Flag::Carry, cpu.y >= op);
                    cpu.flags.set(Flag::Zero, cpu.y == op);
                    cpu.flags.set(Flag::Negative, cpu.y < op);
                }
                Instr::INC(addr) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    let res = cpu.memory.read_u8(addr)?.wrapping_add(1);
                    cpu.memory.write_u8(addr, res)?;
                    cpu.flags.set_nz(res)
                }
                Instr::INX => {
                    cpu.x = cpu.x.wrapping_add(1);
                    cpu.flags.set_nz(cpu.x);
                }
                Instr::INY => {
                    cpu.y = cpu.y.wrapping_add(1);
                    cpu.flags.set_nz(cpu.y);
                }
                Instr::DEC(addr) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    let res = cpu.memory.read_u8(addr)?.wrapping_sub(1);
                    cpu.memory.write_u8(addr, res)?;
                    cpu.flags.set_nz(res)
                }
                Instr::DEX => {
                    cpu.x = cpu.x.wrapping_sub(1);
                    cpu.flags.set_nz(cpu.x);
                }
                Instr::DEY => {
                    cpu.y = cpu.y.wrapping_sub(1);
                    cpu.flags.set_nz(cpu.y);
                }
                Instr::ASL(Operand::Accumulator) => {
                    cpu.flags
                        .set(Flag::Carry, (cpu.accumulator as i8).is_negative());
                    cpu.accumulator = cpu.accumulator.wrapping_shl(1);
                    cpu.flags.set_nz(cpu.accumulator);
                }
                Instr::ASL(Operand::Addressing(addr)) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    let op = cpu.memory.read_u8(addr)?;
                    let res = op.wrapping_shl(1);
                    cpu.memory.write_u8(addr, res)?;

                    cpu.flags.set(Flag::Carry, (op as i8).is_negative());
                    cpu.flags.set_nz(res);
                }
                Instr::LSR(Operand::Accumulator) => {
                    cpu.flags.set(Flag::Carry, cpu.accumulator & 1 == 1);
                    cpu.accumulator = cpu.accumulator.wrapping_shr(1);
                    cpu.flags.set_nz(cpu.accumulator);
                }
                Instr::LSR(Operand::Addressing(addr)) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    let op = cpu.memory.read_u8(addr)?;
                    let res = op.wrapping_shr(1);
                    cpu.memory.write_u8(addr, res)?;

                    cpu.flags.set(Flag::Carry, op & 1 == 1);
                    cpu.flags.set_nz(res);
                }
                // https://github.com/kromych/yamos6502/blob/main/src/yamos6502.rs#L491
                Instr::ROL(Operand::Accumulator) => {
                    let prev_acc = cpu.accumulator;

                    cpu.accumulator = cpu.accumulator.wrapping_shl(1);
                    cpu.accumulator |= cpu.flags.contains(Flag::Carry) as u8;

                    cpu.flags.set(Flag::Carry, (prev_acc as i8).is_negative());
                    cpu.flags.set_nz(cpu.accumulator);
                }
                Instr::ROL(Operand::Addressing(addr)) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    let op = cpu.memory.read_u8(addr)?;

                    let mut res = op.wrapping_shl(1);
                    res |= cpu.flags.contains(Flag::Carry) as u8;

                    cpu.memory.write_u8(addr, res)?;
                    cpu.flags.set(Flag::Carry, (op as i8).is_negative());
                    cpu.flags.set_nz(res);
                }
                Instr::ROR(Operand::Accumulator) => {
                    let prev_acc = cpu.accumulator;

                    cpu.accumulator = cpu.accumulator.wrapping_shr(1);
                    if cpu.flags.contains(Flag::Carry) {
                        cpu.accumulator |= 1 << 7;
                    }

                    cpu.flags.set(Flag::Carry, prev_acc & 1 == 1);
                    cpu.flags.set_nz(cpu.accumulator);
                }
                Instr::ROR(Operand::Addressing(addr)) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    let op = cpu.memory.read_u8(addr)?;

                    let mut res = op.wrapping_shr(1);
                    if cpu.flags.contains(Flag::Carry) {
                        res |= 1 << 7;
                    }

                    cpu.memory.write_u8(addr, res)?;
                    cpu.flags.set(Flag::Carry, op & 1 == 1);
                    cpu.flags.set_nz(res);
                }
                Instr::ASL(Operand::Immediate(_))
                | Instr::LSR(Operand::Immediate(_))
                | Instr::ROL(Operand::Immediate(_))
                | Instr::ROR(Operand::Immediate(_)) => unreachable!(),
                Instr::JMP(addr) => pc = addr,
                Instr::JMPIndirect(base) => pc = cpu.memory.read_u16(base)?,
                Instr::JSR(addr) => {
                    cpu.push_u16(pc.wrapping_sub(1))?;
                    pc = addr
                }
                Instr::BCC(addr) => {
                    if !cpu.flags.contains(Flag::Carry) {
                        tick(cpu, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BCS(addr) => {
                    if cpu.flags.contains(Flag::Carry) {
                        tick(cpu, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BEQ(addr) => {
                    if cpu.flags.contains(Flag::Zero) {
                        tick(cpu, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BMI(addr) => {
                    if cpu.flags.contains(Flag::Negative) {
                        tick(cpu, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BNE(addr) => {
                    if !cpu.flags.contains(Flag::Zero) {
                        #[cfg(debug_assertions)]
                        if prev_pc == addr {
                            return Ok(());
                        }

                        tick(cpu, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BPL(addr) => {
                    if !cpu.flags.contains(Flag::Negative) {
                        tick(cpu, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BVC(addr) => {
                    if !cpu.flags.contains(Flag::Overflow) {
                        tick(cpu, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BVS(addr) => {
                    if cpu.flags.contains(Flag::Overflow) {
                        tick(cpu, 1 + 2 * page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::CLC => cpu.flags.remove(Flag::Carry),
                Instr::CLD => cpu.flags.remove(Flag::Decimal),
                Instr::CLI => cpu.flags.remove(Flag::InterruptDisable),
                Instr::CLV => cpu.flags.remove(Flag::Overflow),
                Instr::SEC => cpu.flags.insert(Flag::Carry),
                Instr::SED => cpu.flags.insert(Flag::Decimal),
                Instr::SEI => cpu.flags.insert(Flag::InterruptDisable),
                // https://github.com/kromych/yamos6502/blob/main/src/yamos6502.rs#L698
                Instr::BRK => {
                    cpu.push_u16(pc.wrapping_add(1))?;
                    cpu.push(cpu.flags.into_u8(false))?;
                    cpu.flags.insert(Flag::InterruptDisable);
                    pc = cpu.memory.read_u16(0xfffe)?;
                }
                Instr::NOP => {}
                Instr::RTS => {
                    pc = cpu.pop_u16()?.wrapping_add(1);
                }
                Instr::RTI => {
                    cpu.flags = Flags::from_u8(cpu.pop()?);
                    pc = cpu.pop_u16()?;
                }
            }
        }
    }
}
