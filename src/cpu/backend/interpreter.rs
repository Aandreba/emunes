use super::Backend;
use crate::cpu::{
    bcd::{bcd_to_u8, u8_to_bcd},
    flags::{Flag, Flags},
    instrs::{page_crossed, read_instruction, Instr, Operand},
    memory::Memory,
    Cpu, RunError,
};
use std::convert::Infallible;

pub struct Interpreter;

impl Backend for Interpreter {
    type Error = Infallible;

    fn run<M: Memory>(
        cpu: &mut Cpu<M, Self>,
        mut pc: u16,
        mut tick: impl FnMut(&mut Cpu<M, Self>, u8),
    ) -> Result<(), RunError<M, Self>> {
        let mut prev_cycles = 0;

        loop {
            let prev_pc = pc;
            tick(cpu, prev_cycles);

            // Handle NMI interrupt
            if core::mem::replace(&mut cpu.state.nmi_interrupt, false) {
                log::debug!("NMI interrupt");
                cpu.push_u16(pc)?;
                cpu.push(cpu.state.flags.into_u8(true))?;
                cpu.state.flags.insert(Flag::InterruptDisable);
                // tick(cpu, 2);
                prev_cycles = 2;
                pc = cpu.memory.read_u16(0xfffa).map_err(RunError::Memory)?;
                continue;
            }

            let Some(instr) =
                read_instruction(&mut cpu.memory, &mut pc).map_err(RunError::Memory)?
            else {
                panic!(
                    "unknown instruction found at 0x{prev_pc:04X}: 0x{:02X}",
                    cpu.memory.read_u8(prev_pc).unwrap()
                )
            };

            prev_cycles = instr.cycles();
            log::trace!("{prev_pc:04X}: {instr:04X?}");

            match instr {
                Instr::LDA(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    cpu.state.accumulator = op;
                    cpu.state.flags.set_nz(cpu.state.accumulator);

                    if page_crossed {
                        tick(cpu, 1);
                    }
                }
                Instr::LDX(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    cpu.state.x = op;
                    cpu.state.flags.set_nz(cpu.state.x);

                    if page_crossed {
                        tick(cpu, 1);
                    }
                }
                Instr::LDY(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    cpu.state.y = op;
                    cpu.state.flags.set_nz(cpu.state.y);

                    if page_crossed {
                        tick(cpu, 1);
                    }
                }
                Instr::STA(addr) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    cpu.memory
                        .write_u8(addr, cpu.state.accumulator)
                        .map_err(RunError::Memory)?;
                }
                Instr::STX(addr) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    cpu.memory
                        .write_u8(addr, cpu.state.x)
                        .map_err(RunError::Memory)?;
                }
                Instr::STY(addr) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    cpu.memory
                        .write_u8(addr, cpu.state.y)
                        .map_err(RunError::Memory)?;
                }
                Instr::TAX => {
                    cpu.state.x = cpu.state.accumulator;
                    cpu.state.flags.set_nz(cpu.state.accumulator);
                }
                Instr::TAY => {
                    cpu.state.y = cpu.state.accumulator;
                    cpu.state.flags.set_nz(cpu.state.accumulator);
                }
                Instr::TXA => {
                    cpu.state.accumulator = cpu.state.x;
                    cpu.state.flags.set_nz(cpu.state.x);
                }
                Instr::TYA => {
                    cpu.state.accumulator = cpu.state.y;
                    cpu.state.flags.set_nz(cpu.state.y);
                }
                Instr::TSX => {
                    cpu.state.x = cpu.state.stack_ptr;
                    cpu.state.flags.set_nz(cpu.state.x)
                }
                Instr::TXS => cpu.state.stack_ptr = cpu.state.x,
                Instr::PHA => cpu.push(cpu.state.accumulator)?,
                Instr::PHP => cpu.push(cpu.state.flags.into_u8(false))?,
                Instr::PLA => {
                    cpu.state.accumulator = cpu.pop()?;
                    cpu.state.flags.set_nz(cpu.state.accumulator)
                }
                Instr::PLP => cpu.state.flags = Flags::from_u8(cpu.pop()?),
                Instr::AND(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    cpu.state.accumulator &= op;
                    cpu.state.flags.set_nz(cpu.state.accumulator);

                    if page_crossed {
                        tick(cpu, 1);
                    }
                }
                Instr::EOR(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    cpu.state.accumulator ^= op;
                    cpu.state.flags.set_nz(cpu.state.accumulator);

                    if page_crossed {
                        tick(cpu, 1);
                    }
                }
                Instr::ORA(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    cpu.state.accumulator |= op;
                    cpu.state.flags.set_nz(cpu.state.accumulator);

                    if page_crossed {
                        tick(cpu, 1);
                    }
                }
                Instr::BIT(addr) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    let op = cpu.memory.read_u8(addr).map_err(RunError::Memory)?;
                    cpu.state
                        .flags
                        .set(Flag::Zero, (cpu.state.accumulator & op) == 0);
                    cpu.state
                        .flags
                        .set(Flag::Negative, (op as i8).is_negative());
                    cpu.state.flags.set(Flag::Overflow, (op >> 6) & 1 == 1);
                }
                // https://github.com/kromych/yamos6502/blob/main/src/yamos6502.rs#L578
                Instr::ADC(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    let carry = cpu.state.flags.contains(Flag::Carry);
                    let decimal = cpu.state.flags.contains(Flag::Decimal);

                    let res = if decimal && cpu.state.decimal_enabled {
                        // Decimal
                        let mut res = bcd_to_u8(cpu.state.accumulator)
                            .wrapping_add(bcd_to_u8(op))
                            .wrapping_add(carry as u8);

                        if res > 99 {
                            res -= 100;
                            cpu.state.flags.insert(Flag::Carry);
                        } else {
                            cpu.state.flags.remove(Flag::Carry);
                        }

                        u8_to_bcd(res) as u16
                    } else {
                        // Binary
                        let op = op as u16;
                        let acc = cpu.state.accumulator as u16;
                        let res = acc.wrapping_add(op).wrapping_add(carry as u16);

                        cpu.state
                            .flags
                            .set(Flag::Overflow, (acc ^ res) & (op ^ res) & 0x0080 != 0);
                        cpu.state.flags.set(Flag::Carry, res & 0xff00 != 0);
                        res
                    };

                    cpu.state.accumulator = res as u8;
                    cpu.state.flags.set(Flag::Zero, res & 0xff == 0);
                    cpu.state.flags.set(Flag::Negative, res & 0x80 != 0);

                    if page_crossed {
                        tick(cpu, 1)
                    }
                }
                // https://github.com/kromych/yamos6502/blob/main/src/yamos6502.rs#L628
                Instr::SBC(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    let borrow = !cpu.state.flags.contains(Flag::Carry);
                    let decimal = cpu.state.flags.contains(Flag::Decimal);

                    let res = if decimal && cpu.state.decimal_enabled {
                        // Decimal
                        let mut res = bcd_to_u8(cpu.state.accumulator)
                            .wrapping_sub(bcd_to_u8(op))
                            .wrapping_sub(borrow as u8) as i8;

                        if res.is_negative() {
                            res += 100;
                            cpu.state.flags.remove(Flag::Carry);
                        } else {
                            cpu.state.flags.insert(Flag::Carry);
                        }

                        u8_to_bcd(res as u8) as u16
                    } else {
                        // Binary
                        let op = op as u16;
                        let acc = cpu.state.accumulator as u16;
                        let res = acc.wrapping_sub(op).wrapping_sub(borrow as u16);

                        cpu.state
                            .flags
                            .set(Flag::Overflow, (acc ^ res) & (!op ^ res) & 0x0080 != 0);
                        cpu.state.flags.set(Flag::Carry, res & 0xff00 == 0);
                        res
                    };

                    cpu.state.accumulator = res as u8;
                    cpu.state.flags.set(Flag::Zero, res & 0xff == 0);
                    cpu.state.flags.set(Flag::Negative, res & 0x80 != 0);

                    if page_crossed {
                        tick(cpu, 1)
                    }
                }
                Instr::CMP(op) => {
                    let (op, page_crossed) = cpu.get_operand(op)?;
                    cpu.state
                        .flags
                        .set(Flag::Carry, cpu.state.accumulator >= op);
                    cpu.state.flags.set(Flag::Zero, cpu.state.accumulator == op);
                    cpu.state
                        .flags
                        .set(Flag::Negative, cpu.state.accumulator < op);

                    if page_crossed {
                        tick(cpu, 1)
                    }
                }
                Instr::CPX(op) => {
                    let (op, _) = cpu.get_operand(op)?;
                    cpu.state.flags.set(Flag::Carry, cpu.state.x >= op);
                    cpu.state.flags.set(Flag::Zero, cpu.state.x == op);
                    cpu.state.flags.set(Flag::Negative, cpu.state.x < op);
                }
                Instr::CPY(op) => {
                    let (op, _) = cpu.get_operand(op)?;
                    cpu.state.flags.set(Flag::Carry, cpu.state.y >= op);
                    cpu.state.flags.set(Flag::Zero, cpu.state.y == op);
                    cpu.state.flags.set(Flag::Negative, cpu.state.y < op);
                }
                Instr::INC(addr) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    let res = cpu
                        .memory
                        .read_u8(addr)
                        .map_err(RunError::Memory)?
                        .wrapping_add(1);
                    cpu.memory.write_u8(addr, res).map_err(RunError::Memory)?;
                    cpu.state.flags.set_nz(res)
                }
                Instr::INX => {
                    cpu.state.x = cpu.state.x.wrapping_add(1);
                    cpu.state.flags.set_nz(cpu.state.x);
                }
                Instr::INY => {
                    cpu.state.y = cpu.state.y.wrapping_add(1);
                    cpu.state.flags.set_nz(cpu.state.y);
                }
                Instr::DEC(addr) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    let res = cpu
                        .memory
                        .read_u8(addr)
                        .map_err(RunError::Memory)?
                        .wrapping_sub(1);
                    cpu.memory.write_u8(addr, res).map_err(RunError::Memory)?;
                    cpu.state.flags.set_nz(res)
                }
                Instr::DEX => {
                    cpu.state.x = cpu.state.x.wrapping_sub(1);
                    cpu.state.flags.set_nz(cpu.state.x);
                }
                Instr::DEY => {
                    cpu.state.y = cpu.state.y.wrapping_sub(1);
                    cpu.state.flags.set_nz(cpu.state.y);
                }
                Instr::ASL(Operand::Accumulator) => {
                    cpu.state
                        .flags
                        .set(Flag::Carry, (cpu.state.accumulator as i8).is_negative());
                    cpu.state.accumulator = cpu.state.accumulator.wrapping_shl(1);
                    cpu.state.flags.set_nz(cpu.state.accumulator);
                }
                Instr::ASL(Operand::Addressing(addr)) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    let op = cpu.memory.read_u8(addr).map_err(RunError::Memory)?;
                    let res = op.wrapping_shl(1);
                    cpu.memory.write_u8(addr, res).map_err(RunError::Memory)?;

                    cpu.state.flags.set(Flag::Carry, (op as i8).is_negative());
                    cpu.state.flags.set_nz(res);
                }
                Instr::LSR(Operand::Accumulator) => {
                    cpu.state
                        .flags
                        .set(Flag::Carry, cpu.state.accumulator & 1 == 1);
                    cpu.state.accumulator = cpu.state.accumulator.wrapping_shr(1);
                    cpu.state.flags.set_nz(cpu.state.accumulator);
                }
                Instr::LSR(Operand::Addressing(addr)) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    let op = cpu.memory.read_u8(addr).map_err(RunError::Memory)?;
                    let res = op.wrapping_shr(1);
                    cpu.memory.write_u8(addr, res).map_err(RunError::Memory)?;

                    cpu.state.flags.set(Flag::Carry, op & 1 == 1);
                    cpu.state.flags.set_nz(res);
                }
                // https://github.com/kromych/yamos6502/blob/main/src/yamos6502.rs#L491
                Instr::ROL(Operand::Accumulator) => {
                    let prev_acc = cpu.state.accumulator;

                    cpu.state.accumulator = cpu.state.accumulator.wrapping_shl(1);
                    cpu.state.accumulator |= cpu.state.flags.contains(Flag::Carry) as u8;

                    cpu.state
                        .flags
                        .set(Flag::Carry, (prev_acc as i8).is_negative());
                    cpu.state.flags.set_nz(cpu.state.accumulator);
                }
                Instr::ROL(Operand::Addressing(addr)) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    let op = cpu.memory.read_u8(addr).map_err(RunError::Memory)?;

                    let mut res = op.wrapping_shl(1);
                    res |= cpu.state.flags.contains(Flag::Carry) as u8;

                    cpu.memory.write_u8(addr, res).map_err(RunError::Memory)?;
                    cpu.state.flags.set(Flag::Carry, (op as i8).is_negative());
                    cpu.state.flags.set_nz(res);
                }
                Instr::ROR(Operand::Accumulator) => {
                    let prev_acc = cpu.state.accumulator;

                    cpu.state.accumulator = cpu.state.accumulator.wrapping_shr(1);
                    if cpu.state.flags.contains(Flag::Carry) {
                        cpu.state.accumulator |= 1 << 7;
                    }

                    cpu.state.flags.set(Flag::Carry, prev_acc & 1 == 1);
                    cpu.state.flags.set_nz(cpu.state.accumulator);
                }
                Instr::ROR(Operand::Addressing(addr)) => {
                    let (addr, _) = cpu.get_addressing(addr)?;
                    let op = cpu.memory.read_u8(addr).map_err(RunError::Memory)?;

                    let mut res = op.wrapping_shr(1);
                    if cpu.state.flags.contains(Flag::Carry) {
                        res |= 1 << 7;
                    }

                    cpu.memory.write_u8(addr, res).map_err(RunError::Memory)?;
                    cpu.state.flags.set(Flag::Carry, op & 1 == 1);
                    cpu.state.flags.set_nz(res);
                }
                Instr::ASL(Operand::Immediate(_))
                | Instr::LSR(Operand::Immediate(_))
                | Instr::ROL(Operand::Immediate(_))
                | Instr::ROR(Operand::Immediate(_)) => unreachable!(),
                Instr::JMP(addr) => pc = addr,
                Instr::JMPIndirect(base) => {
                    pc = cpu.memory.read_u16(base).map_err(RunError::Memory)?
                }
                Instr::JSR(addr) => {
                    cpu.push_u16(pc.wrapping_sub(1))?;
                    pc = addr
                }
                Instr::BCC(addr) => {
                    if !cpu.state.flags.contains(Flag::Carry) {
                        tick(cpu, 1 + page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BCS(addr) => {
                    if cpu.state.flags.contains(Flag::Carry) {
                        tick(cpu, 1 + page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BEQ(addr) => {
                    if cpu.state.flags.contains(Flag::Zero) {
                        tick(cpu, 1 + page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BMI(addr) => {
                    if cpu.state.flags.contains(Flag::Negative) {
                        tick(cpu, 1 + page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BNE(addr) => {
                    if !cpu.state.flags.contains(Flag::Zero) {
                        #[cfg(debug_assertions)]
                        if prev_pc == addr {
                            return Ok(());
                        }

                        tick(cpu, 1 + page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BPL(addr) => {
                    if !cpu.state.flags.contains(Flag::Negative) {
                        tick(cpu, 1 + page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BVC(addr) => {
                    if !cpu.state.flags.contains(Flag::Overflow) {
                        tick(cpu, 1 + page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::BVS(addr) => {
                    if cpu.state.flags.contains(Flag::Overflow) {
                        tick(cpu, 1 + page_crossed(pc, addr) as u8);
                        pc = addr;
                    }
                }
                Instr::CLC => cpu.state.flags.remove(Flag::Carry),
                Instr::CLD => cpu.state.flags.remove(Flag::Decimal),
                Instr::CLI => cpu.state.flags.remove(Flag::InterruptDisable),
                Instr::CLV => cpu.state.flags.remove(Flag::Overflow),
                Instr::SEC => cpu.state.flags.insert(Flag::Carry),
                Instr::SED => cpu.state.flags.insert(Flag::Decimal),
                Instr::SEI => cpu.state.flags.insert(Flag::InterruptDisable),
                // https://github.com/kromych/yamos6502/blob/main/src/yamos6502.rs#L698
                Instr::BRK => {
                    cpu.push_u16(pc.wrapping_add(1))?;
                    cpu.push(cpu.state.flags.into_u8(false))?;
                    cpu.state.flags.insert(Flag::InterruptDisable);
                    pc = cpu.memory.read_u16(0xfffe).map_err(RunError::Memory)?;
                }
                Instr::NOP => {}
                Instr::RTS => {
                    pc = cpu.pop_u16()?.wrapping_add(1);
                }
                Instr::RTI => {
                    cpu.state.flags = Flags::from_u8(cpu.pop()?);
                    pc = cpu.pop_u16()?;
                }
            }
        }
    }
}
