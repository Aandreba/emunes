use self::{
    backend::{interpreter::Interpreter, Backend},
    flags::Flags,
    instrs::{page_crossed, Addressing, Operand},
    memory::Memory,
};
use std::{
    fmt::Debug,
    ops::{Deref, DerefMut},
};

pub mod backend;
pub mod bcd;
pub mod flags;
pub mod instrs;
pub mod memory;

pub struct Cpu<M, B = Interpreter> {
    pub state: State,
    pub memory: M,
    pub backend: B,
}

impl<M, B> Cpu<M, B> {
    pub fn new(memory: M, backend: B) -> Self {
        return Self {
            state: State {
                accumulator: 0,
                x: 0,
                y: 0,
                stack_ptr: 0xfe,
                flags: Flags::default(),
                decimal_enabled: true,
                nmi_interrupt: false,
            },
            memory,
            backend,
        };
    }

    pub fn disable_decimal_mode(&mut self) {
        self.decimal_enabled = false
    }

    pub fn enable_decimal_mode(mut self) {
        self.decimal_enabled = true
    }
}

impl<M: Memory, B: Backend> Cpu<M, B> {
    pub fn restart(&mut self, tick: impl FnMut(&mut Self, u8)) -> Result<(), RunError<M, B>> {
        self.accumulator = 0;
        self.x = 0;
        self.y = 0;
        self.stack_ptr = 0xfd;
        self.flags = Flags::from_u8(0b100100);
        let pc = self.memory.read_u16(0xfffc).map_err(RunError::Memory)?;
        self.run(pc, tick)
    }

    #[inline(always)]
    pub fn run(&mut self, pc: u16, tick: impl FnMut(&mut Self, u8)) -> Result<(), RunError<M, B>> {
        return B::run(self, pc, tick);
    }
}

// Stack Ops
impl<M: Memory, B: Backend> Cpu<M, B> {
    #[track_caller]
    pub fn push(&mut self, val: u8) -> Result<(), RunError<M, B>> {
        self.memory
            .write_u8(self.stack_addr(), val)
            .map_err(RunError::Memory)?;
        self.stack_ptr = self.stack_ptr.wrapping_sub(1);
        return Ok(());
    }

    #[track_caller]
    pub fn push_u16(&mut self, val: u16) -> Result<(), RunError<M, B>> {
        let [lo, hi] = val.to_le_bytes();
        self.push(hi)?;
        self.push(lo)?;
        return Ok(());
    }

    #[track_caller]
    pub fn pop(&mut self) -> Result<u8, RunError<M, B>> {
        self.stack_ptr = self.stack_ptr.wrapping_add(1);
        return self
            .memory
            .read_u8(self.stack_addr())
            .map_err(RunError::Memory);
    }

    #[track_caller]
    pub fn pop_u16(&mut self) -> Result<u16, RunError<M, B>> {
        let lo = self.pop()?;
        let hi = self.pop()?;
        return Ok(u16::from_le_bytes([lo, hi]));
    }

    fn stack_addr(&self) -> u16 {
        return 0x100 + self.stack_ptr as u16;
    }
}

// Operand ops
impl<M: Memory, B: Backend> Cpu<M, B> {
    #[track_caller]
    pub fn get_operand(&mut self, op: Operand) -> Result<(u8, bool), RunError<M, B>> {
        return Ok(match op {
            Operand::Accumulator => (self.accumulator, false),
            Operand::Immediate(val) => (val, false),
            Operand::Addressing(addr) => {
                let (addr, page_crossed) = self.get_addressing(addr)?;
                (
                    self.memory.read_u8(addr).map_err(RunError::Memory)?,
                    page_crossed,
                )
            }
        });
    }

    #[track_caller]
    pub fn get_addressing(&mut self, addr: Addressing) -> Result<(u16, bool), RunError<M, B>> {
        return Ok(match addr {
            Addressing::ZeroPage(addr) => (addr as u16, false),
            Addressing::ZeroPageX(base) => (base.wrapping_add(self.x) as u16, false),
            Addressing::ZeroPageY(base) => (base.wrapping_add(self.y) as u16, false),
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
                    .read_u16(base.wrapping_add(self.x) as u16)
                    .map_err(RunError::Memory)?,
                false,
            ),
            Addressing::IndirectIndexed(base) => {
                let base = self
                    .memory
                    .read_u16(base as u16)
                    .map_err(RunError::Memory)?;
                let addr = base.wrapping_add(self.y as u16);
                (addr, page_crossed(base, addr))
            }
        });
    }
}

impl<M, B> Deref for Cpu<M, B> {
    type Target = State;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

impl<M, B> DerefMut for Cpu<M, B> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.state
    }
}

impl<M, B: Debug> Debug for Cpu<M, B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cpu")
            .field("accumulator", &self.accumulator)
            .field("x", &self.x)
            .field("y", &self.y)
            .field("stack_ptr", &self.stack_ptr)
            .field("flags", &self.flags)
            .field("backend", &self.backend)
            .finish_non_exhaustive()
    }
}

pub enum RunError<M: Memory, B: Backend> {
    Memory(M::Error),
    Backend(B::Error),
    UnknownOpcode(u16),
}

impl<M: Memory, B: Backend> Debug for RunError<M, B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Memory(arg0) => f.debug_tuple("Memory").field(arg0).finish(),
            Self::Backend(arg0) => f.debug_tuple("Backend").field(arg0).finish(),
            Self::UnknownOpcode(arg0) => f.debug_tuple("UnknownOpcode").field(arg0).finish(),
        }
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct State {
    pub accumulator: u8,
    pub x: u8,
    pub y: u8,
    pub stack_ptr: u8,
    pub flags: Flags,
    pub decimal_enabled: bool,
    pub nmi_interrupt: bool,
}
