use super::{unwrap_lock, Memory};
use std::{collections::HashMap, fmt::Debug, ops::Deref, panic::Location, sync::Mutex};

pub struct DebugMemory<M> {
    pub history: Mutex<HashMap<u16, Vec<Operation>>>,
    pub memory: M,
}

impl<M> DebugMemory<M> {
    pub fn new(memory: M) -> Self {
        return Self {
            history: Mutex::new(HashMap::new()),
            memory,
        };
    }

    pub fn clear_history(&mut self) {
        unwrap_lock(self.history.get_mut()).clear();
    }

    #[inline]
    pub fn history_at(&mut self, addr: u16) -> &[Operation] {
        unwrap_lock(self.history.get_mut())
            .get(&addr)
            .map(Vec::deref)
            .unwrap_or_default()
    }

    pub fn into_parts(self) -> (M, HashMap<u16, Vec<Operation>>) {
        return (self.memory, unwrap_lock(self.history.into_inner()));
    }
}

impl<M: Memory> Memory for DebugMemory<M> {
    type Error = M::Error;

    #[inline(always)]
    #[track_caller]
    fn read_u8(&mut self, addr: u16) -> Result<u8, Self::Error> {
        let val = self.memory.read_u8(addr)?;
        log::trace!("Read '0x{val:02X}' from '0x{addr:04X}'");

        unwrap_lock(self.history.lock())
            .entry(addr)
            .or_insert_with(|| Vec::with_capacity(1))
            .push(Operation {
                kind: OperationKind::Read,
                val,
                caller: Location::caller(),
            });

        return Ok(val);
    }

    #[inline(always)]
    #[track_caller]
    fn write_u8(&mut self, addr: u16, val: u8) -> Result<(), Self::Error> {
        self.memory.write_u8(addr, val)?;
        log::trace!("Wrote '0x{val:02X}' to '0x{addr:04X}'");

        unwrap_lock(self.history.get_mut())
            .entry(addr)
            .or_insert_with(|| Vec::with_capacity(1))
            .push(Operation {
                kind: OperationKind::Write,
                val,
                caller: Location::caller(),
            });

        return Ok(());
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Operation {
    pub kind: OperationKind,
    pub val: u8,
    pub caller: &'static Location<'static>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OperationKind {
    Read,
    Write,
}
