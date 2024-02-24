use super::{memory::Memory, Cpu};

pub mod interpreter;
#[cfg(feature = "llvm")]
pub mod llvm;

pub trait Backend: Sized {
    fn run<M: Memory>(
        cpu: &mut Cpu<M, Self>,
        pc: u16,
        tick: impl FnMut(&mut Cpu<M, Self>, u8),
    ) -> Result<(), M::Error>;
}
