use super::Backend;
use inkwell::{
    context::Context, memory_buffer::MemoryBuffer, module::Module, support::LLVMString,
    values::FunctionValue,
};
use std::collections::HashMap;

const SKELETON: &[u8] = include_bytes!("../../../skeleton.ll");

pub struct Llvm<'a> {
    compiled: HashMap<u16, FunctionValue<'a>>,
    module: Module<'a>,
    cx: &'a Context,
}

impl<'a> Llvm<'a> {
    pub fn new(cx: &'a Context) -> Result<Self, LLVMString> {
        let ir =
            MemoryBuffer::create_from_memory_range_copy(&SKELETON[..SKELETON.len() - 1], "main");

        return Ok(Self {
            compiled: HashMap::new(),
            module: cx.create_module_from_ir(ir)?,
            cx,
        });
    }

    pub fn print_to_stderr(&self) {
        self.module.print_to_stderr();
    }
}

impl<'a> Backend for Llvm<'a> {
    fn run<M: crate::cpu::memory::Memory>(
        cpu: &mut crate::cpu::Cpu<M, Self>,
        pc: u16,
        tick: impl FnMut(u8),
    ) -> Result<(), M::Error> {
        todo!()
    }
}

pub struct Builder<'a> {
    builder: inkwell::builder::Builder<'a>,
}

#[cfg(test)]
mod tests {
    use super::Llvm;
    use inkwell::context::Context;

    #[test]
    fn init() {
        let cx = Context::create();
        let llvm = Llvm::new(&cx).unwrap();
        llvm.print_to_stderr();
    }
}
