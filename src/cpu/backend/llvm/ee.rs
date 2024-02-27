use inkwell::llvm_sys::orc2;

pub struct LlJitBuilder {
    inner: orc2::lljit::LLVMOrcLLJITBuilderRef,
}

impl LlJitBuilder {}

impl Drop for LlJitBuilder {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe { orc2::lljit::LLVMOrcDisposeLLJITBuilder(self.inner) }
    }
}

pub struct LlJit {
    inner: orc2::lljit::LLVMOrcLLJITRef,
}

impl Drop for LlJit {
    fn drop(&mut self) {
        orc2::lljit::LLVMOrcDisposeLLJIT()
    }
}
