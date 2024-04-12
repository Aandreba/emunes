use emunes::cpu::{
    backend::{interpreter::Interpreter, llvm::Llvm},
    memory::{create_linear_memory, debug::DebugMemory, Memory},
    Cpu,
};

#[test]
fn interpreter() {
    let h = flexi_logger::Logger::try_with_str("debug")
        .unwrap()
        .start()
        .unwrap();

    let mut memory = DebugMemory::new(create_linear_memory());
    memory
        .copy_from(0x00, include_bytes!("6502_functional_test.bin"))
        .unwrap();
    // invalid opcode will make emulation terminate
    memory.write_u8(0x3469, 0xff).unwrap();

    let mut cpu = Cpu::new(memory, Interpreter);
    cpu.run(0x400, |_, _| {}).unwrap();

    h.flush();
    drop(h);
}

#[test]
fn llvm() {
    let h = flexi_logger::Logger::try_with_str("trace")
        .unwrap()
        .start()
        .unwrap();

    let mut memory = DebugMemory::new(create_linear_memory());
    memory
        .copy_from(0x00, include_bytes!("6502_functional_test.bin"))
        .unwrap();
    // invalid opcode will make emulation terminate
    memory.write_u8(0x3469, 0xff).unwrap();

    let mut cpu = Cpu::new(memory, Llvm::new());
    cpu.run(0x400, |_, _| {}).unwrap();

    h.flush();
    drop(h);
}
