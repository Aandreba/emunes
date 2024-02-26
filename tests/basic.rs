use emunes::cpu::{
    backend::llvm::Llvm,
    memory::{create_linear_memory, debug::DebugMemory, Memory},
    Cpu,
};
use inkwell::context::Context;

#[test]
fn basic() {
    let code = [0xa9, 0xc0, 0xaa, 0xe8, 0x00];

    let h = flexi_logger::Logger::try_with_str("trace")
        .unwrap()
        .start()
        .unwrap();

    let mut memory = DebugMemory::new(create_linear_memory());
    memory.copy_from(0x00, &code).unwrap();
    // invalid opcode will make emulation terminate
    // memory.write_u8(0x3469, 0xff).unwrap();

    let cx = Context::create();
    let mut cpu = Cpu::new(memory, Llvm::new(&cx).unwrap());
    cpu.run(0x00, |_| {}).unwrap();

    cpu.backend.print_to_stderr();

    h.flush();
    drop(h);
}

#[test]
fn basic_adc() {
    let code = [0x69, 0xc0, 0xaa, 0xe8, 0x00];

    let h = flexi_logger::Logger::try_with_str("trace")
        .unwrap()
        .start()
        .unwrap();

    let mut memory = DebugMemory::new(create_linear_memory());
    memory.copy_from(0x00, &code).unwrap();
    // invalid opcode will make emulation terminate
    // memory.write_u8(0x3469, 0xff).unwrap();

    let cx = Context::create();
    let mut cpu = Cpu::new(memory, Llvm::new(&cx).unwrap());
    cpu.disable_decimal_mode();

    cpu.run(0x00, |_| {}).unwrap();
    cpu.backend.print_to_stderr();

    h.flush();
    drop(h);
}

#[test]
fn basic_adc_branch() {
    let code = [0x69, 0xc0, 0x90, -3i8 as u8, 0xaa, 0xe8, 0x00];

    let h = flexi_logger::Logger::try_with_str("trace")
        .unwrap()
        .start()
        .unwrap();

    let mut memory = DebugMemory::new(create_linear_memory());
    memory.copy_from(0x00, &code).unwrap();
    // invalid opcode will make emulation terminate
    // memory.write_u8(0x3469, 0xff).unwrap();

    let cx = Context::create();
    let mut cpu = Cpu::new(memory, Llvm::new(&cx).unwrap());
    cpu.disable_decimal_mode();

    cpu.run(0x00, |_| {}).unwrap();
    cpu.backend.print_to_stderr();

    h.flush();
    drop(h);
}
