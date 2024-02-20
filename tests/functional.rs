use emunes::cpu::{memory::Memory, Cpu};
use simple_logger::SimpleLogger;

#[test]
fn functional() {
    SimpleLogger::new().init().unwrap();

    let mut memory = Memory::new();
    memory.copy_from(0x00, include_bytes!("6502_functional_test.bin"));

    let mut cpu = Cpu::new(memory);
    cpu.run(0x400, |cpu, cycles| println!("{cpu:#?}"));
}
