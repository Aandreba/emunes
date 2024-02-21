use emunes::cpu::{memory::Memory, Cpu};
use flexi_logger::{Duplicate, FileSpec, WriteMode};

#[test]
fn functional() {
    let h = flexi_logger::Logger::try_with_str("trace")
        .unwrap()
        .log_to_file(FileSpec::default().directory("./logs").suppress_timestamp())
        .write_mode(WriteMode::BufferAndFlush)
        .duplicate_to_stderr(Duplicate::All)
        .start()
        .unwrap();

    let mut memory = Memory::new();
    memory.copy_from(0x00, include_bytes!("6502_functional_test.bin"));

    let mut cpu = Cpu::new(memory);
    cpu.run(0x400, |cpu, cycles| println!("{cpu:#?}"));

    drop(h);
}
