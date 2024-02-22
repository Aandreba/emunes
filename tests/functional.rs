use emunes::cpu::{
    memory::{create_linear_memory, debug::DebugMemory, Memory},
    Cpu,
};
use flexi_logger::{Duplicate, FileSpec};

#[test]
fn functional() {
    let h = flexi_logger::Logger::try_with_str("trace")
        .unwrap()
        .log_to_file(FileSpec::default().directory("./logs").suppress_timestamp())
        .duplicate_to_stderr(Duplicate::All)
        .start()
        .unwrap();

    let mut memory = DebugMemory::new(create_linear_memory());
    memory
        .copy_from(0x00, include_bytes!("6502_functional_test.bin"))
        .unwrap();
    // invalid opcode will make emulation terminate
    memory.write_u8(0x3469, 0xff).unwrap();

    let mut cpu = Cpu::new(memory);
    cpu.run(0x400, |cpu, _| {
        log::trace!("{cpu:#02X?}");
    })
    .unwrap();

    h.flush();
    drop(h);
}
