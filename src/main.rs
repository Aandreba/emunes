use std::time::Duration;
use emunes::cpu::{
    memory::{create_linear_memory, debug::DebugMemory, Memory},
    Cpu,
};
use flexi_logger::{Duplicate, FileSpec};

pub fn main() {
    std::thread::sleep(Duration::from_secs(2));

    let h = flexi_logger::Logger::try_with_str("trace")
        .unwrap()
        .start()
        .unwrap();

    let mut memory = DebugMemory::new(create_linear_memory());
    memory
        .copy_from(0x00, include_bytes!("../tests/6502_functional_test.bin"))
        .unwrap();
    // invalid opcode will make emulation terminate
    memory.write_u8(0x3469, 0xff).unwrap();

    let mut cpu = Cpu::new(memory);
    cpu.run(0x400, |_, _| {})
    .unwrap();

    h.flush();
    drop(h);
}
