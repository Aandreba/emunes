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
        .log_to_file(FileSpec::default().directory("./logs").suppress_timestamp())
        .duplicate_to_stderr(Duplicate::All)
        .start()
        .unwrap();

    let mut memory = DebugMemory::new(create_linear_memory());
    memory
        .copy_from(0x00, include_bytes!("../tests/6502_functional_test.bin"))
        .unwrap();

    let mut cpu = Cpu::new(memory);
    cpu.run(0x400, |cpu, _| {
        log::trace!("{cpu:#02X?}");
    })
    .unwrap();

    let (memory, history) = cpu.memory.into_parts();
    println!("{:#?}", history.get(&0x000C));

    h.flush();
    drop(h);
}
