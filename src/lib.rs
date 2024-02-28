use cartridge::Cartridge;
use cpu::{backend::interpreter::Interpreter, Cpu};
use memory::NesMemory;

pub mod cartridge;
pub mod cpu;
pub mod memory;
pub mod ppu;
pub mod video;

pub struct Nes {
    cpu: Cpu<NesMemory, Interpreter>,
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Self {
        return Self {
            cpu: Cpu::new(NesMemory::new(cartridge), Interpreter),
        };
    }

    pub fn run(&mut self) {
        self.cpu.restart(|mem, cycles| todo!());
    }
}
