use cartridge::Cartridge;
use cpu::{backend::interpreter::Interpreter, Cpu, RunError};
use memory::NesMemory;
use video::Error;
use winit::{event::Event, event_loop::EventLoop, keyboard::KeyCode};
use winit_input_helper::WinitInputHelper;

pub mod cartridge;
pub mod cpu;
pub mod memory;
pub mod ppu;
pub mod video;

pub struct Nes {
    cpu: Cpu<NesMemory, Interpreter>,
    event_loop: EventLoop<()>,
}

impl Nes {
    pub async fn new(cartridge: Cartridge) -> Result<Self, Error> {
        let (memory, event_loop) = NesMemory::new(cartridge).await?;
        return Ok(Self {
            cpu: Cpu::new(memory, Interpreter),
            event_loop,
        });
    }

    pub fn run(mut self) -> Result<(), RunError<NesMemory, Interpreter>> {
        let mut input = WinitInputHelper::new();
        self.event_loop
            .run(move |event, elwt| {
                if input.update(&event) {
                    // Esc --> Exit
                    if input.key_pressed(KeyCode::Escape) {
                        elwt.exit();
                        return;
                    }
                }
            })
            .unwrap();

        return self
            .cpu
            .restart(|cpu, cycles| if cpu.memory.ppu.tick(3 * cycles) {});
    }
}
