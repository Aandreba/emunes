use cartridge::Cartridge;
use cpu::{backend::interpreter::Interpreter, Cpu, RunError};
use memory::NesMemory;
use video::Error;
use winit::{
    event_loop::{EventLoop, EventLoopWindowTarget},
    keyboard::KeyCode,
    platform::run_on_demand::EventLoopExtRunOnDemand,
};
use winit_input_helper::WinitInputHelper;

use crate::video::palette::SYSTEM_PALETTE;

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
        let mut cpu = Cpu::new(memory, Interpreter);
        cpu.disable_decimal_mode();

        return Ok(Self { cpu, event_loop });
    }

    pub fn display_chr_rom(&mut self) {
        let mut input = WinitInputHelper::new();
        let event_loop = move |event, elwt: &EventLoopWindowTarget<_>| {
            if input.update(&event) {
                // Esc --> Exit
                if input.close_requested() || input.key_pressed(KeyCode::Escape) {
                    elwt.exit();
                    return;
                }
            }
        };

        cfg_if::cfg_if! {
            if #[cfg(target_family = "wasm")] {
                EventLoopExtWebSys::spawn(self.event_loop, event_loop);
                return self.cpu.restart(tick);
            } else {
                self.event_loop.run_on_demand(event_loop).unwrap();
            }
        }
    }

    pub fn run(mut self) -> Result<(), RunError<NesMemory, Interpreter>> {
        let mut input = WinitInputHelper::new();
        let event_loop = move |event, elwt: &EventLoopWindowTarget<_>| {
            if input.update(&event) {
                // Esc --> Exit
                if input.close_requested() || input.key_pressed(KeyCode::Escape) {
                    elwt.exit();
                    return;
                }
            }
        };

        let tick = |cpu: &mut Cpu<NesMemory, Interpreter>, cycles| {
            cpu.nmi_interrupt |= core::mem::replace(&mut cpu.memory.nmi_interrupt, false);
            if cpu.memory.ppu.tick(&mut cpu.nmi_interrupt, 3 * cycles) {
                cpu.memory
                    .ppu
                    .render(&mut cpu.memory.video, &SYSTEM_PALETTE);

                if let Err(e) = cpu.memory.video.pixels.render() {
                    log::error!("{e}");
                }

                cpu.memory.video.window.request_redraw();
            }
        };

        cfg_if::cfg_if! {
            if #[cfg(target_family = "wasm")] {
                EventLoopExtWebSys::spawn(self.event_loop, event_loop);
                return self.cpu.restart(tick);
            } else {
                let handle = std::thread::spawn(move || self
                    .cpu
                    .restart(tick));

                self.event_loop.run(event_loop).unwrap();
                if handle.is_finished() {
                    return handle.join().unwrap()
                } else {
                    return Ok(());
                }
            }
        }
    }
}
