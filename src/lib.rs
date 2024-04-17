use crate::video::palette::SYSTEM_PALETTE;
use cpu::{backend::Backend, Cpu, RunError};
use joystick::Joystick;
use memory::NesMemory;
use std::sync::atomic::Ordering;
use winit::{event::Event, event_loop::EventLoopWindowTarget};
use winit_input_helper::WinitInputHelper;

pub mod cartridge;
pub mod cpu;
pub mod joystick;
pub mod mapper;
pub mod memory;
pub mod ppu;
pub mod video;

pub struct Nes<B> {
    cpu: Cpu<NesMemory, B>,
}

impl<B: Backend> Nes<B> {
    pub fn from_memory(memory: NesMemory, backend: B) -> Self {
        let mut cpu = Cpu::new(memory, backend);
        cpu.disable_decimal_mode();
        return Self { cpu };
    }

    pub fn set_input(
        &mut self,
        player1: Joystick,
        player2: Option<Joystick>,
    ) -> impl Send + FnMut(Event<()>, &EventLoopWindowTarget<()>) {
        let mut input = WinitInputHelper::new();
        let joypad1 = self.cpu.memory.joypad1.status.clone();
        let joypad2 = self.cpu.memory.joypad2.status.clone();

        return move |event, elwt| {
            if input.update(&event) {
                // Esc --> Exit
                if input.close_requested() {
                    elwt.exit();
                    return;
                }

                joypad1.store(player1.handle_input(&input), Ordering::Release);
                if let Some(ref player2) = player2 {
                    joypad2.store(player2.handle_input(&input), Ordering::Release);
                }
            }
        };
    }

    pub fn run(mut self) -> Result<(), RunError<NesMemory, B>> {
        let tick = |cpu: &mut Cpu<NesMemory, B>, cycles| {
            cpu.nmi_interrupt |= core::mem::replace(&mut cpu.memory.nmi_interrupt, false);
            if cpu
                .memory
                .ppu
                .tick(&mut cpu.state.nmi_interrupt, 3 * cycles)
            {
                cpu.memory
                    .ppu
                    .render(&mut cpu.memory.video, &SYSTEM_PALETTE);

                if let Err(e) = cpu.memory.video.pixels.render() {
                    log::error!("{e}");
                }

                cpu.memory.video.window.request_redraw();
            }
        };

        self.cpu.restart(tick)?;

        return Ok(());
    }
}
