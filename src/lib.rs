use cartridge::Cartridge;
use cpu::{backend::interpreter::Interpreter, Cpu, RunError};
use joystick::Joystick;
use memory::NesMemory;
use std::{rc::Rc, sync::atomic::Ordering};
use video::Error;
use winit::event_loop::{EventLoop, EventLoopWindowTarget};
use winit_input_helper::WinitInputHelper;

use crate::video::palette::SYSTEM_PALETTE;

pub mod cartridge;
pub mod cpu;
pub mod joystick;
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

    pub fn run(
        mut self,
        player1: Joystick,
        player2: Option<Joystick>,
    ) -> Result<(), RunError<NesMemory, Interpreter>> {
        let mut input = WinitInputHelper::new();
        let joypad1 = self.cpu.memory.joypad1.status.clone();
        let joypad2 = self.cpu.memory.joypad2.status.clone();

        let mut event_loop = move |event, elwt: &EventLoopWindowTarget<_>| {
            if input.update(&event) {
                // Esc --> Exit
                if input.close_requested() {
                    elwt.exit();
                    return;
                }

                println!("{}", player1.handle_input(&input));
                joypad1.store(player1.handle_input(&input), Ordering::Release);
                if let Some(ref player2) = player2 {
                    joypad2.store(player2.handle_input(&input), Ordering::Release);
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
                let handle = Rc::new(std::thread::spawn(move || self
                    .cpu
                    .restart(tick)));

                let el_handle = handle.clone();
                self.event_loop.run(move |event, elwt| {
                    if el_handle.is_finished() {
                        elwt.exit();
                    }
                    event_loop(event, elwt);
                }).unwrap();

                if handle.is_finished() {
                    return Rc::into_inner(handle).unwrap().join().unwrap()
                } else {
                    return Ok(());
                }
            }
        }
    }
}
