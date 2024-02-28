use crate::cartridge::Cartridge;

use self::{
    memory::Memory,
    registers::{address::Address, controller::Controller, mask::Mask, status::Status},
};

pub mod memory;
pub mod nametable;
pub mod palette;
pub mod registers;
pub mod tiles;

#[derive(Debug, Clone)]
pub struct Ppu {
    pub memory: Memory,
    // Registers
    pub controller: Controller,
    pub mask: Mask,
    pub status: Status,
    pub oam_address: u8,
    pub oam_data: u8,
    pub address: Address,
    data_buffer: u8,
    // TODO scroll
    current_cycle: u64,
}

impl Ppu {
    pub fn new(cartride: Cartridge) -> Self {
        return Self {
            memory: todo!(),
            controller: todo!(),
            mask: todo!(),
            status: todo!(),
            oam_address: todo!(),
            oam_data: todo!(),
            address: todo!(),
            data_buffer: todo!(),
            current_cycle: todo!(),
        };
    }

    pub fn tick(&mut self, cycles: u8) {
        self.current_cycle += cycles as u64;
        todo!()
    }
}

impl Ppu {
    pub fn read_data(&mut self) -> u8 {
        let addr = self.address.into_inner();
        todo!();
    }

    pub fn write_data(&mut self, val: u8) {
        let addr = self.address.into_inner();
        todo!()
    }
}
