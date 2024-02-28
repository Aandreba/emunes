use self::{
    memory::Memory,
    registers::{address::Address, controller::Controller, mask::Mask, status::Status},
};
use crate::cartridge::Cartridge;

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
    pub last_write: u8,
}

impl Ppu {
    pub fn new(cartridge: Cartridge) -> Self {
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
            last_write: 0,
        };
    }

    pub fn tick(&mut self, cycles: u8) {
        self.current_cycle += cycles as u64;
        todo!()
    }
}

impl Ppu {
    pub fn read_status(&mut self) -> u8 {
        let res = self.status.read() | (self.last_write & 0b11111);
        // TODO scroll latch reset
        return res;
    }

    pub fn read_data(&mut self) -> u8 {
        let addr = self.address.into_inner();

        let mut res = self.memory.read(addr);
        if addr <= 0x3eff {
            res = core::mem::replace(&mut self.data_buffer, self.memory.read(addr));
        }

        self.address
            .increment(self.controller.vram_address_increment());
        return res;
    }

    pub fn write_data(&mut self, val: u8) {
        let addr = self.address.into_inner();
        self.memory.write(addr, val);
        self.address
            .increment(self.controller.vram_address_increment());
    }
}
