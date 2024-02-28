use self::registers::{controller::Controller, mask::Mask, status::Status};

pub mod registers;
pub mod tiles;

#[derive(Debug, Clone, Default)]
pub struct Ppu {
    pub controller: Controller,
    pub mask: Mask,
    pub status: Status,
    pub oam_address: u8,
    pub oam_data: u8,
    pub current_cycle: u64,
    last_write: u8,
}

impl Ppu {
    pub fn new() -> Self {
        return Self::default();
    }

    pub fn tick(&mut self, cycles: u8) {
        todo!()
    }
}
