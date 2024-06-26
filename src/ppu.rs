use self::{
    memory::Memory,
    oam::Oam,
    registers::{
        address::Address, controller::Controller, mask::Mask, scroll::Scroll, status::Status,
    },
};

pub mod memory;
pub mod nametable;
pub mod oam;
pub mod palette;
pub mod registers;
pub mod render;
pub mod tiles;

#[derive(Debug, Clone)]
pub struct Ppu {
    pub memory: Memory,
    // Registers
    pub controller: Controller,
    pub mask: Mask,
    pub status: Status,
    pub oam_address: u8,
    pub oam_data: Oam,
    pub address: Address,
    pub scroll: Scroll,
    data_buffer: u8,
    current_cycle: u64,
    current_scanline: u64,
}

impl Ppu {
    pub fn new(chr_rom: Vec<u8>, mirror: crate::cartridge::Mirroring) -> Self {
        return Self {
            memory: Memory::new(chr_rom, mirror),
            controller: Controller::default(),
            mask: Mask::default(),
            status: Status::default(),
            oam_address: 0,
            oam_data: Oam::default(),
            address: Address::default(),
            scroll: Scroll::default(),
            data_buffer: 0,
            current_cycle: 0,
            current_scanline: 0,
        };
    }

    pub fn tick(&mut self, nmi_interrupt: &mut bool, cycles: u8) -> bool {
        self.current_cycle += cycles as u64;
        if self.current_cycle >= 341 {
            if self.is_sprite_0_hit(self.current_cycle) {
                self.status.set_sprite_zero_hit(true);
            }

            self.current_cycle = self.current_cycle - 341;
            self.current_scanline += 1;

            if self.current_scanline == 241 {
                self.status.set_vblank_started(true);
                self.status.set_sprite_zero_hit(false);
                if self.controller.vbi_nmi_enabled() {
                    *nmi_interrupt = true;
                }
            }

            if self.current_scanline >= 262 {
                self.current_scanline = 0;
                *nmi_interrupt = false;
                self.status.set_sprite_zero_hit(false);
                self.status.set_vblank_started(false);
                return true;
            }
        }
        return false;
    }

    fn is_sprite_0_hit(&self, cycle: u64) -> bool {
        let y = self.oam_data[0].top_y as u64;
        let x = self.oam_data[3].left_x as u64;
        (y == self.current_scanline) && x <= cycle && self.mask.show_sprites()
    }
}

impl Ppu {
    pub fn read_status(&mut self) -> u8 {
        let res = self.status.into_inner();
        self.status.set_vblank_started(false);
        self.address.reset_latch();
        // TODO scroll latch reset
        return res;
    }

    pub fn read_oam_data(&mut self) -> u8 {
        return self.oam_data.as_bytes()[self.oam_address as usize];
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
}

impl Ppu {
    pub fn write_controller(&mut self, val: u8) -> bool {
        let prev_nmi = self.controller.vbi_nmi_enabled();
        self.controller.set(val);
        return !prev_nmi && self.controller.vbi_nmi_enabled() && self.status.vblank_started();
    }

    pub fn write_mask(&mut self, val: u8) {
        self.mask.set(val)
    }

    pub fn write_oam_address(&mut self, val: u8) {
        self.oam_address = val
    }

    pub fn write_oam_data(&mut self, val: u8) {
        self.oam_data.as_mut_bytes()[self.oam_address as usize] = val;
        self.oam_address = self.oam_address.wrapping_add(1);
    }

    pub fn write_scroll(&mut self, val: u8) {
        self.scroll.write(val)
    }

    pub fn write_data(&mut self, val: u8) {
        let addr = self.address.into_inner();
        self.memory.write(addr, val);
        self.address
            .increment(self.controller.vram_address_increment());
    }

    pub fn write_address(&mut self, val: u8) {
        self.address.write(val)
    }
}
