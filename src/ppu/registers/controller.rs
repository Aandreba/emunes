#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[repr(transparent)]
pub struct Controller(u8);

impl Controller {
    pub fn set(&mut self, bits: u8) {
        self.0 = bits
    }

    pub const fn base_nametable_address(self) -> u16 {
        let offset = self.0 & 0b11;
        return 0x2000 + (offset as u16) * 0x400;
    }

    pub const fn vram_address_increment(self) -> u64 {
        return match self.0 & 0b100 != 0 {
            false => 1,
            true => 32,
        };
    }

    pub const fn sprite_pattern_table_address(self) -> u16 {
        return match self.0 & 0b1000 != 0 {
            false => 0x0,
            true => 0x1000,
        };
    }

    pub const fn background_pattern_table_address(self) -> u16 {
        return match self.0 & 0b10000 != 0 {
            false => 0x0,
            true => 0x1000,
        };
    }

    pub const fn mode(self) -> PpuMode {
        return match self.0 & 0b100000 != 0 {
            true => PpuMode::Master,
            false => PpuMode::Slave,
        };
    }

    pub const fn vbi_nmi_enabled(self) -> bool {
        return (self.0 >> 7) & 1 == 1;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PpuMode {
    Master,
    Slave,
}
