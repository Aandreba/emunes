use super::tiles::u2;
use crate::video::{palette::Palette, Color};

#[derive(Debug, Clone, Default)]
pub struct PaletteMemory {
    pub universal_background_color: u8,
    pub background: [u8; 14],
    pub sprite: [u8; 14],
}

impl PaletteMemory {
    pub fn get_ubc(&self, colors: &Palette) -> Color {
        return colors[self.universal_background_color as usize];
    }

    pub fn get_background_palette(&self, idx: u2, palette: &Palette) -> [Color; 3] {
        let offset = 4 * idx as usize;
        return [
            palette[self.background[offset] as usize],
            palette[self.background[offset + 1] as usize],
            palette[self.background[offset + 2] as usize],
        ];
    }

    pub fn get_sprite_palette(&self, idx: u2, palette: &Palette) -> [Color; 3] {
        let offset = 4 * idx as usize;
        return [
            palette[self.sprite[offset] as usize],
            palette[self.sprite[offset + 1] as usize],
            palette[self.sprite[offset + 2] as usize],
        ];
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0x3f00 | 0x3f10 => self.universal_background_color,
            0x3f01..=0x3f0f => self.background[(addr - 0x3f01) as usize],
            0x3f11..=0x3f1f => self.sprite[(addr - 0x3f11) as usize],
            #[cfg(debug_assertions)]
            _ => unreachable!(),
            #[cfg(not(debug_assertions))]
            _ => unsafe { std::hint::unreachable_unchecked() },
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        *(match addr {
            0x3f00 | 0x3f10 => &mut self.universal_background_color,
            0x3f01..=0x3f0f => &mut self.background[(addr - 0x3f01) as usize],
            0x3f11..=0x3f1f => &mut self.sprite[(addr - 0x3f11) as usize],
            #[cfg(debug_assertions)]
            _ => unreachable!(),
            #[cfg(not(debug_assertions))]
            _ => unsafe { std::hint::unreachable_unchecked() },
        }) = val
    }
}
