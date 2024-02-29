use super::tiles::u2;
use bytemuck::{Pod, Zeroable};
use std::hint::unreachable_unchecked;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Pod, Zeroable)]
#[repr(C)]
pub struct NameTable {
    pub tiles: [[u8; 32]; 30],
    pub attributes: [[Attribute; 8]; 8],
}

impl NameTable {
    #[inline(always)]
    pub fn as_bytes(&self) -> &[u8; 1024] {
        return bytemuck::cast_ref(self);
    }

    #[inline(always)]
    pub fn as_mut_bytes(&mut self) -> &mut [u8; 1024] {
        return bytemuck::cast_mut(self);
    }

    pub fn get_palette_idx(&self, tile_row: usize, tile_col: usize) -> u2 {
        let attribute = self.attributes[tile_row / 4][tile_col / 4];
        let idx = (tile_col % 4 / 2) | ((tile_row % 4 / 2) << 1);
        return attribute.get(idx as u8);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Pod, Zeroable)]
#[repr(transparent)]
pub struct Attribute(u8);

impl Attribute {
    pub fn get(&self, idx: u8) -> u2 {
        let offset = idx << 1;
        return match (self.0 >> offset) & 0b11 {
            0 => u2::Zero,
            1 => u2::One,
            2 => u2::Two,
            3 => u2::Three,
            _ => unsafe { unreachable_unchecked() },
        };
    }
}
