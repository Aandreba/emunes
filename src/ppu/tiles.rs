use std::{
    hint::unreachable_unchecked,
    mem::size_of,
    ops::{Deref, DerefMut},
};

const CHR_SIZE: usize = 8 << 10;
const TILE_COUNT: usize = CHR_SIZE / size_of::<Tile>();

#[derive(Debug, Clone)]
pub struct PatternTable {
    data: Box<[Tile; TILE_COUNT]>,
}

impl PatternTable {
    pub fn new(chr_rom: Vec<u8>) -> Option<Self> {
        let chr_rom = chr_rom.into_boxed_slice();
        if chr_rom.len() != CHR_SIZE {
            return None;
        }

        return Some(Self {
            data: unsafe { Box::from_raw(Box::into_raw(chr_rom).cast::<[Tile; TILE_COUNT]>()) },
        });
    }

    #[inline]
    pub fn as_bytes(&self) -> &[u8; CHR_SIZE] {
        return unsafe {
            &*(self.as_tiles() as *const [Tile; TILE_COUNT] as *const [u8; CHR_SIZE])
        };
    }

    #[inline]
    pub fn as_mut_bytes(&mut self) -> &mut [u8; CHR_SIZE] {
        return unsafe {
            &mut *(self.as_mut_tiles() as *mut [Tile; TILE_COUNT] as *mut [u8; CHR_SIZE])
        };
    }

    #[inline(always)]
    pub fn as_tiles(&self) -> &[Tile; TILE_COUNT] {
        return self;
    }

    #[inline(always)]
    pub fn as_mut_tiles(&mut self) -> &mut [Tile; TILE_COUNT] {
        return self;
    }
}

impl Deref for PatternTable {
    type Target = [Tile; TILE_COUNT];

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl DerefMut for PatternTable {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(transparent)]
pub struct Tile([u8; 16]);

impl Tile {
    pub fn get_pixel(&self, x: u8, y: u8) -> Option<u2> {
        if x >= 8 || y >= 8 {
            return None;
        }

        let lo = (self.0[y as usize] >> (7 - x)) & 1 == 1;
        let hi = (self.0[y as usize + 0x8] >> (7 - x)) & 1 == 1;

        return Some(match lo as u8 | ((hi as u8) << 1) {
            0 => u2::Zero,
            1 => u2::One,
            2 => u2::Two,
            3 => u2::Three,
            _ => unsafe { unreachable_unchecked() },
        });
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[allow(non_camel_case_types)]
pub enum u2 {
    #[default]
    Zero = 0,
    One = 1,
    Two = 2,
    Three = 3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Bank {
    Left,
    Right,
}
