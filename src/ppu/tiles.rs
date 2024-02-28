use std::{hint::unreachable_unchecked, mem::size_of, ops::Deref};

const CHR_SIZE: usize = 8 << 10;
const TILE_COUNT: usize = CHR_SIZE / size_of::<Tile>();

pub struct Chr {
    data: Box<[Tile; TILE_COUNT]>,
}

impl Chr {
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

    #[inline(always)]
    pub fn as_tiles(&self) -> &[Tile; TILE_COUNT] {
        return self;
    }
}

impl Deref for Chr {
    type Target = [Tile; TILE_COUNT];

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Tile([u8; 16]);

impl Tile {
    pub fn get_pixel(&self, x: u8, y: u8) -> Option<Pixel> {
        if x >= 8 || y >= 8 {
            return None;
        }

        let lo = (self.0[y as usize] >> x) & 1 == 1;
        let hi = (self.0[y as usize + 0x8] >> x) & 1 == 1;

        return Some(match lo as u8 + ((hi as u8) << 1) {
            0 => Pixel::Zero,
            1 => Pixel::One,
            2 => Pixel::Two,
            3 => Pixel::Three,
            _ => unsafe { unreachable_unchecked() },
        });
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Pixel {
    Zero = 0,
    One = 1,
    Two = 2,
    Three = 3,
}
