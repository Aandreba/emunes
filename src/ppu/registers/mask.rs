#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[repr(transparent)]
pub struct Mask(u8);

impl Mask {
    pub fn set(&mut self, bits: u8) {
        self.0 = bits
    }

    pub const fn grayscale(self) -> bool {
        return self.0 & 0b1 == 1;
    }

    pub const fn show_leftmost_background(self) -> bool {
        return self.0 & 0b10 != 0;
    }

    pub const fn show_leftmost_sprites(self) -> bool {
        return self.0 & 0b100 != 0;
    }

    pub const fn show_background(self) -> bool {
        return self.0 & 0b1000 != 0;
    }

    pub const fn show_sprites(self) -> bool {
        return self.0 & 0b10000 != 0;
    }

    pub const fn emphasize_red(self) -> bool {
        return self.0 & 0b100000 != 0;
    }

    pub const fn emphasize_green(self) -> bool {
        return self.0 & 0b1000000 != 0;
    }

    pub const fn emphasize_blue(self) -> bool {
        return self.0 & 0b10000000 != 0;
    }
}
