#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[repr(transparent)]
pub struct Status(u8);

impl Status {
    pub const fn vblank_started(self) -> bool {
        return self.0 & 0b10000000 != 0;
    }

    pub const fn sprite_zero_hit(self) -> bool {
        return self.0 & 0b1000000 != 0;
    }

    pub const fn sprite_overflow(self) -> bool {
        return self.0 & 0b100000 != 0;
    }

    pub fn read(&mut self) -> u8 {
        let res = self.0;
        self.set_vblank_started(false);
        return res;
    }
}

impl Status {
    #[inline(always)]
    pub fn set_vblank_started(&mut self, vblank_started: bool) {
        self.set_bit(7, vblank_started)
    }

    #[inline(always)]
    pub fn set_sprite_zero_hit(&mut self, sprite_zero_hit: bool) {
        self.set_bit(6, sprite_zero_hit)
    }

    #[inline(always)]
    pub fn set_sprite_overflow(&mut self, sprite_overflow: bool) {
        self.set_bit(5, sprite_overflow)
    }

    fn set_bit(&mut self, bit: u8, value: bool) {
        debug_assert!(bit < 8);
        let mask = 1 << bit;

        if value {
            self.0 |= mask
        } else {
            self.0 &= !mask
        }
    }
}
