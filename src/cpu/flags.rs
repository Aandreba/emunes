use std::fmt::Debug;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(transparent)]
pub struct Flags(u8);

impl Flags {
    #[inline]
    pub fn insert(&mut self, flag: Flag) {
        self.0 |= 1 << flag as u8
    }

    #[inline]
    pub fn remove(&mut self, flag: Flag) {
        self.0 &= !(1 << flag as u8)
    }

    #[inline]
    pub fn set(&mut self, flag: Flag, value: bool) {
        match value {
            true => self.insert(flag),
            false => self.remove(flag),
        }
    }

    #[inline]
    pub fn set_nz(&mut self, val: u8) {
        self.set(Flag::Zero, val == 0);
        self.set(Flag::Negative, (val as i8).is_negative())
    }

    #[inline]
    pub fn contains(self, flag: Flag) -> bool {
        return (self.0 >> flag as u8) & 1 == 1;
    }

    // http://wiki.nesdev.com/w/index.php/CPU_status_flag_behavior
    pub fn into_u8(self, from_interrupt: bool) -> u8 {
        let mut bits = self.0 | (1 << 5);
        if !from_interrupt {
            bits |= 1 << 4
        }
        return bits;
    }

    #[inline(always)]
    pub fn from_u8(val: u8) -> Self {
        return Self((val & !(1 << 4)) | (1 << 5));
    }
}

impl Debug for Flags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut data = Vec::with_capacity(8);
        for (flag, name) in [
            (Flag::Carry as u8, "CARRY"),
            (Flag::Zero as u8, "ZERO"),
            (Flag::InterruptDisable as u8, "INTERRUPT_DISABLE"),
            (Flag::Decimal as u8, "DECIMAL"),
            (Flag::Overflow as u8, "OVERFLOW"),
            (Flag::Negative as u8, "NEGATIVE"),
            (4, "BREAK"),
        ] {
            if (self.0 >> flag) & 1 == 1 {
                data.push(name);
            }
        }

        return write!(f, "{} (0x{:02X})", &data.join(" | "), self.0);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Flag {
    Carry = 0,
    Zero = 1,
    InterruptDisable = 2,
    Decimal = 3,
    Overflow = 6,
    Negative = 7,
}
