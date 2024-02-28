#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[repr(transparent)]
pub struct Address(u16);

impl Address {
    pub fn write(&mut self, val: u8) {
        self.0 <<= 8;
        self.0 |= val as u16
    }

    pub fn increment(&mut self, delta: u16) {
        self.0 = self.0.wrapping_add(delta)
    }

    pub const fn into_inner(self) -> u16 {
        return self.0 % 0x4000;
    }
}
