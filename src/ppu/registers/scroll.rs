#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Scroll {
    pub x: u8,
    pub y: u8,
    pub latch: bool,
}

impl Scroll {
    pub fn write(&mut self, data: u8) {
        if !self.latch {
            self.x = data;
        } else {
            self.y = data;
        }
        self.latch = !self.latch;
    }

    pub fn reset_latch(&mut self) {
        self.latch = false;
    }
}
