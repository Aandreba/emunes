use winit::keyboard::KeyCode;
use winit_input_helper::WinitInputHelper;

pub struct Joystick {
    pub up: KeyCode,
    pub left: KeyCode,
    pub down: KeyCode,
    pub right: KeyCode,
    pub a: KeyCode,
    pub b: KeyCode,
    pub select: KeyCode,
    pub start: KeyCode,
}

impl Joystick {
    pub const ARROW: Joystick = Joystick {
        up: KeyCode::ArrowUp,
        left: KeyCode::ArrowLeft,
        down: KeyCode::ArrowDown,
        right: KeyCode::ArrowRight,
        a: KeyCode::KeyA,
        b: KeyCode::KeyS,
        select: KeyCode::Space,
        start: KeyCode::Enter,
    };

    pub fn handle_input(&self, input: &WinitInputHelper) -> u8 {
        let up = Self::key_detected(input, self.up) as u8;
        let left = Self::key_detected(input, self.left) as u8;
        let down = Self::key_detected(input, self.down) as u8;
        let right = Self::key_detected(input, self.right) as u8;
        let a = Self::key_detected(input, self.a) as u8;
        let b = Self::key_detected(input, self.b) as u8;
        let start = Self::key_detected(input, self.start) as u8;
        let select = Self::key_detected(input, self.select) as u8;

        return a
            | (b << 1)
            | (select << 2)
            | (start << 3)
            | (up << 4)
            | (down << 5)
            | (left << 6)
            | (right << 7);
    }

    fn key_detected(input: &WinitInputHelper, keycode: KeyCode) -> bool {
        return input.key_pressed(keycode) || input.key_held(keycode);
    }
}

impl Default for Joystick {
    fn default() -> Self {
        Self::ARROW
    }
}
