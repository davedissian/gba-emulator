use gpu::{KeyCode, KeyState};

pub enum InputButton {
    A,
    B,
    Select,
    Start,
    Left,
    Right,
    Up,
    Down
}

pub struct Input {
    rows: [u8; 2], // Row 0: A, B, Select, Start. Row 1: Right, Left, Down, Up
    row_select: u8, // 0x10 to select row 0, 0x20 to select row 1.
    input_map: fn(KeyCode) -> Option<InputButton>
}

impl Input {
    pub fn new(input_map: fn(KeyCode) -> Option<InputButton>) -> Input {
        Input {
            rows: [0x0Fu8; 2],
            row_select: 0,
            input_map
        }
    }

    pub fn on_key_input(&mut self, code: KeyCode, state: KeyState) {
        if let Some(button) = (self.input_map)(code) {
            let (mask, row) = match button {
                InputButton::A => (0b0001, 0),
                InputButton::B => (0b0010, 0),
                InputButton::Select => (0b0100, 0),
                InputButton::Start => (0b1000, 0),
                InputButton::Right => (0b0001, 1),
                InputButton::Left => (0b0010, 1),
                InputButton::Down => (0b0100, 1),
                InputButton::Up => (0b1000, 1),
            };
            if state == KeyState::Pressed {
                // Clear mask bit.
                self.rows[row] &= !mask;
            } else {
                // Set mask bit.
                self.rows[row] |= mask;
            }
        }
    }

    pub fn read_u8(&self) -> u8 {
        self.row_select | match self.row_select {
            0x10 => self.rows[0],
            0x20 => self.rows[1],
            _ => 0u8
        }
    }

    pub fn write_u8(&mut self, value: u8) {
        self.row_select = value & 0x30;
    }
}