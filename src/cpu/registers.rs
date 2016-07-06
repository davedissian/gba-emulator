/*
    Flag Register:

    Bit  Name  Set Clr  Expl.
    7    zf    Z   NZ   Zero Flag
    6    n     -   -    Add/Sub-Flag (BCD)
    5    h     -   -    Half Carry Flag (BCD)
    4    cy    C   NC   Carry Flag
    3-0  -     -   -    Not used (always zero)
*/

pub struct Registers {
    pub a: u8,
    pub f: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub sp: u16,    // Stack Pointer
    pub pc: u16     // Program Counter
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg8 {
    A, F, B, C, D, E, H, L
}

#[derive(PartialEq, Clone, Copy, Eq, Debug)]
pub enum Reg16 {
    AF, BC, DE, HL, SP, PC
}

#[derive(Debug)]
pub enum Flag {
    Z, N, H, C
}

impl Registers {
    pub fn new() -> Registers {
        Registers {
            a: 0,
            f: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            sp: 0,
            pc: 0
        }
    }

    fn select_flag(f: Flag) -> u8 {
        match f {
            Flag::Z => 7,
            Flag::N => 6,
            Flag::H => 5,
            Flag::C => 4
        }
    }

    pub fn get_flag(&self, f: Flag) -> bool {
        ((self.f >> Registers::select_flag(f)) & 1) == 1
    }

    pub fn update_flag(&mut self, f: Flag, v: bool) {
        if v {
            self.set_flag(f);
        } else {
            self.reset_flag(f);
        }
    }

    pub fn set_flag(&mut self, f: Flag) {
        self.f |= 1 << Registers::select_flag(f);
    }

    pub fn reset_flag(&mut self, f: Flag) {
        self.f &= !(1 << Registers::select_flag(f));
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn set_z_flag() {
        let mut r = Registers::new();
        r.set_flag(Flag::Z);
        assert_eq!(r.f, 0b10000000)
    }

    #[test]
    fn set_n_flag() {
        let mut r = Registers::new();
        r.set_flag(Flag::N);
        assert_eq!(r.f, 0b01000000)
    }

    #[test]
    fn set_h_flag() {
        let mut r = Registers::new();
        r.set_flag(Flag::H);
        assert_eq!(r.f, 0b00100000)
    }

    #[test]
    fn set_c_flag() {
        let mut r = Registers::new();
        r.set_flag(Flag::C);
        assert_eq!(r.f, 0b00010000)
    }

    #[test]
    fn reset_c_flag_only() {
        let mut r = Registers::new();
        r.f = 0b00010000;
        r.reset_flag(Flag::C);
        assert_eq!(r.f, 0b00000000)
    }

    #[test]
    fn reset_z_and_c_flag_with_all_set() {
        let mut r = Registers::new();
        r.f = 0b11010000;
        r.reset_flag(Flag::Z);
        r.reset_flag(Flag::C);
        assert_eq!(r.f, 0b01000000)
    }
}
