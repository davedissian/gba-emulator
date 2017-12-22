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

macro_rules! read_reg_pair {
    ($regs:expr, $h:ident, $l:ident) => {
        (($regs.$h as u16) << 8) | $regs.$l as u16
    };
}

macro_rules! write_reg_pair {
    ($regs:expr, $h:ident, $l:ident, $v:expr) => {{
        $regs.$h = ($v >> 8) as u8;
        $regs.$l = ($v & 0xFF) as u8;
    }};
}

impl Registers {
    pub fn new(skip_boot: bool) -> Registers {
        if skip_boot {
            Registers {
                a: 0x01,
                f: 0xB0,
                b: 0x00,
                c: 0x13,
                d: 0x00,
                e: 0xD8,
                h: 0x01,
                l: 0x4D,
                pc: 0x0100,
                sp: 0xFFFE,
            }
        } else {
            Registers {
                a: 0,
                f: 0,
                b: 0,
                c: 0,
                d: 0,
                e: 0,
                h: 0,
                l: 0,
                pc: 0,
                sp: 0,
            }
        }
    }

    pub fn af(&self) -> u16 {
        read_reg_pair!(self, a, f)
    }

    pub fn bc(&self) -> u16 {
        read_reg_pair!(self, b, c)
    }

    pub fn de(&self) -> u16 {
        read_reg_pair!(self, d, e)
    }

    pub fn hl(&self) -> u16 {
        read_reg_pair!(self, h, l)
    }

    pub fn set_af(&mut self, value: u16) {
        write_reg_pair!(self, a, f, value);
    }

    pub fn set_bc(&mut self, value: u16) {
        write_reg_pair!(self, b, c, value);
    }

    pub fn set_de(&mut self, value: u16) {
        write_reg_pair!(self, d, e, value);
    }

    pub fn set_hl(&mut self, value: u16) {
        write_reg_pair!(self, h, l, value);
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

    pub fn flag(&mut self, f: Flag, v: bool) {
        if v {
            self.f |= 1 << Registers::select_flag(f);
        } else {
            self.f &= !(1 << Registers::select_flag(f));
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn set_z_flag() {
        let mut r = Registers::new(false);
        r.flag(Flag::Z, true);
        assert_eq!(r.f, 0b10000000)
    }

    #[test]
    fn set_n_flag() {
        let mut r = Registers::new(false);
        r.flag(Flag::N, true);
        assert_eq!(r.f, 0b01000000)
    }

    #[test]
    fn set_h_flag() {
        let mut r = Registers::new(false);
        r.flag(Flag::H, true);
        assert_eq!(r.f, 0b00100000)
    }

    #[test]
    fn set_c_flag() {
        let mut r = Registers::new(false);
        r.flag(Flag::C, true);
        assert_eq!(r.f, 0b00010000)
    }

    #[test]
    fn reset_c_flag_only() {
        let mut r = Registers::new(false);
        r.f = 0b00010000;
        r.flag(Flag::C, false);
        assert_eq!(r.f, 0b00000000)
    }

    #[test]
    fn reset_z_and_c_flag_with_all_set() {
        let mut r = Registers::new(false);
        r.f = 0b11010000;
        r.flag(Flag::Z, false);
        r.flag(Flag::C, false);
        assert_eq!(r.f, 0b01000000)
    }
}
