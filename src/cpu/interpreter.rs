use std::rc::Rc;
use std::cell::RefCell;
use memory::Memory;
use cpu::{Cond, IndirectAddr};
use cpu::registers::*;
use cpu::ops::*;
use cpu::fetcher::*;

// CPU Data
pub struct Cpu {
    pub running: bool,
    memory: Rc<RefCell<Memory>>,
    regs: Registers
}

// Registers
macro_rules! read_reg_pair {
    ($regs:expr, $h:ident, $l:ident) => {
        (($regs.$h as u16) << 8) | $regs.$l as u16
    };
}

macro_rules! write_reg_pair {
    ($h:expr, $l:expr, $v:expr) => {{
        $h = ($v >> 8) as u8;
        $l = ($v & 0xFF) as u8;
    }};
}

fn get_address(cpu: &Cpu, a: &IndirectAddr) -> u16 {
    match *a {
        IndirectAddr::BC => read_reg_pair!(cpu.regs, b, c),
        IndirectAddr::DE => read_reg_pair!(cpu.regs, d, e),
        IndirectAddr::HL => read_reg_pair!(cpu.regs, h, l),
        IndirectAddr::C => cpu.regs.c as u16 + 0xFF00,
        IndirectAddr::Imm8(n) => n as u16 + 0xFF00,
        IndirectAddr::Imm16(n) => n
    }
}

impl Fetcher for Cpu {
    fn fetch_word(&mut self) -> u8 {
        let byte = self.mem_read_u8(self.regs.pc);
        self.regs.pc += 1;
        byte
    }
}
    
// Interpreter implementation of the CPU ops defined in the ops module
#[allow(unused_variables)]
impl<'a> CpuOps for &'a mut Cpu {
    fn read_arg8(&self, arg: Arg8) -> u8 {
        match arg {
            Arg8::Reg(r) => match r {
                Reg8::A => self.regs.a,
                Reg8::B => self.regs.b,
                Reg8::C => self.regs.c,
                Reg8::D => self.regs.d,
                Reg8::E => self.regs.e,
                Reg8::F => self.regs.f,
                Reg8::H => self.regs.h,
                Reg8::L => self.regs.l
            },

            Arg8::Ind(addr) => {
                let addr = get_address(self, &addr);
                self.mem_read_u8(addr)
            }

            Arg8::Imm(v) => v
        }
    }

    fn write_arg8(&mut self, arg: Arg8, data: u8) {
        match arg {
            Arg8::Reg(r) => match r {
                Reg8::A => self.regs.a = data,
                Reg8::B => self.regs.b = data,
                Reg8::C => self.regs.c = data,
                Reg8::D => self.regs.d = data,
                Reg8::E => self.regs.e = data,
                Reg8::F => self.regs.f = data,
                Reg8::H => self.regs.h = data,
                Reg8::L => self.regs.l = data
            },

            Arg8::Ind(addr) => {
                let addr = get_address(self, &addr);
                self.mem_write_u8(addr, data);
            },

            _ => panic!("Cannot write to {:?}", arg)
        }
    }

    fn read_arg16(&self, arg: Arg16) -> u16 {
        match arg {
            Arg16::Reg(r) => match r {
                Reg16::AF => read_reg_pair!(self.regs, a, f),
                Reg16::BC => read_reg_pair!(self.regs, b, c),
                Reg16::DE => read_reg_pair!(self.regs, d, e),
                Reg16::HL => read_reg_pair!(self.regs, h, l),
                Reg16::SP => self.regs.sp,
                Reg16::PC => self.regs.pc,
            },

            Arg16::Ind(addr) => {
                let addr = get_address(self, &addr);
                self.mem_read_u16(addr)
            },

            Arg16::Imm(v) => v
        }
    }

    fn write_arg16(&mut self, arg: Arg16, data: u16) {
        match arg {
            Arg16::Reg(r) => match r {
                Reg16::AF => write_reg_pair!(self.regs.a, self.regs.f, data),
                Reg16::BC => write_reg_pair!(self.regs.b, self.regs.c, data),
                Reg16::DE => write_reg_pair!(self.regs.d, self.regs.e, data),
                Reg16::HL => write_reg_pair!(self.regs.h, self.regs.l, data),
                Reg16::SP => self.regs.sp = data,
                Reg16::PC => self.regs.pc = data,
            },

            Arg16::Ind(addr) => {
                let addr = get_address(self, &addr);
                self.mem_write_u16(addr, data);
            },

            _ => panic!("Cannot write to {:?}", arg)
        }
    }

    fn ld(&mut self, o: Arg8, i: Arg8) {
        let value = self.read_arg8(i);
        self.write_arg8(o, value);
    }
    
    fn ldd(&mut self, o: Arg8, i: Arg8) {
        self.ld(o, i);
        self.dec16(Arg16::Reg(Reg16::HL));
    }

    fn ldi(&mut self, o: Arg8, i: Arg8) {
        self.ld(o, i);
        self.inc16(Arg16::Reg(Reg16::HL));
    }

    fn ldh(&mut self, o: Arg8, i: Arg8){
        // Add 0xFF00 to any immediate input/output addresses
        let o = match o {
            Arg8::Ind(IndirectAddr::Imm8(v)) => Arg8::Ind(IndirectAddr::Imm16(v as u16 + 0xFF00)),
            _ => o
        };
        let i = match i {
            Arg8::Ind(IndirectAddr::Imm8(v)) => Arg8::Ind(IndirectAddr::Imm16(v as u16 + 0xFF00)),
            _ => i
        };
        self.ld(o, i);
    }

    fn ld16(&mut self, o: Arg16, i: Arg16) {
        let value = self.read_arg16(i);
        self.write_arg16(o, value);
    }

    fn ldhl16(&mut self, offset: i8) {
        let result: u16 = if offset < 0 {
            self.regs.sp - (-offset as u16)
        } else {
            self.regs.sp + (offset as u16)
        };
        write_reg_pair!(self.regs.h, self.regs.l, result);
        self.regs.reset_flag(Flag::Z);
        self.regs.reset_flag(Flag::N);
        // TODO(David): Why does this work?
        let temp: u16 = read_reg_pair!(self.regs, h, l) ^ self.regs.sp ^ result;
        self.regs.update_flag(Flag::H, temp & 0x10 == 0x10);
        self.regs.update_flag(Flag::C, temp & 0x100 == 0x100);
    }

    fn push(&mut self, i: Arg16) {
        let content = self.read_arg16(i);
        unborrow!(self.mem_write_u16(self.regs.sp, content));
        self.regs.sp -= 2;
    }

    fn pop(&mut self, o: Arg16) {
        self.regs.sp += 2;
        let value = self.mem_read_u16(self.regs.sp);
        self.write_arg16(o, value);
    }

    fn add(&mut self, i: Arg8) {
        let operand: u8 = self.read_arg8(i);
        let result: u16 = self.regs.a as u16 + operand as u16;
        self.regs.a = result as u8;
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.reset_flag(Flag::N);
        unborrow!(self.regs.update_flag(Flag::H, (self.regs.a & 0xF + operand & 0xF) > 0xF));
        self.regs.update_flag(Flag::C, result > 0xFF);
    }

    fn adc(&mut self, i: Arg8) {
        let operand: u8 = self.read_arg8(i);
        let carry: u8 = if self.regs.get_flag(Flag::C) { 1 } else { 0 };
        let result: u16 = self.regs.a as u16 + operand as u16 + carry as u16;
        self.regs.a = result as u8;
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.reset_flag(Flag::N);
        unborrow!(self.regs.update_flag(Flag::H, (self.regs.a & 0xF + operand & 0xF) > 0xF));
        self.regs.update_flag(Flag::C, result > 0xFF);
    }

    fn sub(&mut self, i: Arg8) {
        let operand: u8 = self.read_arg8(i);
        let result: u8 = self.regs.a - operand;
        self.regs.a = result;
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.set_flag(Flag::N);
        unborrow!(self.regs.update_flag(Flag::H, (self.regs.a & 0xF) < (operand & 0xF)));
        unborrow!(self.regs.update_flag(Flag::C, self.regs.a < operand));
    }

    fn sbc(&mut self, i: Arg8) {
        let operand: u8 = self.read_arg8(i);
        let carry: u8 = if self.regs.get_flag(Flag::C) { 1 } else { 0 };
        let result: u8 = self.regs.a - operand - carry;
        self.regs.a = result;
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.set_flag(Flag::N);
        unborrow!(self.regs.update_flag(Flag::H, (self.regs.a & 0xF) < (operand & 0xF + carry)));
        // We need to be careful in case operand is 11111111 and carry is 1, therefore casting to
        // u16 is required to prevent u8 overflow!
        unborrow!(self.regs.update_flag(Flag::C, (self.regs.a as u16) < (operand as u16 + carry as u16)));
    }

    fn and(&mut self, i: Arg8) {
        self.regs.a &= self.read_arg8(i);
        let result = self.regs.a;
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.reset_flag(Flag::N);
        self.regs.set_flag(Flag::H);
        self.regs.reset_flag(Flag::C);
    }

    fn or(&mut self, i: Arg8) {
        self.regs.a |= self.read_arg8(i);
        let result = self.regs.a;
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.reset_flag(Flag::N);
        self.regs.reset_flag(Flag::H);
        self.regs.reset_flag(Flag::C);
    }

    fn xor(&mut self, i: Arg8) {
        self.regs.a ^= self.read_arg8(i);
        let result = self.regs.a;
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.reset_flag(Flag::N);
        self.regs.reset_flag(Flag::H);
        self.regs.reset_flag(Flag::C);
    }

    fn cp(&mut self, i: Arg8) {
        // Reuse sub but preserve register value
        let old_a = self.regs.a;
        self.sub(i);
        self.regs.a = old_a;
    }

    fn inc(&mut self, io: Arg8) {
        let result = self.read_arg8(io) + 1;
        self.write_arg8(io, result);
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.reset_flag(Flag::N);
        self.regs.update_flag(Flag::H, (result & 0xF + 1) > 0xF);
    }

    fn dec(&mut self, io: Arg8) {
        let result = self.read_arg8(io) - 1;
        self.write_arg8(io, result);
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.set_flag(Flag::N);
        self.regs.update_flag(Flag::H, ((result - 1) & 0xF) == 0xF);
    }

    fn add16(&mut self, i: Arg16) {
        let operand: u16 = self.read_arg16(i);
        let result: u32 = read_reg_pair!(self.regs, h, l) as u32 + operand as u32;
        write_reg_pair!(self.regs.h, self.regs.l, result as u16);
        self.regs.reset_flag(Flag::N);
        unborrow!(self.regs.update_flag(Flag::H, (read_reg_pair!(self.regs, h, l) & 0xFFF +
                                                  operand & 0xFFF) > 0xFFF));
        self.regs.update_flag(Flag::C, result > 0xFFFF);
    }

    fn add16sp(&mut self, i: i8) {
        let result: u16 = if i < 0 {
            self.regs.sp - (-i as u16)
        } else {
            self.regs.sp + (i as u16)
        };
        self.regs.sp = result;
        self.regs.reset_flag(Flag::Z);
        self.regs.reset_flag(Flag::N);
        // TODO(David): Why does this work?
        let temp: u16 = self.regs.sp ^ (i as u16) ^ result;
        self.regs.update_flag(Flag::H, temp & 0x10 == 0x10);
        self.regs.update_flag(Flag::C, temp & 0x100 == 0x100);
    }

    fn inc16(&mut self, io: Arg16) {
        let result = self.read_arg16(io) + 1;
        self.write_arg16(io, result);
    }

    fn dec16(&mut self, io: Arg16) {
        let result = self.read_arg16(io) - 1;
        self.write_arg16(io, result);
    }

    // misc
    fn nop(&mut self) {}

    fn daa(&mut self) {
        // TODO(David): Ambiguous spec, test this
        // A stores a number up to 255. In BCD form each nibble would store a single digit,
        // therefore the maximum number that can be stored is 99.

        // Source:
        // The DAA instruction corrects this invalid result. It checks to see if there was a carry
        // out of the low order BCD digit and adjusts the value (by adding six to it) if there was
        // an overflow. After adjusting for overflow out of the L.O. digit, the DAA instruction
        // repeats this process for the H.O. digit. DAA sets the carry flag if the was a (decimal)
        // carry out of the H.O. digit of the operation.
    }

    fn cpl(&mut self) {
        self.regs.a = !self.regs.a;
        self.regs.set_flag(Flag::N);
        self.regs.set_flag(Flag::H);
    }

    fn ccf(&mut self) {
        self.regs.reset_flag(Flag::N);
        self.regs.reset_flag(Flag::H);
        unborrow!(self.regs.update_flag(Flag::C, !self.regs.get_flag(Flag::C)));
    }

    fn scf(&mut self) {
        self.regs.reset_flag(Flag::N);
        self.regs.reset_flag(Flag::H);
        self.regs.set_flag(Flag::C);
    }

    fn halt(&mut self) {
        println!("HALTED UNTIL NEXT INTERRUPT");
    }

    fn stop(&mut self) {
        println!("STOPPED");
    }

    fn ei(&mut self) {
    }

    fn di(&mut self) {
    }

    // rotate and shift
    fn rlc(&mut self, io: Arg8) {
        // Rotate left, update carry flag
        let value = self.read_arg8(io);
        let msb = value >> 7;
        let result = (value << 1) + msb;
        self.write_arg8(io, result);
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.reset_flag(Flag::N);
        self.regs.reset_flag(Flag::H);
        self.regs.update_flag(Flag::C, msb == 1);
    }

    fn rl(&mut self, io: Arg8) {
        // Rotate left through carry flag (9 bit rotation)
        let value = self.read_arg8(io);
        let result = value << 1 + if self.regs.get_flag(Flag::C) { 1 } else { 0 };
        self.write_arg8(io, result);
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.reset_flag(Flag::N);
        self.regs.reset_flag(Flag::H);
        self.regs.update_flag(Flag::C, value & 0x80 == 0x80);
    }

    fn rrc(&mut self, io: Arg8) {
        let value = self.read_arg8(io);
        let lsb = value & 0x1;
        let result = (value >> 1) + (lsb << 7);
        self.write_arg8(io, result);
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.reset_flag(Flag::N);
        self.regs.reset_flag(Flag::H);
        self.regs.update_flag(Flag::C, lsb == 1);
    }

    fn rr(&mut self, io: Arg8) {
        let value = self.read_arg8(io);
        let result = (value >> 1) + if self.regs.get_flag(Flag::C) { 0x80 } else { 0 };
        self.write_arg8(io, result);
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.reset_flag(Flag::N);
        self.regs.reset_flag(Flag::H);
        self.regs.update_flag(Flag::C, value & 0x1 == 0x1);
    }

    fn sla(&mut self, io: Arg8) {
        let value = self.read_arg8(io);
        let result = value << 1;
        self.write_arg8(io, result);
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.reset_flag(Flag::N);
        self.regs.reset_flag(Flag::H);
        self.regs.update_flag(Flag::C, value & 0x80 == 0x80);
    }

    fn sra(&mut self, io: Arg8) {
        let value = self.read_arg8(io);
        let result = value >> 1 | value & 0x80;
        self.write_arg8(io, result);
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.reset_flag(Flag::N);
        self.regs.reset_flag(Flag::H);
        self.regs.update_flag(Flag::C, value & 0x1 == 0x1);
    }

    fn swap(&mut self, io: Arg8) {
        let initial = self.read_arg8(io);
        self.write_arg8(io, ((initial >> 4) & 0xF) | ((initial << 4) & 0xF));
    }

    fn srl(&mut self, io: Arg8) {
        let value = self.read_arg8(io);
        let result = value >> 1;
        self.write_arg8(io, result);
        self.regs.update_flag(Flag::Z, result == 0);
        self.regs.reset_flag(Flag::N);
        self.regs.reset_flag(Flag::H);
        self.regs.update_flag(Flag::C, value & 0x1 == 0x1);
    }

    // bit manipulation
    fn bit(&mut self, bit_id: u8, i: Arg8) {
        if bit_id > 7 {
            panic!("Bit index out of bounds");
        }
        let value = self.read_arg8(i);
        self.regs.update_flag(Flag::Z, (value >> bit_id) & 0x1 == 0);
        self.regs.reset_flag(Flag::N);
        self.regs.set_flag(Flag::H);
    }

    fn set(&mut self, bit_id: u8, io: Arg8) {
        if bit_id > 7 {
            panic!("Bit index out of bounds");
        }
        let value = self.read_arg8(io);
        self.write_arg8(io, value | (1 << bit_id));
    }

    fn res(&mut self, bit_id: u8, io: Arg8) {
        if bit_id > 7 {
            panic!("Bit index out of bounds");
        }
        let value = self.read_arg8(io);
        self.write_arg8(io, value & !(1 << bit_id));
    }

    // control
    fn jp(&mut self, cond: Cond, dest: Arg16) {
        if self.check_condition(cond) {
            self.regs.pc = self.read_arg16(dest);
        } 
    }

    fn jr(&mut self, cond: Cond, offset: i8) {
        if self.check_condition(cond) {
            if offset < 0 {
                self.regs.pc -= -offset as u16;
            } else {
                self.regs.pc += offset as u16;
            }
        }
    }

    fn call(&mut self, cond: Cond, dest: Arg16) {
        // CALL is implemented as PUSH SP; JP cond dest
        if self.check_condition(cond) {
            unborrow!(self.push(Arg16::Imm(self.regs.pc + 3))); // CALL is 3 bytes
            self.jp(Cond::None, dest);
        }
    }

    fn rst(&mut self, offset: u8) {
        unborrow!(self.push(Arg16::Imm(self.regs.pc)));
        self.jp(Cond::None, Arg16::Imm(offset as u16));
    }

    fn ret(&mut self, cond: Cond) {
        if self.check_condition(cond) {
            self.regs.sp += 2;
            self.regs.pc = self.mem_read_u16(self.regs.sp);
        }
    }

    fn reti(&mut self) {
        self.ret(Cond::None);
        self.ei();
    }
}

impl Cpu {
    pub fn new(memory: Rc<RefCell<Memory>>) -> Cpu {
        Cpu {
            running: true,
            memory: memory,
            regs: Registers::new()
        }
    }

    pub fn tick(&mut self) {
        let instr = self.fetch_instr();
        println!("{:x} {:?}", self.regs.pc, instr);
        //self.dispatch(instr);
        (&mut (*self)).dispatch(instr); // <- wtf?

        // Stop execution for the lols
        if self.regs.pc > 256 {
            self.running = false;
            self.dump_state();
        }
    }

    // Instruction helpers
    fn check_condition(&self, cond: Cond) -> bool {
        match cond {
            Cond::None => true,
            Cond::NZ => !self.regs.get_flag(Flag::Z),
            Cond::Z => self.regs.get_flag(Flag::Z),
            Cond::NC => !self.regs.get_flag(Flag::C),
            Cond::C => self.regs.get_flag(Flag::C)
        }
    }

    // Memory reading helper functions
    fn mem_read_u8(&self, addr: u16) -> u8 {
        self.memory.borrow().read_u8(addr)
    }

    fn mem_read_u16(&self, addr: u16) -> u16 {
        let l = self.mem_read_u8(addr);
        let h = self.mem_read_u8(addr + 1);
        ((l as u16) << 8) | (h as u16)
    }

    fn mem_write_u8(&mut self, addr: u16, data: u8) {
        self.memory.borrow_mut().write_u8(addr, data);
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        self.memory.borrow_mut().write_u16(addr, data);
    }

    pub fn dump_state(&self) {
        println!("Registers:");
        println!("- PC: {:04x} SP: {:04x} ", self.regs.pc, self.regs.sp);
        println!("- A: {:02x} F: {:02x} B: {:02x} C: {:02x}", self.regs.a, self.regs.f, self.regs.b, self.regs.c);
        println!("- D: {:02x} E: {:02x} H: {:02x} L: {:02x}", self.regs.d, self.regs.e, self.regs.h, self.regs.l);
        println!("Flags:");
        println!("- Zero: {}", self.regs.get_flag(Flag::Z));
        println!("- Add/Sub: {}", self.regs.get_flag(Flag::N));
        println!("- Half Carry: {}", self.regs.get_flag(Flag::H));
        println!("- Carry Flag: {}", self.regs.get_flag(Flag::C));
    }
}

// Test cases
#[cfg(test)]
mod test {
    use std::rc::Rc;
    use std::cell::RefCell;
    use memory::Memory;
    use super::*;
    use cpu::ops::*;
    use cpu::registers::Reg8::*;
    use cpu::registers::Reg16::*;

    fn test_u8() -> u8 {
        144u8
    }

    fn test_u16() -> u16 {
        47628u16
    }

    fn init_cpu() -> Cpu {
        Cpu::new(Rc::new(RefCell::new(Memory::new_blank())))
    }

    #[test]
    fn ld_a_then_b() {
        let mut cpu = &mut init_cpu();
        cpu.ld(Arg8::Reg(A), Arg8::Imm(test_u8()));
        cpu.ld(Arg8::Reg(B), Arg8::Reg(A));
        assert_eq!(cpu.regs.a, test_u8());
        assert_eq!(cpu.regs.a, cpu.regs.b);
    }

    #[test]
    fn ld16_bc_then_de() {
        let mut cpu = &mut init_cpu();
        cpu.ld16(Arg16::Reg(BC), Arg16::Imm(test_u16()));
        cpu.ld16(Arg16::Reg(DE), Arg16::Reg(BC));
        assert_eq!(read_reg_pair!(cpu.regs, b, c), test_u16());
        assert_eq!(read_reg_pair!(cpu.regs, b, c), read_reg_pair!(cpu.regs, d, e));
    }
}
