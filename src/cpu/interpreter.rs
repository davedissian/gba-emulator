use std::rc::Rc;
use std::cell::RefCell;
use memory::*;
use cpu::{Cond, IndirectAddr};
use cpu::registers::*;
use cpu::ops::*;
use cpu::fetcher::*;

// CPU Data
pub struct Cpu {
    pub running: bool,
    memory: Rc<RefCell<Memory>>,
    pub regs: Registers,
    pub clock: Clock,
    pub last_instr_time: u16,

    interrupts_enabled: bool,
    halted: bool
}

pub struct Clock {
    pub m: u32, // 4.1kHz
    pub t: u32  // 1.0kHz
}

fn get_address(cpu: &Cpu, a: &IndirectAddr) -> u16 {
    match *a {
        IndirectAddr::BC => cpu.regs.bc(),
        IndirectAddr::DE => cpu.regs.de(),
        IndirectAddr::HL => cpu.regs.hl(),
        IndirectAddr::C => cpu.regs.c as u16 + 0xFF00,
        IndirectAddr::Imm8(n) => n as u16 + 0xFF00,
        IndirectAddr::Imm16(n) => n
    }
}

impl Cpu {
    pub fn new(memory: Rc<RefCell<Memory>>) -> Cpu {
        Cpu {
            running: true,
            memory: memory,
            regs: Registers::new(true),
            clock: Clock {
                m: 0,
                t: 0
            },
            last_instr_time: 0,
            interrupts_enabled: false,
            halted: false
        }
    }

    pub fn tick(&mut self) {
        self.check_for_interrupt();

        if self.halted {
            // Increase cycle count.
        } else {
            if self.regs.pc == 0x100 {
                self.memory.borrow_mut().set_boot_mode(false);
                println!("status: Finished booting");
            }

            self.last_instr_time = 0;
            //let pc = self.regs.pc;
            let instr = self.fetch_instr();
            //let state = self.dump_state_small();
            self.dispatch(instr);
            //println!("0x{:04x}\t{}\t{}\t{:?}", pc, state, self.last_instr_time, instr);
            self.clock.m = self.clock.m.wrapping_add(self.last_instr_time as u32);
            self.clock.t = self.clock.t.wrapping_add(self.last_instr_time as u32 * 4);
        }
    }

    fn check_for_interrupt(&mut self) {
        let interrupt_register = self.mem_read_u8(INTERRUPTS_ENABLED_REG);
        if interrupt_register & INTERRUPT_ENABLE_VBLANK != 0 {
            //println!("INTERRUPT_ENABLE_VBLANK")
        } else if interrupt_register & INTERRUPT_ENABLE_LCDC != 0 {
            println!("INTERRUPT_ENABLE_LCDC")
        } else if interrupt_register & INTERRUPT_ENABLE_TIMER != 0 {
            println!("INTERRUPT_ENABLE_TIMER")
        } else if interrupt_register & INTERRUPT_ENABLE_SERIAL_IO != 0 {
            println!("INTERRUPT_ENABLE_SERIAL_IO")
        } else {
            //println!("Warning: Unhandled register type");
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

    fn push_u16(&mut self, value: u16) {
        //println!("push {:x}", self.regs.sp);
        self.regs.sp = self.regs.sp.wrapping_sub(2);
        unborrow!(self.mem_write_u16(self.regs.sp, value));
    }

    fn pop_u16(&mut self) -> u16 {
        let result = unborrow!(self.mem_read_u16(self.regs.sp));
        //println!("pop {:x}", self.regs.sp);
        self.regs.sp = self.regs.sp.wrapping_add(2);
        result
    }

    // Cycles
    fn read_cycle(&mut self) {
        self.last_instr_time += 1;
    }

    fn write_cycle(&mut self) {
        self.last_instr_time += 1;
    }

    fn internal_cycle(&mut self) {
        self.last_instr_time += 1;
    }

    // Memory reading helper functions
    fn mem_read_u8(&mut self, addr: u16) -> u8 {
        self.read_cycle();
        self.memory.borrow().read_u8(addr)
    }

    fn mem_read_u16(&mut self, addr: u16) -> u16 {
        self.read_cycle();
        self.read_cycle();
        self.memory.borrow().read_u16(addr)
    }

    fn mem_write_u8(&mut self, addr: u16, data: u8) {
        self.write_cycle();
        self.memory.borrow_mut().write_u8(addr, data);
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        self.write_cycle();
        self.write_cycle();
        self.memory.borrow_mut().write_u16(addr, data);
    }

    pub fn dump_state(&self) {
        println!("Registers:");
        println!("- PC: {:04X} SP: {:04X} ", self.regs.pc, self.regs.sp);
        println!("- A: {:02X} F: {:02X} B: {:02X} C: {:02X}", self.regs.a, self.regs.f, self.regs.b, self.regs.c);
        println!("- D: {:02X} E: {:02X} H: {:02X} L: {:02X}", self.regs.d, self.regs.e, self.regs.h, self.regs.l);
        println!("Flags:");
        println!("- Zero: {}", self.regs.get_flag(Flag::Z));
        println!("- Add/Sub: {}", self.regs.get_flag(Flag::N));
        println!("- Half Carry: {}", self.regs.get_flag(Flag::H));
        println!("- Carry Flag: {}", self.regs.get_flag(Flag::C));
    }

    pub fn dump_state_small(&self) -> String {
        format!("sp:{:04x} a:{:02x} f:{:02x} b:{:02x} c:{:02x} d:{:02x} e:{:02x} h:{:02x} l:{:02x} znhc:{:04b}",
            self.regs.sp,
            self.regs.a,
            self.regs.f,
            self.regs.b,
            self.regs.c,
            self.regs.d,
            self.regs.e,
            self.regs.h,
            self.regs.l,
            self.regs.f >> 4)
    }
}

impl Fetcher for Cpu {
    fn fetch_u8(&mut self) -> u8 {
        let byte = unborrow!(self.mem_read_u8(self.regs.pc));
        self.regs.pc += 1;
        byte
    }

    fn fetch_u16(&mut self) -> u16 {
        let word = unborrow!(self.mem_read_u16(self.regs.pc));
        self.regs.pc += 2;
        word
    }
}
    
// Interpreter implementation of the CPU ops defined in the ops module
impl CpuOps for Cpu {
    fn read_arg8(&mut self, arg: Arg8) -> u8 {
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

    fn read_arg16(&mut self, arg: Arg16) -> u16 {
        match arg {
            Arg16::Reg(r) => match r {
                Reg16::AF => self.regs.af(),
                Reg16::BC => self.regs.bc(),
                Reg16::DE => self.regs.de(),
                Reg16::HL => self.regs.hl(),
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
                Reg16::AF => self.regs.set_af(data),
                Reg16::BC => self.regs.set_bc(data),
                Reg16::DE => self.regs.set_de(data),
                Reg16::HL => self.regs.set_hl(data),
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
        unborrow!(self.regs.set_hl(self.regs.hl().wrapping_sub(1)));
    }

    fn ldi(&mut self, o: Arg8, i: Arg8) {
        self.ld(o, i);
        unborrow!(self.regs.set_hl(self.regs.hl().wrapping_add(1)));
    }

    fn ldh(&mut self, o: Arg8, i: Arg8){
        // Add 0xFF00 to any immediate input/output addresses
        let o = match o {
            Arg8::Ind(IndirectAddr::Imm8(v)) => Arg8::Ind(IndirectAddr::Imm16(v as u16 | 0xFF00)),
            _ => o
        };
        let i = match i {
            Arg8::Ind(IndirectAddr::Imm8(v)) => Arg8::Ind(IndirectAddr::Imm16(v as u16 | 0xFF00)),
            _ => i
        };
        self.ld(o, i);
    }

    fn ld16(&mut self, o: Arg16, i: Arg16) {
        let value = self.read_arg16(i);
        self.write_arg16(o, value);
        // LD SP, HL is special and takes an internal cycle to execute.
        if o == Arg16::Reg(Reg16::SP) && i == Arg16::Reg(Reg16::HL) {
            self.internal_cycle();
        }
    }

    fn ldhl16(&mut self, offset: i8) {
        // Sign extend operand then convert to unsigned (preserving sign bits).
        let operand = offset as i16 as u16;
        self.regs.flag(Flag::Z, false);
        self.regs.flag(Flag::N, false);
        unborrow!(self.regs.flag(Flag::H, (self.regs.sp & 0x000F) + (operand & 0x000F) > 0x000F));
        unborrow!(self.regs.flag(Flag::C, (self.regs.sp & 0x00FF) + (operand & 0x00FF) > 0x00FF));
        unborrow!(self.regs.set_hl(self.regs.sp.wrapping_add(operand)));
        self.internal_cycle();
    }

    fn push(&mut self, i: Arg16) {
        let content = self.read_arg16(i);
        self.internal_cycle();
        self.push_u16(content);
    }

    fn pop(&mut self, o: Arg16) {
        let value = self.pop_u16();
        self.write_arg16(o, value);
    }

    fn add(&mut self, i: Arg8) {
        let a = self.regs.a;
        let operand = self.read_arg8(i);
        let result = a.wrapping_add(operand);
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, (a & 0xF + operand & 0xF) > 0xF);
        self.regs.flag(Flag::C, ((a as u16) + (operand as u16)) > 0xFF);
        self.regs.a = result;
    }

    fn adc(&mut self, i: Arg8) {
        let a = self.regs.a;
        let operand = self.read_arg8(i);
        let carry: u8 = if self.regs.get_flag(Flag::C) { 1 } else { 0 };
        let result = a.wrapping_add(operand).wrapping_add(carry);
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, (a & 0xF + operand & 0xF + carry) > 0xF);
        self.regs.flag(Flag::C, ((a as u16) + (operand as u16) + (carry as u16)) > 0xFF);
        self.regs.a = result;
    }

    fn sub(&mut self, i: Arg8) {
        let a = self.regs.a;
        let operand = self.read_arg8(i);
        let result = a.wrapping_sub(operand);
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, true);
        self.regs.flag(Flag::H, (a & 0xF) < (operand & 0xF));
        self.regs.flag(Flag::C, (a as u16) < (operand as u16));
        self.regs.a = result;
    }

    fn sbc(&mut self, i: Arg8) {
        let a = self.regs.a;
        let operand = self.read_arg8(i);
        let carry: u8 = if self.regs.get_flag(Flag::C) { 1 } else { 0 };
        let result = a.wrapping_sub(operand).wrapping_sub(carry);
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, true);
        self.regs.flag(Flag::H, (a & 0xF) < (operand & 0xF + carry));
        self.regs.flag(Flag::C, (a as u16) < ((operand as u16) + (carry as u16)));
        self.regs.a = result;
    }

    fn and(&mut self, i: Arg8) {
        self.regs.a &= self.read_arg8(i);
        let result = self.regs.a;
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, true);
        self.regs.flag(Flag::C, false);
    }

    fn or(&mut self, i: Arg8) {
        self.regs.a |= self.read_arg8(i);
        let result = self.regs.a;
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, false);
        self.regs.flag(Flag::C, false);
    }

    fn xor(&mut self, i: Arg8) {
        self.regs.a ^= self.read_arg8(i);
        let result = self.regs.a;
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, false);
        self.regs.flag(Flag::C, false);
    }

    fn cp(&mut self, i: Arg8) {
        // Reuse sub but preserve register value
        let old_a = self.regs.a;
        self.sub(i);
        self.regs.a = old_a;
    }

    fn inc(&mut self, io: Arg8) {
        let result = self.read_arg8(io).wrapping_add(1);
        self.write_arg8(io, result);
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, (result & 0xF) + 1 > 0xF);
    }

    fn dec(&mut self, io: Arg8) {
        let result = self.read_arg8(io).wrapping_sub(1);
        self.write_arg8(io, result);
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, true);
        self.regs.flag(Flag::H, (result & 0xF) == 0);
    }

    fn add16(&mut self, i: Arg16) {
        let operand = self.read_arg16(i);
        let result = self.regs.hl().wrapping_add(operand);
        self.regs.flag(Flag::N, false);
        unborrow!(self.regs.flag(Flag::H, (self.regs.hl() & 0xFFF + operand & 0xFFF) > 0xFFF));
        unborrow!(self.regs.flag(Flag::C, self.regs.hl() > (0xFFFF - operand)));
        self.regs.set_hl(result);
        self.internal_cycle();
    }

    fn add16sp(&mut self, i: i8) {
        // Sign extend operand then convert to unsigned (preserving sign bits).
        let operand = i as i16 as u16;
        self.regs.flag(Flag::Z, false);
        self.regs.flag(Flag::N, false);
        unborrow!(self.regs.flag(Flag::H, (self.regs.sp & 0x000F) + (operand & 0x000F) > 0x000F));
        unborrow!(self.regs.flag(Flag::C, (self.regs.sp & 0x00FF) + (operand & 0x00FF) > 0x00FF));
        self.regs.sp = self.regs.sp.wrapping_add(operand);
        self.internal_cycle();
        self.internal_cycle();
    }

    fn inc16(&mut self, io: Arg16) {
        let result = self.read_arg16(io).wrapping_add(1);
        self.write_arg16(io, result);
        self.internal_cycle();
    }

    fn dec16(&mut self, io: Arg16) {
        let result = self.read_arg16(io).wrapping_sub(1);
        self.write_arg16(io, result);
        self.internal_cycle();
    }

    // misc
    fn nop(&mut self) {}

    fn daa(&mut self) {
        let mut a = self.regs.a;
        let mut adjust = if self.regs.get_flag(Flag::C) { 0x60 } else { 0x00 };
        if self.regs.get_flag(Flag::H) {
            adjust |= 0x06;
        }
        if !self.regs.get_flag(Flag::N) {
            if a & 0x0F > 0x09 { adjust |= 0x06; };
            if a > 0x99 { adjust |= 0x60; };
            a = a.wrapping_add(adjust);
        } else {
            a = a.wrapping_sub(adjust);
        }

        self.regs.flag(Flag::C, adjust >= 0x60);
        self.regs.flag(Flag::H, false);
        self.regs.flag(Flag::Z, a == 0);
        self.regs.a = a;
    }

    fn cpl(&mut self) {
        self.regs.a = !self.regs.a;
        self.regs.flag(Flag::N, true);
        self.regs.flag(Flag::H, true);
    }

    fn ccf(&mut self) {
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, false);
        unborrow!(self.regs.flag(Flag::C, !self.regs.get_flag(Flag::C)));
    }

    fn scf(&mut self) {
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, false);
        self.regs.flag(Flag::C, true);
    }

    fn halt(&mut self) {
        println!("HALTED UNTIL NEXT INTERRUPT");
        self.halted = true;
    }

    fn stop(&mut self) {
        println!("STOPPED");
    }

    fn ei(&mut self) {
        self.interrupts_enabled = true;
    }

    fn di(&mut self) {
        self.interrupts_enabled = false;
    }

    // rotate and shift
    fn rlc(&mut self, io: Arg8) {
        // Rotate left, update carry flag
        let value = self.read_arg8(io);
        let msb = value >> 7;
        let result = (value << 1) + msb;
        self.write_arg8(io, result);
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, false);
        self.regs.flag(Flag::C, msb == 1);
    }

    fn rl(&mut self, io: Arg8) {
        // Rotate left through carry flag (9 bit rotation)
        let value = self.read_arg8(io);
        let result = value << 1 + if self.regs.get_flag(Flag::C) { 1 } else { 0 };
        self.write_arg8(io, result);
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, false);
        self.regs.flag(Flag::C, value & 0x80 == 0x80);
    }

    fn rrc(&mut self, io: Arg8) {
        let value = self.read_arg8(io);
        let lsb = value & 0x1;
        let result = (value >> 1) + (lsb << 7);
        self.write_arg8(io, result);
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, false);
        self.regs.flag(Flag::C, lsb == 1);
    }

    fn rr(&mut self, io: Arg8) {
        let value = self.read_arg8(io);
        let result = (value >> 1) + if self.regs.get_flag(Flag::C) { 0x80 } else { 0 };
        self.write_arg8(io, result);
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, false);
        self.regs.flag(Flag::C, value & 0x1 == 0x1);
    }

    fn sla(&mut self, io: Arg8) {
        let value = self.read_arg8(io);
        let result = value << 1;
        self.write_arg8(io, result);
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, false);
        self.regs.flag(Flag::C, value & 0x80 == 0x80);
    }

    fn sra(&mut self, io: Arg8) {
        let value = self.read_arg8(io);
        let result = value >> 1 | value & 0x80;
        self.write_arg8(io, result);
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, false);
        self.regs.flag(Flag::C, value & 0x1 == 0x1);
    }

    fn swap(&mut self, io: Arg8) {
        let initial = self.read_arg8(io);
        self.write_arg8(io, ((initial >> 4) & 0xF) | ((initial << 4) & 0xF));
    }

    fn srl(&mut self, io: Arg8) {
        let value = self.read_arg8(io);
        let result = value >> 1;
        self.write_arg8(io, result);
        self.regs.flag(Flag::Z, result == 0);
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, false);
        self.regs.flag(Flag::C, value & 0x1 == 0x1);
    }

    // bit manipulation
    fn bit(&mut self, bit_id: u8, i: Arg8) {
        if bit_id > 7 {
            panic!("Bit index out of bounds");
        }
        let value = self.read_arg8(i);
        self.regs.flag(Flag::Z, value & (1 << bit_id) == 0);
        self.regs.flag(Flag::N, false);
        self.regs.flag(Flag::H, true);
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
        let dest_addr = self.read_arg16(dest);
        if self.check_condition(cond) {
            self.regs.pc = dest_addr;
            self.internal_cycle();
        }
    }

    fn jr(&mut self, cond: Cond, offset: i8) {
        if self.check_condition(cond) {
            self.regs.pc = ((self.regs.pc as i32) + offset as i32) as u16;
            self.internal_cycle();
        }
    }

    fn call(&mut self, cond: Cond, dest: Arg16) {
        if self.check_condition(cond) {
            // The current value of the program counter is the _next_ instruction, so push that to
            // the stack.
            unborrow!(self.push_u16(self.regs.pc));
            self.regs.pc = self.read_arg16(dest);
            self.internal_cycle();
        }
    }

    fn rst(&mut self, offset: u8) {
        unborrow!(self.push_u16(self.regs.pc));
        self.regs.pc = offset as u16;
        self.internal_cycle();
    }

    fn ret(&mut self, cond: Cond) {
        self.internal_cycle();
        if self.check_condition(cond) {
            self.regs.pc = self.pop_u16();
            self.internal_cycle();
        }
    }

    fn reti(&mut self) {
        self.ret(Cond::None);
        self.ei();
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
        // 0x90
        144u8
    }

    fn test_u16() -> u16 {
        // 0xBA0C
        47628u16
    }

    fn init_cpu() -> Cpu {
        Cpu::new(Rc::new(RefCell::new(Memory::new())))
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
        assert_eq!(cpu.regs.bc(), test_u16());
        assert_eq!(cpu.regs.bc(), cpu.regs.de());
    }
}
