mod registers;
mod ops;

use std::rc::Rc;
use std::cell::RefCell;
use memory::Memory;
use cpu::registers::*;
use cpu::ops::*;

/*
    Instruction Layout
    ====================

    This analysis is based upon the Zilog Z80 manual with modifications done by the GB/GBC
    such as removing the instructions involving the IX and IY registers.

    [] = 1 byte

    Addressing Modes
    --------------------
    Immediate Addressing: [op][operand] (1 or 2 byte op code)
        Operand is a single byte, such as loading the accumulator with a constant.
    Immediate Extended Addressing: [op][low][high] (1 or 2 byte op code)
        Operand is split into two bytes, such as to load the HL register pair with 16 bits
        of data.
    Register Addressing:
        Many Z80 opcodes specify register to registers directly
    Implied Addressing:
        This indicates OP codes imply registers, such as arithmetic instructions always
        implying that the destination is the accumulator (A in AF register).
    Register Indirect Addressing: [op] (1 or 2 byte op code)
        This specifies a 16 bit register pair to be used as a pointer to any location in
        memory. Such as loading the accumulator with data pointed to in the HR register.


    Modified Page Zero Addressing: [op] (1 byte op code)
        Eight special locations in page 0 of memory, depending on which version of CALL is
        called.
    Relative Addressing: [op][displacement] (1 byte op code)
        Used for Jump Relative, displacement is 8 bit twos complement offset from A+2 (where
        A is the current PC value)
    Extended Addressing: [op][lowaddr][highaddr] (1 or 2 byte op code)
        Used to jump to any location in 16 bit memory

*/

// CPU Data
pub struct Cpu {
    pub running: bool,
    memory: Rc<RefCell<Memory>>,
    regs: Registers
}

macro_rules! read_reg_pair {
    ($h:expr, $l:expr) => {
        (($h as u16) << 8) | $l as u16
    };
}

macro_rules! write_reg_pair {
    ($h:expr, $l:expr, $v:expr) => {{
        $h = ($v >> 8) as u8;
        $l = ($v & 0xFF) as u8;
    }};
}

// Control Conditions
#[derive(Debug)]
pub enum Cond {
    None, NZ, Z, NC, C
}

// Registers
impl In8 for Reg8 {
    fn read(&self, cpu: &mut Cpu) -> u8 {
        match *self {
            Reg8::A => cpu.regs.a,
            Reg8::B => cpu.regs.b,
            Reg8::C => cpu.regs.c,
            Reg8::D => cpu.regs.d,
            Reg8::E => cpu.regs.e,
            Reg8::F => cpu.regs.f,
            Reg8::H => cpu.regs.h,
            Reg8::L => cpu.regs.l
        }
    }
}
impl Out8 for Reg8 {
    fn write(&self, cpu: &mut Cpu, data: u8) {
        match *self {
            Reg8::A => cpu.regs.a = data,
            Reg8::B => cpu.regs.b = data,
            Reg8::C => cpu.regs.c = data,
            Reg8::D => cpu.regs.d = data,
            Reg8::E => cpu.regs.e = data,
            Reg8::F => cpu.regs.f = data,
            Reg8::H => cpu.regs.h = data,
            Reg8::L => cpu.regs.l = data
        }
    }
}

impl In16 for Reg16 {
    fn read(&self, cpu: &mut Cpu) -> u16 {
        match *self {
            Reg16::AF => read_reg_pair!(cpu.regs.a, cpu.regs.f),
            Reg16::BC => read_reg_pair!(cpu.regs.b, cpu.regs.c),
            Reg16::DE => read_reg_pair!(cpu.regs.d, cpu.regs.e),
            Reg16::HL => read_reg_pair!(cpu.regs.h, cpu.regs.l),
            Reg16::SP => cpu.regs.sp,
            Reg16::PC => cpu.regs.pc,
        }
    }
}

impl Out16 for Reg16 {
    fn write(&self, cpu: &mut Cpu, data: u16) {
        match *self {
            Reg16::AF => write_reg_pair!(cpu.regs.a, cpu.regs.f, data),
            Reg16::BC => write_reg_pair!(cpu.regs.b, cpu.regs.c, data),
            Reg16::DE => write_reg_pair!(cpu.regs.d, cpu.regs.e, data),
            Reg16::HL => write_reg_pair!(cpu.regs.h, cpu.regs.l, data),
            Reg16::SP => cpu.regs.sp = data,
            Reg16::PC => cpu.regs.pc = data,
        }
    }
}

// Immediate operand - a constant stored in the next byte
#[derive(Debug)]
pub struct Imm8(u8);
impl In8 for Imm8 {
    fn read(&self, _: &mut Cpu) -> u8 {
        let Imm8(v) = *self; v
    }
}

// Immediate extended operand - a constant stored in the next two bytes
#[derive(Debug)]
pub struct Imm16(u16);
impl In16 for Imm16 {
    fn read(&self, _: &mut Cpu) -> u16 {
        let Imm16(v) = *self; v
    }
}

// Indirect Addressing
#[derive(Debug)]
enum IndirectAddr {
    BC, DE, HL,     // (BC/DE/HL)
    C,              // (FF00 + C)
    Imm8,           // (FF00 + n)
    Imm16,          // (nn)
}

impl In8 for IndirectAddr {
    fn read(&self, cpu: &mut Cpu) -> u8 {
        let addr = match *self {
            IndirectAddr::BC => read_reg_pair!(cpu.regs.b, cpu.regs.c),
            IndirectAddr::DE => read_reg_pair!(cpu.regs.d, cpu.regs.e),
            IndirectAddr::HL => read_reg_pair!(cpu.regs.h, cpu.regs.l),
            IndirectAddr::C => cpu.regs.c as u16 + 0xFF00,
            IndirectAddr::Imm8 => cpu.mem_next_u8() as u16 + 0xFF00,
            IndirectAddr::Imm16 => cpu.mem_next_u16()
        };
        cpu.mem_read_u8(addr)
    }
}

impl Out8 for IndirectAddr {
    fn write(&self, cpu: &mut Cpu, data: u8) {
        let addr = match *self {
            IndirectAddr::BC => read_reg_pair!(cpu.regs.b, cpu.regs.c),
            IndirectAddr::DE => read_reg_pair!(cpu.regs.d, cpu.regs.e),
            IndirectAddr::HL => read_reg_pair!(cpu.regs.h, cpu.regs.l),
            IndirectAddr::C => cpu.regs.c as u16 + 0xFF00,
            IndirectAddr::Imm8 => cpu.mem_next_u8() as u16 + 0xFF00,
            IndirectAddr::Imm16 => cpu.mem_next_u16()
        };
        cpu.mem_write_u8(addr, data);
    }
}

impl Out16 for IndirectAddr {
    fn write(&self, cpu: &mut Cpu, data: u16) {
        let addr = match *self {
            IndirectAddr::BC => read_reg_pair!(cpu.regs.b, cpu.regs.c),
            IndirectAddr::DE => read_reg_pair!(cpu.regs.d, cpu.regs.e),
            IndirectAddr::HL => read_reg_pair!(cpu.regs.h, cpu.regs.l),
            IndirectAddr::C => cpu.regs.c as u16 + 0xFF00,
            IndirectAddr::Imm8 => cpu.mem_next_u8() as u16 + 0xFF00,
            IndirectAddr::Imm16 => cpu.mem_next_u16()
        };
        cpu.mem_write_u16(addr, data);
    }
}

// Interpreter implementation of the CPU ops defined in the ops module
impl<'a> CpuOps for &'a mut Cpu {
    fn next_u8(&mut self) -> u8 { self.mem_next_u8() }
    fn next_u16(&mut self) -> u16 { self.mem_next_u16() }

    fn load<I: In8, O: Out8>(&mut self, i: I, o: O) {
        println!("status: Load - in: {:?} out: {:?}", i, o);
        let value = i.read(self);
        o.write(self, value);
    }

    fn load16<I: In16, O: Out16>(&mut self, i: I, o: O) {
        println!("status: Load - in: {:?} out: {:?}", i, o);
        let value = i.read(self);
        o.write(self, value);
    }
    
    fn load16_hlsp(&mut self, offset: i8) {
        println!("status: Load HL SP + {:?}", offset);
    }
    
    fn push<I: In16>(&mut self, i: I) {
        println!("status: Push - in: {:?}", i);
    }

    fn pop<O: Out16>(&mut self, o: O) {
        println!("status: Pop - out: {:?}", o);
    }

    fn add<I: In8>(&mut self, i: I) {
        println!("status: Add - in: {:?}", i);
    }

    fn adc<I: In8>(&mut self, i: I) {
        println!("status: Add with carry - in: {:?}", i);
    }

    fn sub<I: In8>(&mut self, i: I) {
        println!("status: Sub - in: {:?}", i);
    }

    fn sbc<I: In8>(&mut self, i: I) {
        println!("status: Sub with carry - in: {:?}", i);
    }

    fn and<I: In8>(&mut self, i: I) {
        println!("status: And - in: {:?}", i);
    }

    fn xor<I: In8>(&mut self, i: I) {
        println!("status: Xor - in: {:?}", i);
    }

    fn or<I: In8>(&mut self, i: I) {
        println!("status: Or - in: {:?}", i);
    }

    fn cp<I: In8>(&mut self, i: I) {
        println!("status: Compare - in: {:?}", i);
    }

    fn inc<I: In8>(&mut self, i: I) {
        println!("status: Increment - in: {:?}", i);
    }

    fn dec<I: In8>(&mut self, i: I) {
        println!("status: Decrement - in: {:?}", i);
    }

    fn add16<I: In16>(&mut self, i: I) {
        println!("status: Add - in: {:?}", i);
    }

    fn add16_sp(&mut self, i: Imm8) {
        println!("status: Add SP - in: {:?}", i);
    }

    fn inc16<I: In16 + Out16>(&mut self, i: I) {
        println!("status: Increment - in: {:?}", i);
    }

    fn dec16<I: In16 + Out16>(&mut self, i: I) {
        println!("status: Decrement - in: {:?}", i);
    }

    // misc
    fn nop(&mut self) {}
    
    fn daa(&mut self) {
        println!("status: Decimal adjust register A");
    }

    fn cpl(&mut self) {
        println!("status: Complement A");
    }

    fn ccf(&mut self) {
        println!("status: Complement carry flag")
    }

    fn scf(&mut self) {
        println!("status: Set carry flag");
    }
    
    fn halt(&mut self) {
        println!("status: Halt");
    }

    fn stop(&mut self) {
        println!("status: Stop");
    }
    
    fn ei(&mut self) {
        println!("status: Enable interrupts");
    }

    fn di(&mut self) {
        println!("status: Disable interrupts");
    }

    // rotate and shift
    fn rrca(&mut self) {
        println!("status: Rotate A right with carry");
    }

    fn rra(&mut self) {
        println!("status: Rotate A right");
    }

    fn rlc<I: In8 + Out8>(&mut self, i: I) {
        println!("status: Rotate left with carry - in: {:?}", i);
    }
    
    fn rl<I: In8 + Out8>(&mut self, i: I) {
        println!("status: Rotate left - in: {:?}", i);
    }
    
    fn rrc<I: In8 + Out8>(&mut self, i: I) {
        println!("status: Rotate right with carry - in: {:?}", i);
    }
    
    fn rr<I: In8 + Out8>(&mut self, i: I) {
        println!("status: Rotate right - in: {:?}", i);
    }
    
    fn sla<I: In8 + Out8>(&mut self, i: I) {
        println!("status: Shift left into carry - in: {:?}", i);
    }
    
    fn sra<I: In8 + Out8>(&mut self, i: I) {
        println!("status: Shift right into carry - in: {:?}", i);
    }

    fn swap<I: In8 + Out8>(&mut self, i: I) {
        println!("status: Swap - in: {:?}", i);
    }
    
    fn srl<I: In8 + Out8>(&mut self, i: I) {
        println!("status: Shift right into carry - in: {:?}", i);
    }

    // bit manipulation
    fn bit<O: Out8>(&mut self, o: O) {
        let bit_id = self.next_u8();
        println!("status: Bit test - bit: {:02x} out: {:?}", bit_id, o);
    }

    fn set<O: Out8>(&mut self, o: O) {
        let bit_id = self.next_u8();
        println!("status: Set bit - bit: {:02x} out: {:?}", bit_id, o);
    }

    fn res<O: Out8>(&mut self, o: O) {
        let bit_id = self.next_u8();
        println!("status: Reset bit - bit: {:02x} out: {:?}", bit_id, o);
    }

    // control
    fn jp(&mut self, cond: Cond) {
        let dest = self.next_u16();
        println!("status: Jump - dest: {:04x} cond: {:?}", dest, cond);
    }

    fn jp_hl(&mut self) {
        println!("status: Jump (HL)");
    }

    fn jr(&mut self, cond: Cond) {
        let offset = self.next_u8();
        println!("status: Jump - dest: {:04x} cond: {:?}", self.regs.pc + offset as u16, cond);
    }

    fn call(&mut self, cond: Cond) {
        let dest = self.next_u16();
        println!("status: Call - dest: {:04x} cond: {:?}", dest, cond);
    }

    fn rst(&mut self, offset: u8) {
        println!("status: Reset - dest: {:04x}", offset);
    }

    fn ret(&mut self, cond: Cond) {
        println!("status: Return - cond: {:?}", cond);
    }

    fn reti(&mut self) {
        println!("status: Return from Interrupt");
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
        ops::decode(&mut *self);

        // Stop execution for the lols
        if self.regs.pc > 256 {
            self.running = false;
            self.dump_state();
        }
    }

    // Memory reading helper functions
    fn mem_read_u8(&self, addr: u16) -> u8 {
        self.memory.borrow().read_u8(addr)
    }

    fn mem_next_u8(&mut self) -> u8 {
        let byte = self.mem_read_u8(self.regs.pc);
        self.regs.pc += 1;
        byte
    }

    fn mem_next_u16(&mut self) -> u16 {
        let l = self.mem_next_u8();
        let h = self.mem_next_u8();
        ((l as u16) << 8) | (h as u16)
    }

    fn mem_write_u8(&mut self, addr: u16, data: u8) {
        self.memory.borrow_mut().write_u8(addr, data);
    }
    
    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        self.memory.borrow_mut().write_u16(addr, data);
    }

    // Some misc helper functions
    fn get_bits(&self, number: u16, min: u16, max: u16) -> u16 {
        (number >> min) & ((max - min) - 1)
    }

    fn get_bit(&self, number: u16, bit: u16) -> u16 {
        (number >> bit) & 0x1
    }

    pub fn dump_state(&self) {
        println!("Registers:");
        println!("- PC: {:04x} SP: {:04x} ", self.regs.pc, self.regs.sp);
        println!("- A: {:02x} F: {:02x} B: {:02x} C: {:02x}", self.regs.a, self.regs.f, self.regs.b, self.regs.c);
        println!("- D: {:02x} E: {:02x} H: {:02x} L: {:02x}", self.regs.d, self.regs.e, self.regs.h, self.regs.l);
        println!("Flags:");
        println!("- Zero: {}", self.get_bit(self.regs.f as u16, 7));
        println!("- Add/Sub: {}", self.get_bit(self.regs.f as u16, 6));
        println!("- Half Carry: {}", self.get_bit(self.regs.f as u16, 5));
        println!("- Carry Flag {}", self.get_bit(self.regs.f as u16, 4));
    }
}

// Test cases
#[cfg(test)]
mod test {
    use std::rc::Rc;
    use std::cell::RefCell;
    use memory::Memory;
    use super::*;
    use cpu::registers::*;
    use cpu::ops::*;

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
    fn load_from_reg_a_to_b() {
        let mut cpu = &mut init_cpu();
        cpu.load(Imm8(test_u8()), Reg8::A);
        cpu.load(Reg8::A, Reg8::B);
        assert_eq!(cpu.regs.a, test_u8());
        assert_eq!(cpu.regs.a, cpu.regs.b);
    }

    #[test]
    fn load_from_reg_bc_to_de() {
        let mut cpu = &mut init_cpu();
        cpu.load16(Imm16(test_u16()), Reg16::BC);
        cpu.load16(Reg16::BC, Reg16::DE);
        assert_eq!(Reg16::BC.read(cpu), test_u16());
        assert_eq!(Reg16::BC.read(cpu), Reg16::DE.read(cpu));
    }
}
