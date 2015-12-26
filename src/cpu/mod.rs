mod registers;
mod ops;

use std::rc::Rc;
use std::cell::RefCell;
use memory::Memory;
use cpu::registers::{Registers, Reg8};
use cpu::ops::{CpuOps, In8, Out8};

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

// Control Conditions
#[derive(Debug)]
enum Cond {
    None, C, NC, Z, NZ
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

// Immediate operand - a constant stored in the next byte
#[derive(Debug)]
struct Imm8;
impl In8 for Imm8 {
    fn read(&self, cpu: &mut Cpu) -> u8 {
        // Just read the next byte
        cpu.next_u8()
    }
}

// Immediate extended operand - a constant stored in the next two bytes
// TODO

// Indirect Addressing
#[derive(Debug)]
enum IndirectAddr {
    BC, DE, HL
}
impl In8 for IndirectAddr {
    fn read(&self, cpu: &mut Cpu) -> u8 {
        // do stuff
        0
    }
}
impl Out8 for IndirectAddr {
    fn write(&self, cpu: &mut Cpu, data: u8) {
    }
}

// Interpreter implementation of the CPU ops defined in the ops module
impl<'a> CpuOps for &'a mut Cpu {
    fn load<I: In8, O: Out8>(self, i: I, o: O) {
        println!("status: Load - in: {:?} out: {:?}", i, o);
    }
    fn add<I: In8>(self, i: I) {
        println!("status: Add - in: {:?}", i);
    }
    fn adc<I: In8>(self, i: I) {
        println!("status: Add with carry - in: {:?}", i);
    }
    fn sub<I: In8>(self, i: I) {
        println!("status: Sub - in: {:?}", i);
    }
    fn sbc<I: In8>(self, i: I) {
        println!("status: Sub with carry - in: {:?}", i);
    }
    fn and<I: In8>(self, i: I) {
        println!("status: And - in: {:?}", i);
    }
    fn xor<I: In8>(self, i: I) {
        println!("status: Xor - in: {:?}", i);
    }
    fn or<I: In8>(self, i: I) {
        println!("status: Or - in: {:?}", i);
    }
    fn cp<I: In8>(self, i: I) {
        println!("status: Compare - in: {:?}", i);
    }
    fn inc<I: In8>(self, i: I) {
        println!("status: Increment - in: {:?}", i);
    }
    fn dec<I: In8>(self, i: I) {
        println!("status: Decrement - in: {:?}", i);
    }
    // control
    fn jp(self, cond: Cond) {
        let dest = self.next_u16();
        println!("status: Jump - dest: {:02x} cond: {:?}", dest, cond);
    }
    fn jp_hl(self, cond: Cond) {
        println!("status: Jump (HL) - cond: {:?}", cond);
    }
    fn jr(self, cond: Cond) {
        let offset = self.next_u8();
        println!("status: Jump - dest: {:02x} cond: {:?}", self.regs.pc + offset as u16, cond);
    }
    fn call(self, cond: Cond) {
        let dest = self.next_u16();
        println!("status: Call - dest: {:02x} cond: {:?}", dest, cond);
    }
    fn ret(self, cond: Cond) {
        println!("status: Return - cond: {:?}", cond);
    }
    fn reti(self) {
        println!("status: Return from Interrupt");
    }
    // misc
    fn nop(self) {}
    fn daa(self) {}
    fn cpl(self) {}
    fn neg(self) {}
    fn ccf(self) {}
    fn scf(self) {}
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
        let opcode = self.next_u8();
        ops::decode(&mut *self as &mut Cpu, opcode);

        // Stop execution for the lols
        if self.regs.pc > 256 {
            self.running = false;
            self.dump_state();
        }
    }

    // Memory reading helper functions
    fn next_u8(&mut self) -> u8 {
        let byte = self.memory.borrow().read_u8(self.regs.pc);
        self.regs.pc += 1;
        byte
    }

    fn next_u16(&mut self) -> u16 {
        ((self.next_u8() as u16) >> 8) | self.next_u8() as u16
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
