use std::fmt::Debug;
use cpu::Cpu;
use cpu::registers::Reg8::{
    A, B, C, D, E, F, H, L
};
use cpu::Imm8;
use cpu::IndirectAddr;

// A trait which represents an instruction input (register/indirect/immediate)
pub trait In8 : Debug {
    fn read(&self, cpu: &mut Cpu) -> u8;
}

// A trait which represents an instruction output (register/indirect)
pub trait Out8 : Debug {
    fn write(&self, cpu: &mut Cpu, data: u8);
}

pub trait CpuOps {
    fn noop(&self);
    // 8-bit load
    fn load<I: In8, O: Out8>(&self, i: I, o: O);
    // 8-bit arithmetic
    fn add<I: In8>(&self, i: I);
    fn adc<I: In8>(&self, i: I);
    fn sub<I: In8>(&self, i: I);
    fn sbc<I: In8>(&self, i: I);
    fn and<I: In8>(&self, i: I);
    fn xor<I: In8>(&self, i: I);
    fn or<I: In8>(&self, i: I);
    fn cp<I: In8>(&self, i: I);
    fn inc<I: In8>(&self, i: I);
    fn dec<I: In8>(&self, i: I);
    // Jumps

}

// Take an op code, and run the relevant op command
pub fn decode<O: CpuOps>(ops: O, opcode: u8) {
    match opcode {
        0x00 => ops.noop(),
        
        0x7F => ops.load(A, A),
        0x78 => ops.load(B, A),
        0x79 => ops.load(C, A),
        0x7A => ops.load(D, A),
        0x7B => ops.load(E, A),
        0x7C => ops.load(F, A),
        0x7D => ops.load(L, A),
        0x7E => ops.load(IndirectAddr::HL, A),
        0x0A => ops.load(IndirectAddr::BC, A),
        0x1A => ops.load(IndirectAddr::DE, A),
        0xFD => panic!("Unhandled"),

        0x47 => ops.load(A, B),
        0x40 => ops.load(B, B),
        0x41 => ops.load(C, B),
        0x42 => ops.load(D, B),
        0x43 => ops.load(E, B),
        0x44 => ops.load(F, B),
        0x45 => ops.load(L, B),
        0x46 => ops.load(IndirectAddr::HL, B),

        0x4F => ops.load(A, C),
        0x48 => ops.load(B, C),
        0x49 => ops.load(C, C),
        0x4A => ops.load(D, C),
        0x4B => ops.load(E, C),
        0x4C => ops.load(F, C),
        0x4D => ops.load(L, C),
        0x4E => ops.load(IndirectAddr::HL, C),

        0x57 => ops.load(A, D),
        0x50 => ops.load(B, D),
        0x51 => ops.load(C, D),
        0x52 => ops.load(D, D),
        0x53 => ops.load(E, D),
        0x54 => ops.load(F, D),
        0x55 => ops.load(L, D),
        0x56 => ops.load(IndirectAddr::HL, D),

        0x5F => ops.load(A, E),
        0x58 => ops.load(B, E),
        0x59 => ops.load(C, E),
        0x5A => ops.load(D, E),
        0x5B => ops.load(E, E),
        0x5C => ops.load(F, E),
        0x5D => ops.load(L, E),
        0x5E => ops.load(IndirectAddr::HL, E),

        0x67 => ops.load(A, H),
        0x60 => ops.load(B, H),
        0x61 => ops.load(C, H),
        0x62 => ops.load(D, H),
        0x63 => ops.load(E, H),
        0x64 => ops.load(F, H),
        0x65 => ops.load(L, H),
        0x66 => ops.load(IndirectAddr::HL, H),

        0x6F => ops.load(A, L),
        0x68 => ops.load(B, L),
        0x69 => ops.load(C, L),
        0x6A => ops.load(D, L),
        0x6B => ops.load(E, L),
        0x6C => ops.load(F, L),
        0x6D => ops.load(L, L),
        0x6E => ops.load(IndirectAddr::HL, L),

        // All these instructions are immediate addressing
        /*
        0xDD => {
            let imm_addr = Imm(memory.read_u8(self.regs.pc + 2));
            match opcode_bytes[1] {
                0xD5 => ops.load(imm_addr, B),
                0xDE => ops.load(imm_addr, C),
                0x1B => ops.load(imm_addr, D),
                0x1E => ops.load(imm_addr, E),
                0x2B => ops.load(imm_addr, H),
                0x36 => ops.load(imm_addr, L),
                0x78 => ops.load(imm_addr, IndirectAddr::HL), // Indirect Destination
                _ => panic!("Unknown opcode 0x{:02x}{:02x}", opcode_bytes[0], opcode_bytes[1])
            };
            3 // All consume 3 bytes
        }
        */

        // Register Indirect Destination
        0x77 => ops.load(A, IndirectAddr::HL),
        0x70 => ops.load(B, IndirectAddr::HL),
        0x71 => ops.load(C, IndirectAddr::HL),
        0x72 => ops.load(D, IndirectAddr::HL),
        0x73 => ops.load(E, IndirectAddr::HL),
        0x74 => ops.load(F, IndirectAddr::HL),
        0x75 => ops.load(L, IndirectAddr::HL),

        0x02 => ops.load(A, IndirectAddr::BC),

        0x12 => ops.load(A, IndirectAddr::DE),

        // External Address Destination
        /*
        0x32 => {
            let ext_addr = Ext(memory.read_u16(self.regs.pc + 1));
            ops.load(A, ext_addr);
            3
        },
        */

        // --------------------


        // Arithmetic
        // --------------------

        // Add
        0x87 => ops.add(A),
        0x80 => ops.add(B),
        0x81 => ops.add(C),
        0x82 => ops.add(D),
        0x83 => ops.add(E),
        0x84 => ops.add(F),
        0x85 => ops.add(L),
        0x86 => ops.add(IndirectAddr::HL),
        0xC6 => ops.add(Imm8),

        // Add with Carry
        0x8F => ops.adc(A),
        0x88 => ops.adc(B),
        0x89 => ops.adc(C),
        0x8A => ops.adc(D),
        0x8B => ops.adc(E),
        0x8C => ops.adc(F),
        0x8D => ops.adc(L),
        0x8E => ops.adc(IndirectAddr::HL),
        0xCE => ops.adc(Imm8),

        // Subtract
        0x97 => ops.sub(A),
        0x90 => ops.sub(B),
        0x91 => ops.sub(C),
        0x92 => ops.sub(D),
        0x93 => ops.sub(E),
        0x94 => ops.sub(F),
        0x95 => ops.sub(L),
        0x96 => ops.sub(IndirectAddr::HL),
        0xD6 => ops.sub(Imm8),

        // Subtract with Carry
        0x9F => ops.sbc(A),
        0x98 => ops.sbc(B),
        0x99 => ops.sbc(C),
        0x9A => ops.sbc(D),
        0x9B => ops.sbc(E),
        0x9C => ops.sbc(F),
        0x9D => ops.sbc(L),
        0x9E => ops.sbc(IndirectAddr::HL),
        0xDE => ops.sbc(Imm8),

        // AND
        0xA7 => ops.and(A),
        0xA0 => ops.and(B),
        0xA1 => ops.and(C),
        0xA2 => ops.and(D),
        0xA3 => ops.and(E),
        0xA4 => ops.and(F),
        0xA5 => ops.and(L),
        0xA6 => ops.and(IndirectAddr::HL),
        0xE6 => ops.and(Imm8),

        // XOR
        0xAF => ops.xor(A),
        0xA8 => ops.xor(B),
        0xA9 => ops.xor(C),
        0xAA => ops.xor(D),
        0xAB => ops.xor(E),
        0xAC => ops.xor(F),
        0xAD => ops.xor(L),
        0xAE => ops.xor(IndirectAddr::HL),
        0xEE => ops.xor(Imm8),

        // OR
        0xB7 => ops.or(A),
        0xB0 => ops.or(B),
        0xB1 => ops.or(C),
        0xB2 => ops.or(D),
        0xB3 => ops.or(E),
        0xB4 => ops.or(F),
        0xB5 => ops.or(L),
        0xB6 => ops.or(IndirectAddr::HL),
        0xF6 => ops.or(Imm8),

        // Compare
        0xBF => ops.cp(A),
        0xB8 => ops.cp(B),
        0xB9 => ops.cp(C),
        0xBA => ops.cp(D),
        0xBB => ops.cp(E),
        0xBC => ops.cp(F),
        0xBD => ops.cp(L),
        0xBE => ops.cp(IndirectAddr::HL),
        0xFE => ops.cp(Imm8),

        // Increment
        0x3C => ops.inc(A),
        0x04 => ops.inc(B),
        0x0C => ops.inc(C),
        0x14 => ops.inc(D),
        0x1C => ops.inc(E),
        0x24 => ops.inc(F),
        0x2C => ops.inc(L),
        0x34 => ops.inc(IndirectAddr::HL),

        // Decrement
        0x3D => ops.dec(A),
        0x05 => ops.dec(B),
        0x0D => ops.dec(C),
        0x15 => ops.dec(D),
        0x1D => ops.dec(E),
        0x25 => ops.dec(F),
        0x2D => ops.dec(L),
        0x35 => ops.dec(IndirectAddr::HL),


        // --------------------

        // 16-bit Arithmetic
        // --------------------


        // TODO

        // --------------------


        // Rotate and Shift
        // --------------------

        // TODO

        // --------------------


        // Bit Manipulation
        // --------------------

        // TODO

        // --------------------


        // Jump, Call and return
        // --------------------
/*
        0xC3 => {
            let ext_addr = memory.read_u16(self.regs.pc + 1);
            self.dispatch_jump(ext_addr, Condition::None); 3
        },
        0xD8 => {
            let ext_addr = memory.read_u16(self.regs.pc + 1);
            self.dispatch_jump(ext_addr, Condition::Carry); 3
        },
        0xD2 => {
            let ext_addr = memory.read_u16(self.regs.pc + 1);
            self.dispatch_jump(ext_addr, Condition::NonCarry); 3
        },
        0xCA => {
            let ext_addr = memory.read_u16(self.regs.pc + 1);
            self.dispatch_jump(ext_addr, Condition::Zero); 3
        },
        0xC2 => {
            let ext_addr = memory.read_u16(self.regs.pc + 1);
            self.dispatch_jump(ext_addr, Condition::NonZero); 3
        },
        0xEA => {
            let ext_addr = memory.read_u16(self.regs.pc + 1);
            self.dispatch_jump(ext_addr, Condition::ParityEven); 3
        },
        0xE2 => {
            let ext_addr = memory.read_u16(self.regs.pc + 1);
            self.dispatch_jump(ext_addr, Condition::ParityOdd); 3
        },
        0xFA => {
            let ext_addr = memory.read_u16(self.regs.pc + 1);
            self.dispatch_jump(ext_addr, Condition::SignNeg); 3
        },
        0xF2 => {
            let ext_addr = memory.read_u16(self.regs.pc + 1);
            self.dispatch_jump(ext_addr, Condition::SignPos); 3
        }
        */
        
        _ => println!("warning: Unknown opcode 0x{:02x}", opcode)
    }
}