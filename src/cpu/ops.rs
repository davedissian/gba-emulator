/*
    The decode method below was build using the Zilog Z80 processor manual,
    with the following alterations taken from gbspec.txt at:
    http://www.devrs.com/gb/files/gbspec.txt

    Additionally, the following was used: http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf

    The following are added instructions:

        ADD  SP,nn             ;nn = signed byte
        LD  (HLI),A            ;Write A to (HL) and increment HL
        LD  (HLD),A            ;Write A to (HL) and decrement HL
        LD  A,(HLI)            ;Write (HL) to A and increment HL
        LD  A,(HLD)            ;Write (HL) to A and decrement HL
        LD  A,($FF00+nn)
        LD  A,($FF00+C)
        LD  ($FF00+nn),A
        LD  ($FF00+C),A
        LD  (nnnn),SP
        LD  HL,SP+nn           ;nn = signed byte
        STOP                   ;Stop processor & screen until button press
        SWAP r                 ;Swap high & low nibbles of r

    The following instructions have been removed:

        Any command that uses the IX or IY registers.
        All IN/OUT instructions.
        All exchange instructions.
        All commands prefixed by ED (except remapped RETI).
        All conditional jumps/calls/rets on parity/overflow and sign flag.

    The following instructions have different opcodes:

        LD  A,[nnnn]        (..)
        LD  [nnnn],A        (..)
        RETI                (D9)
*/

use std::fmt::Debug;
use cpu::Cpu;
use cpu::Cond;
use cpu::Imm8;
use cpu::Imm16;
use cpu::IndirectAddr;
use cpu::registers::Reg8::{
    A, B, C, D, E, F, H, L
};
use cpu::registers::Reg16::{
    AF, BC, DE, HL, SP
};

pub trait In8 : Debug {
    fn read(&self, cpu: &mut Cpu) -> u8;
}

pub trait Out8 : Debug {
    fn write(&self, cpu: &mut Cpu, data: u8);
}

pub trait In16 : Debug {
    fn read(&self, cpu: &mut Cpu) -> u16;
}

pub trait Out16 : Debug {
    fn write(&self, cpu: &mut Cpu, data: u16);
}

pub trait CpuOps {
    fn next_u8(self) -> u8;
    fn next_u16(self) -> u16;
    
    // 8-bit and 16-bit load
    fn load<I: In8, O: Out8>(self, i: I, o: O);
    fn load16<I: In16, O: Out16>(self, i: I, o: O);
    fn load16_hlsp(self, i: Imm8); // Load SP + n into HL
    fn push<I: In16>(self, i: I);
    fn pop<O: Out16>(self, o: O);
    // 8-bit arithmetic
    fn add<I: In8>(self, i: I);
    fn adc<I: In8>(self, i: I);
    fn sub<I: In8>(self, i: I);
    fn sbc<I: In8>(self, i: I);
    fn and<I: In8>(self, i: I);
    fn xor<I: In8>(self, i: I);
    fn or<I: In8>(self, i: I);
    fn cp<I: In8>(self, i: I);
    fn inc<I: In8 + Out8>(self, i: I);
    fn dec<I: In8 + Out8>(self, i: I);
    // 16-bit arithmetic
    fn add16<I: In16>(self, i: I);
    fn add16_sp(self, i: Imm8);
    fn inc16<I: In16 + Out16>(self, i: I);
    fn dec16<I: In16 + Out16>(self, i: I);
    // misc
    fn nop(self);
    fn daa(self);
    fn cpl(self);
    fn ccf(self);
    fn scf(self);
    fn halt(self);
    fn stop(self);
    fn ei(self);
    fn di(self);
    // rotate and shift
    fn rrca(self);
    fn rra(self);
    fn rlc<I: In8 + Out8>(self, i: I);
    fn rl<I: In8 + Out8>(self, i: I);
    fn rrc<I: In8 + Out8>(self, i: I);
    fn rr<I: In8 + Out8>(self, i: I);
    fn sla<I: In8 + Out8>(self, i: I);
    fn sra<I: In8 + Out8>(self, i: I);
    fn swap<I: In8 + Out8>(self, i: I);
    fn srl<I: In8 + Out8>(self, i: I);
    // bit manipulation
    fn bit<O: Out8>(self, o: O);
    fn set<O: Out8>(self, o: O);
    fn res<O: Out8>(self, o: O);
    // control
    fn jp(self, cond: Cond);        // JP n
    fn jp_hl(self);                 // JP (HL)
    fn jr(self, cond: Cond);
    fn call(self, cond: Cond);
    fn rst(self, offset: u8);
    fn ret(self, cond: Cond);
    fn reti(self);
}

pub fn decode<O: CpuOps>(ops: O) {
    let opcode = ops.next_u8();
    match opcode { 
        // 8-bit load
        0x06 => ops.load(Imm8, B), 
        0x0E => ops.load(Imm8, C), 
        0x16 => ops.load(Imm8, D), 
        0x1E => ops.load(Imm8, E), 
        0x26 => ops.load(Imm8, H), 
        0x2E => ops.load(Imm8, L), 
        0x36 => ops.load(Imm8, IndirectAddr::HL),

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

        0x02 => ops.load(A, IndirectAddr::BC),
        0x12 => ops.load(A, IndirectAddr::DE),
        0x77 => ops.load(A, IndirectAddr::HL),
        0xEA => ops.load(A, IndirectAddr::Imm16),
        0x70 => ops.load(B, IndirectAddr::HL),
        0x71 => ops.load(C, IndirectAddr::HL),
        0x72 => ops.load(D, IndirectAddr::HL),
        0x73 => ops.load(E, IndirectAddr::HL),
        0x74 => ops.load(F, IndirectAddr::HL),
        0x75 => ops.load(L, IndirectAddr::HL),

        0xF2 => ops.load(A, IndirectAddr::C),
        0xE2 => ops.load(IndirectAddr::C, A),

        0x3A => println!("warning: Unimplemented LDD A, (HL)"),
        0x32 => println!("warning: Unimplemented LDD (HL), A"),
        0x2A => println!("warning: Unimplemented LDI A, (HL)"),
        0x22 => println!("warning: Unimplemented LDI (HL), A"),
       
        0xE0 => ops.load(A, IndirectAddr::Imm8),
        0xF0 => ops.load(IndirectAddr::Imm8, A),

        // 16-bit load
        0x01 => ops.load16(Imm16, BC),
        0x11 => ops.load16(Imm16, DE),
        0x21 => ops.load16(Imm16, HL),
        0x31 => ops.load16(Imm16, SP),
        0xF9 => ops.load16(HL, SP),
        0xF8 => ops.load16_hlsp(Imm8),
        0x08 => ops.load16(SP, IndirectAddr::Imm16),

        0xF5 => ops.push(AF),
        0xC5 => ops.push(BC),
        0xD5 => ops.push(DE),
        0xE5 => ops.push(HL),

        0xF1 => ops.pop(AF),
        0xC1 => ops.pop(BC),
        0xD1 => ops.pop(DE),
        0xE1 => ops.pop(HL),

        // arithmetic
        0x87 => ops.add(A),
        0x80 => ops.add(B),
        0x81 => ops.add(C),
        0x82 => ops.add(D),
        0x83 => ops.add(E),
        0x84 => ops.add(F),
        0x85 => ops.add(L),
        0x86 => ops.add(IndirectAddr::HL),
        0xC6 => ops.add(Imm8),

        0x8F => ops.adc(A),
        0x88 => ops.adc(B),
        0x89 => ops.adc(C),
        0x8A => ops.adc(D),
        0x8B => ops.adc(E),
        0x8C => ops.adc(F),
        0x8D => ops.adc(L),
        0x8E => ops.adc(IndirectAddr::HL),
        0xCE => ops.adc(Imm8),

        0x97 => ops.sub(A),
        0x90 => ops.sub(B),
        0x91 => ops.sub(C),
        0x92 => ops.sub(D),
        0x93 => ops.sub(E),
        0x94 => ops.sub(F),
        0x95 => ops.sub(L),
        0x96 => ops.sub(IndirectAddr::HL),
        0xD6 => ops.sub(Imm8),

        0x9F => ops.sbc(A),
        0x98 => ops.sbc(B),
        0x99 => ops.sbc(C),
        0x9A => ops.sbc(D),
        0x9B => ops.sbc(E),
        0x9C => ops.sbc(F),
        0x9D => ops.sbc(L),
        0x9E => ops.sbc(IndirectAddr::HL),
        0xDE => ops.sbc(Imm8),

        0xA7 => ops.and(A),
        0xA0 => ops.and(B),
        0xA1 => ops.and(C),
        0xA2 => ops.and(D),
        0xA3 => ops.and(E),
        0xA4 => ops.and(F),
        0xA5 => ops.and(L),
        0xA6 => ops.and(IndirectAddr::HL),
        0xE6 => ops.and(Imm8),

        0xAF => ops.xor(A),
        0xA8 => ops.xor(B),
        0xA9 => ops.xor(C),
        0xAA => ops.xor(D),
        0xAB => ops.xor(E),
        0xAC => ops.xor(F),
        0xAD => ops.xor(L),
        0xAE => ops.xor(IndirectAddr::HL),
        0xEE => ops.xor(Imm8),

        0xB7 => ops.or(A),
        0xB0 => ops.or(B),
        0xB1 => ops.or(C),
        0xB2 => ops.or(D),
        0xB3 => ops.or(E),
        0xB4 => ops.or(F),
        0xB5 => ops.or(L),
        0xB6 => ops.or(IndirectAddr::HL),
        0xF6 => ops.or(Imm8),

        0xBF => ops.cp(A),
        0xB8 => ops.cp(B),
        0xB9 => ops.cp(C),
        0xBA => ops.cp(D),
        0xBB => ops.cp(E),
        0xBC => ops.cp(F),
        0xBD => ops.cp(L),
        0xBE => ops.cp(IndirectAddr::HL),
        0xFE => ops.cp(Imm8),

        0x3C => ops.inc(A),
        0x04 => ops.inc(B),
        0x0C => ops.inc(C),
        0x14 => ops.inc(D),
        0x1C => ops.inc(E),
        0x24 => ops.inc(F),
        0x2C => ops.inc(L),
        0x34 => ops.inc(IndirectAddr::HL),

        0x3D => ops.dec(A),
        0x05 => ops.dec(B),
        0x0D => ops.dec(C),
        0x15 => ops.dec(D),
        0x1D => ops.dec(E),
        0x25 => ops.dec(F),
        0x2D => ops.dec(L),
        0x35 => ops.dec(IndirectAddr::HL),
        
        0x09 => ops.add16(BC),
        0x19 => ops.add16(DE),
        0x29 => ops.add16(HL),
        0x39 => ops.add16(SP),
        0xE8 => ops.add16_sp(Imm8),

        0x03 => ops.inc16(BC),
        0x13 => ops.inc16(DE),
        0x23 => ops.inc16(HL),
        0x33 => ops.inc16(SP),

        0x0B => ops.dec16(BC),
        0x1B => ops.dec16(DE),
        0x2B => ops.dec16(HL),
        0x3B => ops.dec16(SP),

        // misc
        0x27 => ops.daa(),
        0x2F => ops.cpl(),
        0x3F => ops.ccf(),
        0x37 => ops.scf(),
        0x00 => ops.nop(),
        0x76 => ops.halt(),
        0x10 => {
            let next_opcode = ops.next_u8();
            match next_opcode {
                0x00 => ops.stop(),
                _ => panic!("warning: Unknown opcode 0x10{:02x}", next_opcode) 
            }
        }
        0xF3 => ops.di(),
        0xFB => ops.ei(),

        // rotates and shifts
        0x0F => ops.rrca(),
        0x1f => ops.rra(),
        0xCB => {
            let next_opcode = ops.next_u8();
            match next_opcode {
                0x07 => ops.rlc(A),
                0x00 => ops.rlc(B),
                0x01 => ops.rlc(C),
                0x02 => ops.rlc(D),
                0x03 => ops.rlc(E),
                0x04 => ops.rlc(H),
                0x05 => ops.rlc(L),
                0x06 => ops.rlc(IndirectAddr::HL),
                
                0x17 => ops.rl(A),
                0x10 => ops.rl(B),
                0x11 => ops.rl(C),
                0x12 => ops.rl(D),
                0x13 => ops.rl(E),
                0x14 => ops.rl(H),
                0x15 => ops.rl(L),
                0x16 => ops.rl(IndirectAddr::HL),

                0x0F => ops.rrc(A),
                0x08 => ops.rrc(B),
                0x09 => ops.rrc(C),
                0x0A => ops.rrc(D),
                0x0B => ops.rrc(E),
                0x0C => ops.rrc(H),
                0x0D => ops.rrc(L),
                0x0E => ops.rrc(IndirectAddr::HL),
                
                0x1F => ops.rr(A),
                0x18 => ops.rr(B),
                0x19 => ops.rr(C),
                0x1A => ops.rr(D),
                0x1B => ops.rr(E),
                0x1C => ops.rr(H),
                0x1D => ops.rr(L),
                0x1E => ops.rr(IndirectAddr::HL),

                0x27 => ops.sla(A),
                0x20 => ops.sla(B),
                0x21 => ops.sla(C),
                0x22 => ops.sla(D),
                0x23 => ops.sla(E),
                0x24 => ops.sla(H),
                0x25 => ops.sla(L),
                0x26 => ops.sla(IndirectAddr::HL),

                0x2F => ops.sra(A),
                0x28 => ops.sra(B),
                0x29 => ops.sra(C),
                0x2A => ops.sra(D),
                0x2B => ops.sra(E),
                0x2C => ops.sra(H),
                0x2D => ops.sra(L),
                0x2E => ops.sra(IndirectAddr::HL),

                0x37 => ops.swap(A),
                0x30 => ops.swap(B),
                0x31 => ops.swap(C),
                0x32 => ops.swap(D),
                0x33 => ops.swap(E),
                0x34 => ops.swap(H),
                0x35 => ops.swap(L),
                0x36 => ops.swap(IndirectAddr::HL),

                0x3F => ops.srl(A),
                0x38 => ops.srl(B),
                0x39 => ops.srl(C),
                0x3A => ops.srl(D),
                0x3B => ops.srl(E),
                0x3C => ops.srl(H),
                0x3D => ops.srl(L),
                0x3E => ops.srl(IndirectAddr::HL),

                // bit manipulation
                0x47 => ops.bit(A),
                0x40 => ops.bit(B),
                0x41 => ops.bit(C),
                0x42 => ops.bit(D),
                0x43 => ops.bit(E),
                0x44 => ops.bit(H),
                0x45 => ops.bit(L),
                0x46 => ops.bit(IndirectAddr::HL),

                0xC7 => ops.set(A),
                0xC0 => ops.set(B),
                0xC1 => ops.set(C),
                0xC2 => ops.set(D),
                0xC3 => ops.set(E),
                0xC4 => ops.set(H),
                0xC5 => ops.set(L),
                0xC6 => ops.set(IndirectAddr::HL),

                0x87 => ops.res(A),
                0x80 => ops.res(B),
                0x81 => ops.res(C),
                0x82 => ops.res(D),
                0x83 => ops.res(E),
                0x84 => ops.res(H),
                0x85 => ops.res(L),
                0x86 => ops.res(IndirectAddr::HL),

                _ => panic!("warning: Unknown opcode 0xCB{:02x}", next_opcode)
            }
        },

        // control
        0xC3 => ops.jp(Cond::None),
        0xC2 => ops.jp(Cond::NZ),
        0xCA => ops.jp(Cond::Z),
        0xD2 => ops.jp(Cond::NC),
        0xDA => ops.jp(Cond::C),
        0xEB => ops.jp_hl(),

        0x18 => ops.jr(Cond::None),
        0x20 => ops.jr(Cond::NZ),
        0x28 => ops.jr(Cond::Z),
        0x30 => ops.jr(Cond::NC),
        0x38 => ops.jr(Cond::C),
        
        0xCD => ops.call(Cond::None),
        0xC4 => ops.call(Cond::NZ),
        0xCC => ops.call(Cond::Z),
        0xD4 => ops.call(Cond::NC),
        0xDC => ops.call(Cond::C),
       
        0xC7 => ops.rst(0x00),
        0xCF => ops.rst(0x08),
        0xD7 => ops.rst(0x10),
        0xDF => ops.rst(0x18),
        0xE7 => ops.rst(0x20),
        0xEF => ops.rst(0x28),
        0xF7 => ops.rst(0x30),
        0xFF => ops.rst(0x38),

        0xC9 => ops.ret(Cond::None),
        0xC0 => ops.ret(Cond::NZ),
        0xC8 => ops.ret(Cond::Z),
        0xD0 => ops.ret(Cond::NC),
        0xD8 => ops.ret(Cond::C),
        
        0xD9 => ops.reti(),
        
        _ => println!("warning: Unknown opcode 0x{:02x}", opcode)
    }
}
