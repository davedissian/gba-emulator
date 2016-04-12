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
use cpu::Op8;
use cpu::IndirectAddr;
use cpu::registers::Reg8::*;
use cpu::registers::Reg16::*;

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
    fn next_u8(&mut self) -> u8;
    fn next_u16(&mut self) -> u16;

    // 8-bit and 16-bit load
    fn load<I: In8, O: Out8>(&mut self, i: I, o: O);
    fn load16<I: In16, O: Out16>(&mut self, i: I, o: O);
    fn load16_hlsp(&mut self, offset: i8); // Load SP + n into HL
    fn push<I: In16>(&mut self, i: I);
    fn pop<O: Out16>(&mut self, o: O);
    // 8-bit arithmetic
    fn add<I: In8>(&mut self, i: I);
    fn adc<I: In8>(&mut self, i: I);
    fn sub<I: In8>(&mut self, i: I);
    fn sbc<I: In8>(&mut self, i: I);
    fn and<I: In8>(&mut self, i: I);
    fn or<I: In8>(&mut self, i: I);
    fn xor<I: In8>(&mut self, i: I);
    fn cp<I: In8>(&mut self, i: I);
    fn inc<I: In8 + Out8>(&mut self, i: I);
    fn dec<I: In8 + Out8>(&mut self, i: I);
    // 16-bit arithmetic
    fn add16<I: In16>(&mut self, i: I);
    fn add16_sp(&mut self, i: Imm8); // TODO(David): Replace Imm8 with just an i8 (must be signed)?
    fn inc16<I: In16 + Out16>(&mut self, i: I);
    fn dec16<I: In16 + Out16>(&mut self, i: I);
    // misc
    fn nop(&mut self);
    fn daa(&mut self);
    fn cpl(&mut self);
    fn ccf(&mut self);
    fn scf(&mut self);
    fn halt(&mut self);
    fn stop(&mut self);
    fn ei(&mut self);
    fn di(&mut self);
    // rotate and shift
    fn rrca(&mut self);
    fn rra(&mut self);
    fn rlc<I: In8 + Out8>(&mut self, i: I);
    fn rl<I: In8 + Out8>(&mut self, i: I);
    fn rrc<I: In8 + Out8>(&mut self, i: I);
    fn rr<I: In8 + Out8>(&mut self, i: I);
    fn sla<I: In8 + Out8>(&mut self, i: I);
    fn sra<I: In8 + Out8>(&mut self, i: I);
    fn swap<I: In8 + Out8>(&mut self, i: I);
    fn srl<I: In8 + Out8>(&mut self, i: I);
    // bit manipulation
    fn bit<O: Out8>(&mut self, bit_id: u8, o: O);
    fn set<O: Out8>(&mut self, bit_id: u8, o: O);
    fn res<O: Out8>(&mut self, bit_id: u8, o: O);
    // control
    fn jp(&mut self, dest: u16, cond: Cond);        // JP n
    fn jp_hl(&mut self);                            // JP (HL)
    fn jr(&mut self, offset: u8, cond: Cond);       // TODO(David): is this a signed offset?
    fn call(&mut self, dest: u16, cond: Cond);
    fn rst(&mut self, offset: u8);
    fn ret(&mut self, cond: Cond);
    fn reti(&mut self);
}

// These macros are required due to a bug in the Rust borrow checker which disallows a mutable
// object 'foo' having two method calls in the same line. Eg:
//  foo.method(foo.value());
macro_rules! op1 {
    ($ops: ident, $instr:ident, $a:expr) => ({
        let temp = $a;
        $ops.$instr(temp);
    })
}

macro_rules! op2 {
    ($ops: ident, $instr:ident, $a:expr, $b:expr) => ({
        let temp_a = $a;
        let temp_b = $b;
        $ops.$instr(temp_a, temp_b);
    })
}

fn to_operand(operand: u8) -> Op8 {
    match operand {
        0x0 => Op8::Reg(B),
        0x1 => Op8::Reg(C),
        0x2 => Op8::Reg(D),
        0x3 => Op8::Reg(E),
        0x4 => Op8::Reg(F),
        0x5 => Op8::Reg(L),
        0x6 => Op8::Ind(IndirectAddr::HL),
        0x7 => Op8::Reg(A),
        _   => panic!("Invalid operand")
    }
}

// Auxiliary macro for decoding instructions of the form:
// | opcode(4) | SWITCH_BIT(1) | op(3)
// Where the SWITCH_BIT decides which of the two instructions to call.
//
// These are relatively common, since operands are encoded in 3 bits (op),
// so single-operand instructions only use up 8 mappings.
// Since their opcodes are 4 bits, there are still 8 mappings, which are
// shared with another instruction.
macro_rules! decode_row {
    ($opcode: expr, $ops: ident, $instr_set: ident, $instr_unset: ident) => ({
        const SWITCH_BIT: u8 = 0x4;
        let op = to_operand($opcode & 0x7 as u8);
        if $opcode & SWITCH_BIT > 0 {
            $ops.$instr_set(op);
        } else {
            $ops.$instr_unset(op);
        }
    })
}

// Auxiliary macro for decoding instructions of the form:
// | opcode(2) | imm_op(3) | op(3)
//
// These happen a lot in the CB-prefixed instructions
macro_rules! decode_cb_block {
    ($opcode: expr, $ops: ident, $instr: ident) => ({
        const OP: u8 = 0x7;
        let op = to_operand($opcode & OP);
        let imm_op = ($opcode >> 3) & OP;

        $ops.$instr(imm_op, op);
    })
}

pub fn decode<O: CpuOps>(mut ops: O) {
    let opcode = ops.next_u8();
    match opcode {
        0x76 => ops.halt(),

        // LD from register to register
        // | 0 1 | op2(3) | op1(3)
        0x40...0x7F => {
            let op1 = to_operand(opcode & 0x7);
            let op2 = to_operand((opcode >> 3) & 0x7);
            ops.load(op1, op2);
        }

        // ADD and ADC
        // | 1 0 0 0 | CARRY_BIT(1) | operand(3)
        0x80...0x8F => decode_row!(opcode, ops, adc, add),

        // SUB and SBC
        // | 1 0 0 1 | CARRY_BIT(1) | operand(3)
        0x90...0x9F => decode_row!(opcode, ops, sbc, sub),

        // AND and XOR
        // | 1 0 1 0 | XOR_BIT(1) | operand(3)
        0xA0...0xAF => decode_row!(opcode, ops, xor, and),

        // OR and CP
        // | 1 0 1 1 | XOR_BIT(1) | operand(3)
        0xB0...0xBF => decode_row!(opcode, ops, or, cp),

        // RST
        // | 1 1 | RST_TO(3) | 1 1 1
        0xC0...0xFF if (opcode & 0x7 == 0x7) => ops.rst(opcode & 0x38),

        // 8-bit load
        0x06 => op2!(ops, load, Imm8(ops.next_u8()), B),
        0x0E => op2!(ops, load, Imm8(ops.next_u8()), C),
        0x16 => op2!(ops, load, Imm8(ops.next_u8()), D),
        0x1E => op2!(ops, load, Imm8(ops.next_u8()), E),
        0x26 => op2!(ops, load, Imm8(ops.next_u8()), H),
        0x2E => op2!(ops, load, Imm8(ops.next_u8()), L),
        0x36 => op2!(ops, load, Imm8(ops.next_u8()), IndirectAddr::HL),

        0x0A => ops.load(IndirectAddr::BC, A),
        0x1A => ops.load(IndirectAddr::DE, A),

        0x02 => ops.load(A, IndirectAddr::BC),
        0x12 => ops.load(A, IndirectAddr::DE),
        0xEA => op2!(ops, load, A, IndirectAddr::Imm16(ops.next_u16())),

        0xF2 => ops.load(A, IndirectAddr::C),
        0xE2 => ops.load(IndirectAddr::C, A),

        0x3A => println!("warning: Unimplemented LDD A, (HL)"),
        0x32 => println!("warning: Unimplemented LDD (HL), A"),
        0x2A => println!("warning: Unimplemented LDI A, (HL)"),
        0x22 => println!("warning: Unimplemented LDI (HL), A"),

        0xE0 => op2!(ops, load, A, IndirectAddr::Imm8(ops.next_u8())),
        0xF0 => op2!(ops, load, IndirectAddr::Imm8(ops.next_u8()), A),

        // 16-bit load
        0x01 => op2!(ops, load16, Imm16(ops.next_u16()), BC),
        0x11 => op2!(ops, load16, Imm16(ops.next_u16()), DE),
        0x21 => op2!(ops, load16, Imm16(ops.next_u16()), HL),
        0x31 => op2!(ops, load16, Imm16(ops.next_u16()), SP),
        0xF9 => ops.load16(HL, SP),
        0xF8 => op1!(ops, load16_hlsp, ops.next_u8() as i8),
        0x08 => op2!(ops, load16, SP, IndirectAddr::Imm16(ops.next_u16())),

        0xF5 => ops.push(AF),
        0xC5 => ops.push(BC),
        0xD5 => ops.push(DE),
        0xE5 => ops.push(HL),

        0xF1 => ops.pop(AF),
        0xC1 => ops.pop(BC),
        0xD1 => ops.pop(DE),
        0xE1 => ops.pop(HL),

        0xC6 => op1!(ops, add, Imm8(ops.next_u8())),
        0xCE => op1!(ops, adc, Imm8(ops.next_u8())),

        0xD6 => op1!(ops, sub, Imm8(ops.next_u8())),
        0xDE => op1!(ops, sbc, Imm8(ops.next_u8())),

        0xE6 => op1!(ops, and, Imm8(ops.next_u8())),
        0xEE => op1!(ops, xor, Imm8(ops.next_u8())),

        0xF6 => op1!(ops, or, Imm8(ops.next_u8())),
        0xFE => op1!(ops, cp, Imm8(ops.next_u8())),

        0x04 => ops.inc(B),
        0x0C => ops.inc(C),
        0x14 => ops.inc(D),
        0x1C => ops.inc(E),
        0x24 => ops.inc(F),
        0x2C => ops.inc(L),
        0x3C => ops.inc(A),
        0x34 => ops.inc(IndirectAddr::HL),

        0x05 => ops.dec(B),
        0x0D => ops.dec(C),
        0x15 => ops.dec(D),
        0x1D => ops.dec(E),
        0x25 => ops.dec(F),
        0x2D => ops.dec(L),
        0x3D => ops.dec(A),
        0x35 => ops.dec(IndirectAddr::HL),

        0x09 => ops.add16(BC),
        0x19 => ops.add16(DE),
        0x29 => ops.add16(HL),
        0x39 => ops.add16(SP),
        0xE8 => op1!(ops, add16_sp, Imm8(ops.next_u8())),

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
                0x00...0x0F => decode_row!(next_opcode, ops, rrc, rlc),
                0x10...0x1F => decode_row!(next_opcode, ops, rl, rr),
                0x20...0x2F => decode_row!(next_opcode, ops, sla, sra),
                0x30...0x3F => decode_row!(next_opcode, ops, srl, swap),
                0x40...0x7F => decode_cb_block!(next_opcode, ops, bit),
                0x80...0xBF => decode_cb_block!(next_opcode, ops, res),
                0xC0...0xFF => decode_cb_block!(next_opcode, ops, set),
                _ => panic!("warning: Unknown opcode 0xCB{:02x}", next_opcode)
            }
        },

        // control
        0xC3 => op2!(ops, jp, ops.next_u16(), Cond::None),
        0xC2 => op2!(ops, jp, ops.next_u16(), Cond::NZ),
        0xCA => op2!(ops, jp, ops.next_u16(), Cond::Z),
        0xD2 => op2!(ops, jp, ops.next_u16(), Cond::NC),
        0xDA => op2!(ops, jp, ops.next_u16(), Cond::C),
        0xE9 => ops.jp_hl(),

        0x18 => op2!(ops, jr, ops.next_u8(), Cond::None),
        0x20 => op2!(ops, jr, ops.next_u8(), Cond::NZ),
        0x28 => op2!(ops, jr, ops.next_u8(), Cond::Z),
        0x30 => op2!(ops, jr, ops.next_u8(), Cond::NC),
        0x38 => op2!(ops, jr, ops.next_u8(), Cond::C),

        0xCD => op2!(ops, call, ops.next_u16(), Cond::None),
        0xC4 => op2!(ops, call, ops.next_u16(), Cond::NZ),
        0xCC => op2!(ops, call, ops.next_u16(), Cond::Z),
        0xD4 => op2!(ops, call, ops.next_u16(), Cond::NC),
        0xDC => op2!(ops, call, ops.next_u16(), Cond::C),

        0xC9 => ops.ret(Cond::None),
        0xC0 => ops.ret(Cond::NZ),
        0xC8 => ops.ret(Cond::Z),
        0xD0 => ops.ret(Cond::NC),
        0xD8 => ops.ret(Cond::C),

        0xD9 => ops.reti(),

        _ => println!("warning: Unknown opcode 0x{:02x}", opcode)
    }
}
