/* Decoder */

use cpu::Op8;
use cpu::Op8::*;
use cpu::Cond;
use cpu::ops::Cont;
use cpu::ops::Cont::*;
use cpu::ops::Instruction;
use cpu::ops::Instruction::*;
use cpu::registers::Reg8::*;
use cpu::registers::Reg16::*;
use cpu::IndirectAddr;

// TODO: add documentation for the macros
macro_rules! imm_type {
    (A   -> $i: ident) => { Ind(IndirectAddr::Imm8($i)) };
    (I   -> $i: ident) => { Imm($i) };
    (I16 -> $i: ident) => { $i };
    (A16 -> $i: ident) => { $i }
}

macro_rules! partial_type {
    (A)   => { Partial8  };
    (I)   => { Partial8  };
    (I16) => { Partial16 };
    (A16) => { Partial16 }
}

macro_rules! instr {
    ($mnem: ident) => {
        Done($mnem)
    };
    ($mnem: ident $t: ident#) => {
        partial_type!($t)(Box::new(|addr| $mnem(imm_type!($t -> addr))))
    };
    ($mnem: ident $op1: expr) => {
        Done($mnem($op1))
    };
    ($mnem: ident $t: ident#, $op2: expr) => {
        partial_type!($t)(Box::new(|addr| $mnem(imm_type!($t -> addr), $op2)))
    };
    ($mnem: ident $op1: expr, $t: ident#) => {
        partial_type!($t)(Box::new(|addr| $mnem($op1, imm_type!($t -> addr))))
    };
    ($mnem: ident $op1: expr, $op2: expr) => {
        Done($mnem($op1, $op2))
    };
}

// Auxiliary macro for decoding instructions of the form:
// | opcode(2) | imm_op(3) | op(3)
//
// These happen a lot in the CB-prefixed instructions
macro_rules! decode_cb_block {
    ($opcode: expr, $instr: ident) => ({
        const OP: u8 = 0x7;
        let op = to_operand($opcode & OP);
        let imm_op = ($opcode >> 3) & OP;
        $instr(imm_op, op)
    })
}

// Auxiliary macro for decoding instructions of the form:
// | opcode(4) | SWITCH_BIT(1) | op(3)
//
// Where the SWITCH_BIT decides which of the two instructions to call.
// These are relatively common, since operands are encoded in 3 bits (op),
// so single-operand instructions only use up 8 mappings.
// Since their opcodes are 4 bits, there are still 8 mappings, which are
// shared with another instruction.
macro_rules! decode_row {
    ($opcode: expr, $instr_set: ident, $instr_unset: ident) => ({
        const SWITCH_BIT: u8 = 0x4;
        let op = to_operand($opcode & 0x7 as u8);
        if $opcode & SWITCH_BIT > 0 {
            $instr_set(op)
        } else {
            $instr_unset(op)
        }
    })
}

fn to_operand(operand: u8) -> Op8 {
    match operand {
        0x0 => Reg(B),
        0x1 => Reg(C),
        0x2 => Reg(D),
        0x3 => Reg(E),
        0x4 => Reg(F),
        0x5 => Reg(L),
        0x6 => Ind(IndirectAddr::HL),
        0x7 => Reg(A),
        _   => panic!("Invalid operand")
    }
}

pub fn decode_cb(code: u8) -> Instruction {
    match code {
        0x00...0x0F => decode_row!(code, RRC, RLC),
        0x10...0x1F => decode_row!(code, RL, RR),
        0x20...0x2F => decode_row!(code, SLA, SRA),
        0x30...0x3F => decode_row!(code, SRL, SWAP),
        0x40...0x7F => decode_cb_block!(code, BIT),
        0x80...0xBF => decode_cb_block!(code, RES),
        0xC0...0xFF => decode_cb_block!(code, SET),
        _ => panic!("warning: Unknown opcode 0xCB{:02x}", code)
    }
}

pub fn decode(opcode: u8) -> Cont<Instruction> {
    match opcode {
        0x76 => instr!(HALT),

        // LD from register to register
        // | 0 1 | op2(3) | op1(3)
        0x40...0x7F => {
            let op1 = to_operand(opcode & 0x7);
            let op2 = to_operand((opcode >> 3) & 0x7);
            Done(LD(op2, op1))
        }

        // ADD and ADC
        // | 1 0 0 0 | CARRY_BIT(1) | operand(3)
        0x80...0x8F => Done(decode_row!(opcode, ADC, ADD)),

        // SUB and SBC
        // | 1 0 0 1 | CARRY_BIT(1) | operand(3)
        0x90...0x9F => Done(decode_row!(opcode, SBC, SUB)),

        // AND and XOR
        // | 1 0 1 0 | XOR_BIT(1) | operand(3)
        0xA0...0xAF => Done(decode_row!(opcode, XOR, AND)),

        // OR and CP
        // | 1 0 1 1 | XOR_BIT(1) | operand(3)
        0xB0...0xBF => Done(decode_row!(opcode, OR, CP)),

        // RST
        // | 1 1 | RST_TO(3) | 1 1 1
        0xC0...0xFF if (opcode & 0x7 == 0x7) => Done(RST(opcode & 0x38)),

        // Load 8 bits
        // | 0 0 | DEST(3)   | 1 1 0
        0x06...0x3E if (opcode & 0x7 == 0x6) => {
            let op = (opcode >> 3) & 0x7;
            Partial8(Box::new(move |val| LD(to_operand(op), Imm(val))))
        },

        0x0A => instr!(LD Reg(A), Ind(IndirectAddr::BC)),
        0x1A => instr!(LD Reg(A), Ind(IndirectAddr::DE)),

        0x02 => instr!(LD Ind(IndirectAddr::BC), Reg(A)),
        0x12 => instr!(LD Ind(IndirectAddr::DE), Reg(A)),

        0xEA => instr!(LD A#, Reg(A)),
        0xFA => instr!(LD Reg(A), A#),

        0xF2 => instr!(LD Ind(IndirectAddr::C), Reg(A)),
        0xE2 => instr!(LD Reg(A), Ind(IndirectAddr::C)),

        0x3A => instr!(LDD Reg(A), Ind(IndirectAddr::HL)),
        0x32 => instr!(LDD Ind(IndirectAddr::HL), Reg(A)),
        0x2A => instr!(LDI Reg(A), Ind(IndirectAddr::HL)),
        0x22 => instr!(LDI Ind(IndirectAddr::HL), Reg(A)),


        0xE0 => instr!(LDH A#, Reg(A)),
        0xF0 => instr!(LDH Reg(A), A#),

        // 16-bit load
        0x01 => instr!(LD16 BC, I16#),
        0x11 => instr!(LD16 DE, I16#),
        0x21 => instr!(LD16 HL, I16#),
        0x31 => instr!(LD16 SP, I16#),

        0xF9 => instr!(LDSPHL),

        0xF8 => instr!(LDHL I#), // TODO: immediate should be signed!

        0x08 => instr!(LDSP A16#),

        0xF5 => instr!(PUSH AF),
        0xC5 => instr!(PUSH BC),
        0xD5 => instr!(PUSH DE),
        0xE5 => instr!(PUSH HL),

        0xF1 => instr!(POP AF),
        0xC1 => instr!(POP BC),
        0xD1 => instr!(POP DE),
        0xE1 => instr!(POP HL),

        0xC6 => instr!(ADD I#),
        0xCE => instr!(ADC I#),

        0xD6 => instr!(SUB I#),
        0xDE => instr!(SBC I#),

        0xE6 => instr!(AND I#),
        0xEE => instr!(XOR I#),

        0xF6 => instr!(OR I#),
        0xFE => instr!(CP I#),

        0x04 => instr!(INC Reg(B)),
        0x0C => instr!(INC Reg(C)),
        0x14 => instr!(INC Reg(D)),
        0x1C => instr!(INC Reg(E)),
        0x24 => instr!(INC Reg(F)),
        0x2C => instr!(INC Reg(L)),
        0x3C => instr!(INC Reg(A)),
        0x34 => instr!(INC Ind(IndirectAddr::HL)),

        0x05 => instr!(DEC Reg(B)),
        0x0D => instr!(DEC Reg(C)),
        0x15 => instr!(DEC Reg(D)),
        0x1D => instr!(DEC Reg(E)),
        0x25 => instr!(DEC Reg(F)),
        0x2D => instr!(DEC Reg(L)),
        0x3D => instr!(DEC Reg(A)),
        0x35 => instr!(DEC Ind(IndirectAddr::HL)),

        0x09 => instr!(ADDHL BC),
        0x19 => instr!(ADDHL DE),
        0x29 => instr!(ADDHL HL),
        0x39 => instr!(ADDHL SP),
        0xE8 => instr!(ADDSP I#),

        0x03 => instr!(INC16 BC),
        0x13 => instr!(INC16 DE),
        0x23 => instr!(INC16 HL),
        0x33 => instr!(INC16 SP),

        0x0B => instr!(DEC16 BC),
        0x1B => instr!(DEC16 DE),
        0x2B => instr!(DEC16 HL),
        0x3B => instr!(DEC16 SP),

        // misc
        0x27 => instr!(DAA),
        0x2F => instr!(CPL),
        0x3F => instr!(CCF),
        0x37 => instr!(SCF),
        0x00 => instr!(NOP),
        0x10 => instr!(STOP),
        0xF3 => instr!(DI),
        0xFB => instr!(EI),

        // rotates and shifts
        0x0F => instr!(RRCA),
        0x1f => instr!(RRA),
        0xCB => Partial8(Box::new(|cb| decode_cb(cb))),

        // control
        0xC3 => instr!(JP A#, Cond::None),
        0xC2 => instr!(JP A#, Cond::NZ),
        0xCA => instr!(JP A#, Cond::Z),
        0xD2 => instr!(JP A#, Cond::NC),
        0xDA => instr!(JP A#, Cond::C),
        0xE9 => instr!(JP Ind(IndirectAddr::HL), Cond::None),

        0x18 => instr!(JR A#, Cond::None),
        0x20 => instr!(JR A#, Cond::NZ),
        0x28 => instr!(JR A#, Cond::Z),
        0x30 => instr!(JR A#, Cond::NC),
        0x38 => instr!(JR A#, Cond::C),

        0xCD => instr!(CALL A#, Cond::None),
        0xC4 => instr!(CALL A#, Cond::NZ),
        0xCC => instr!(CALL A#, Cond::Z),
        0xD4 => instr!(CALL A#, Cond::NC),
        0xDC => instr!(CALL A#, Cond::C),

        0xC9 => instr!(RET Cond::None),
        0xC0 => instr!(RET Cond::NZ),
        0xC8 => instr!(RET Cond::Z),
        0xD0 => instr!(RET Cond::NC),
        0xD8 => instr!(RET Cond::C),

        0xD9 => instr!(RETI),
        0x07 => instr!(RLCA),
        0x17 => instr!(RLCA),

        // UNMAPPED:
        0xD3 => instr!(NOP),
        0xDB => instr!(NOP),
        0xDD => instr!(NOP),
        0xE3 => instr!(NOP),
        0xE4 => instr!(NOP),
        0xEB => instr!(NOP),
        0xEC => instr!(NOP),
        0xED => instr!(NOP),
        0xF4 => instr!(NOP),
        0xFC => instr!(NOP),
        0xFD => instr!(NOP),

        _ => panic!("warning: Unknown opcode 0x{:02x}", opcode)
    }
}
