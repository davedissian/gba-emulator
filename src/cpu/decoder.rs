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
        partial_type!($t)(Box::new(move |addr| $mnem(imm_type!($t -> addr))))
    };
    ($mnem: ident $op1: expr) => {
        Done($mnem($op1))
    };
    ($mnem: ident $t: ident#, $op2: expr) => {
        partial_type!($t)(Box::new(|addr| $mnem(imm_type!($t -> addr), $op2)))
    };
    ($mnem: ident $op1: expr, $t: ident#) => {
        partial_type!($t)(Box::new(move |addr| $mnem($op1, imm_type!($t -> addr))))
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

// count the number of elements within square brackets
macro_rules! count {
    ([]) => {0};
    ([$e: tt $(,$es: tt)*]) => {1 + count!([$($es),*])}
}

// [1, 0, 1, 0] => 10
macro_rules! assemble_bits {
    ([$e: expr $(,$es: expr)*]) => {assemble_bits_acc!([$($es),*],$e)}
}

// helper for assemble_bits
macro_rules! assemble_bits_acc {
    ([], $acc: expr) => {$acc};

    ([$e: expr $(,$es: expr)*], $acc: expr) => {
        assemble_bits_acc!([$($es),*], ($acc << 1) | $e)
    }
}

macro_rules! mask_aux {
    ($acc: expr, [#$n: ident $size: expr]) => {$acc << $size};
    ($acc: expr, $t: tt) => {assemble_bits_acc!($t, $acc)};
}

macro_rules! assemble_mask {
    ($acc: expr,) => {$acc};
    ($acc: expr, $t: tt $($ts: tt)*) => {
        assemble_mask!(mask_aux!($acc, $t), $($ts)*)
    }
}

macro_rules! wildcard_aux {
    ($acc: expr, [#$n: ident $size: expr]) => {$acc << $size};
    ($acc: expr, $t: tt) => {{
        let shift = count!($t);
        ($acc << shift) | ((1 << shift) - 1)
    }};
}

macro_rules! wildcard_mask {
    ($acc: expr,) => {$acc};
    ($acc: expr, $t: tt $($ts: tt)*) => {
        wildcard_mask!(wildcard_aux!($acc, $t), $($ts)*)
    }
}

macro_rules! op_shift {
    () => {0};
    ([#$n: ident $size: expr] $($ts: tt)*) => {$size + op_shift!($($ts)*)};
    ($t: tt $($ts: tt)*) => {count!($t) + op_shift!($($ts)*)}
}

macro_rules! transform {
    ($op: expr, () => ($leaf: expr)) => {$leaf};
    ($op: expr, () => [$($leaf: tt)+]) => {instr!($($leaf)+)};

    ($op: expr, ([#$n: ident $size: expr] $($ts: tt)*) => $leaf: tt) => {{
        let shift = op_shift!($($ts)*);
        let mask  = ((1 << $size) - 1) << shift;
        let $n = to_operand(($op & mask) >> shift);

        transform!($op, ($($ts)*) => $leaf)
    }};

    ($op: expr, ($t: tt $($ts: tt)*) => $leaf: tt) => {{
        transform!($op, ($($ts)*) => $leaf)
    }};
}

macro_rules! matches {
    ($op: expr, #$e: expr) => {$op == $e};
    ($op: expr, $($t: tt)+) => {{
        let mask = assemble_mask!(0, $($t)+);
        let wildcard = wildcard_mask!(0, $($t)+);
        $op & wildcard == mask
    }};
}

macro_rules! match_rule {
    ($op: expr, $(($($t: tt)+) => $e: tt,)+) => {
        $( if matches!($op, $($t)+) {
                transform!($op, ($($t)+) => $e)
            } else
        )+ {panic!("No match: {}", $op)}
    }
}

pub fn decode(opcode: u8) -> Cont<Instruction> {
    match_rule!(opcode,

        (#0x76) => [HALT],
        ([0,1] [#to 3] [#from 3]) => [LD to, from],
        ([1,0,0,0,1] [#op 3]) => [ADC op],
        ([1,0,0,0,0] [#op 3]) => [ADD op],
        ([1,0,0,1,1] [#op 3]) => [SBC op],
        ([1,0,0,1,0] [#op 3]) => [SUB op],
        ([1,0,1,0,1] [#op 3]) => [XOR op],
        ([1,0,1,0,0] [#op 3]) => [AND op],
        ([1,0,1,1,1] [#op 3]) => [OR op],
        ([1,0,1,1,0] [#op 3]) => [CP op],
        ([1,1] [#op 3] [1,1,1]) => [RST op],
        ([0,0] [#dest 3] [1,1,0]) => [LD dest, I#],

        (#0x01) => [LD16 BC, I16#],
        (#0x11) => [LD16 DE, I16#],
        (#0x21) => [LD16 HL, I16#],
        (#0x31) => [LD16 SP, I16#],
        (#0x27) => [DAA],
        (#0x2F) => [CPL],
        (#0x3F) => [CCF],
        (#0x37) => [SCF],
        (#0x00) => [NOP],
        (#0x10) => [STOP],
        (#0xF3) => [DI],
        (#0xFB) => [EI],
        (#0x1A) => [LD Reg(A), Ind(IndirectAddr::DE)],
        (#0x02) => [LD Ind(IndirectAddr::BC), Reg(A)],
        (#0x12) => [LD Ind(IndirectAddr::DE), Reg(A)],
        (#0xEA) => [LD A#, Reg(A)],
        (#0xFA) => [LD Reg(A), A#],
        (#0xF2) => [LD Ind(IndirectAddr::C), Reg(A)],
        (#0xE2) => [LD Reg(A), Ind(IndirectAddr::C)],
        (#0x3A) => [LDD Reg(A), Ind(IndirectAddr::HL)],
        (#0x32) => [LDD Ind(IndirectAddr::HL), Reg(A)],
        (#0x2A) => [LDI Reg(A), Ind(IndirectAddr::HL)],
        (#0x22) => [LDI Ind(IndirectAddr::HL), Reg(A)],
        (#0x0A) => [LD Reg(A), Ind(IndirectAddr::BC)],

        (#0xE0) => [LDH A#, Reg(A)],
        (#0xF0) => [LDH Reg(A), A#],
        // 16-bit load
        (#0x01) => [LD16 BC, I16#],
        (#0x11) => [LD16 DE, I16#],
        (#0x21) => [LD16 HL, I16#],
        (#0x31) => [LD16 SP, I16#],

        (#0xF9) => [LDSPHL],

        (#0xF8) => [LDHL I#], // TODO: immediate should be signed!

        (#0x08) => [LDSP A16#],

        (#0xF5) => [PUSH AF],
        (#0xC5) => [PUSH BC],
        (#0xD5) => [PUSH DE],
        (#0xE5) => [PUSH HL],

        (#0xF1) => [POP AF],
        (#0xC1) => [POP BC],
        (#0xD1) => [POP DE],
        (#0xE1) => [POP HL],

        (#0xC6) => [ADD I#],
        (#0xCE) => [ADC I#],

        (#0xD6) => [SUB I#],
        (#0xDE) => [SBC I#],

        (#0xE6) => [AND I#],
        (#0xEE) => [XOR I#],

        (#0xF6) => [OR I#],
        (#0xFE) => [CP I#],

        (#0x04) => [INC Reg(B)],
        (#0x0C) => [INC Reg(C)],
        (#0x14) => [INC Reg(D)],
        (#0x1C) => [INC Reg(E)],
        (#0x24) => [INC Reg(F)],
        (#0x2C) => [INC Reg(L)],
        (#0x3C) => [INC Reg(A)],
        (#0x34) => [INC Ind(IndirectAddr::HL)],

        (#0x05) => [DEC Reg(B)],
        (#0x0D) => [DEC Reg(C)],
        (#0x15) => [DEC Reg(D)],
        (#0x1D) => [DEC Reg(E)],
        (#0x25) => [DEC Reg(F)],
        (#0x2D) => [DEC Reg(L)],
        (#0x3D) => [DEC Reg(A)],
        (#0x35) => [DEC Ind(IndirectAddr::HL)],

        (#0x09) => [ADDHL BC],
        (#0x19) => [ADDHL DE],
        (#0x29) => [ADDHL HL],
        (#0x39) => [ADDHL SP],
        (#0xE8) => [ADDSP I#],

        (#0x03) => [INC16 BC],
        (#0x13) => [INC16 DE],
        (#0x23) => [INC16 HL],
        (#0x33) => [INC16 SP],

        (#0x0B) => [DEC16 BC],
        (#0x1B) => [DEC16 DE],
        (#0x2B) => [DEC16 HL],
        (#0x3B) => [DEC16 SP],

        // rotates and shifts
        (#0x0F) => [RRCA],
        (#0x1f) => [RRA],
        (#0xCB) => (Partial8(Box::new(|cb| decode_cb(cb)))),

        // control
        (#0xC3) => [JP A#, Cond::None],
        (#0xC2) => [JP A#, Cond::NZ],
        (#0xCA) => [JP A#, Cond::Z],
        (#0xD2) => [JP A#, Cond::NC],
        (#0xDA) => [JP A#, Cond::C],
        (#0xE9) => [JP Ind(IndirectAddr::HL), Cond::None],

        (#0x18) => [JR A#, Cond::None],
        (#0x20) => [JR A#, Cond::NZ],
        (#0x28) => [JR A#, Cond::Z],
        (#0x30) => [JR A#, Cond::NC],
        (#0x38) => [JR A#, Cond::C],

        (#0xCD) => [CALL A#, Cond::None],
        (#0xC4) => [CALL A#, Cond::NZ],
        (#0xCC) => [CALL A#, Cond::Z],
        (#0xD4) => [CALL A#, Cond::NC],
        (#0xDC) => [CALL A#, Cond::C],

        (#0xC9) => [RET Cond::None],
        (#0xC0) => [RET Cond::NZ],
        (#0xC8) => [RET Cond::Z],
        (#0xD0) => [RET Cond::NC],
        (#0xD8) => [RET Cond::C],

        (#0xD9) => [RETI],
        (#0x07) => [RLCA],
        (#0x17) => [RLCA],

        // UNMAPPED:
        (#0xD3) => [NOP],
        (#0xDB) => [NOP],
        (#0xDD) => [NOP],
        (#0xE3) => [NOP],
        (#0xE4) => [NOP],
        (#0xEB) => [NOP],
        (#0xEC) => [NOP],
        (#0xED) => [NOP],
        (#0xF4) => [NOP],
        (#0xFC) => [NOP],
        (#0xFD) => [NOP],
    )
}
