/* Decoder */

use cpu::Op8;
use cpu::Op8::*;
use cpu::Cond;
use cpu::ops::Cont;
use cpu::ops::Cont::*;
use cpu::ops::Instruction;
use cpu::ops::Instruction::*;
use cpu::registers::Reg8::*;
use cpu::registers::Reg16;
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
        partial_type!($t)(Box::new(move |addr| $mnem(imm_type!($t -> addr), $op2)))
    };
    ($mnem: ident $op1: expr, $t: ident#) => {
        partial_type!($t)(Box::new(move |addr| $mnem($op1, imm_type!($t -> addr))))
    };
    ($mnem: ident $op1: expr, $op2: expr) => {
        Done($mnem($op1, $op2))
    };
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
        let $n = ($op & mask) >> shift;

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

fn reg8(operand: u8) -> Op8 {
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

// PUSH and POP might use the AF register instead of SP
// AF and SP are mutually exclusive for a given type of instruction
fn reg16(operand: u8, has_af: bool) -> Reg16 {
    match operand {
         0x0 => BC,
         0x1 => DE,
         0x2 => HL,
         0x3 if has_af => AF,
         0x3 => SP,
         _   => panic!("Invalid operand")
    }
}

fn cond(operand: u8) -> Cond {
    match operand {
        0x0 => Cond::NZ,
        0x1 => Cond::Z,
        0x2 => Cond::NC,
        0x3 => Cond::C,
        _   => panic!("Invalid operand")
    }
}


pub fn decode_cb(code: u8) -> Instruction {
    let cont = match_rule!(code,
        // | opcode(5) | op(3)
        ([0,0,0,0,0] [#op 3]) => [RLC  reg8(op)],
        ([0,0,0,0,1] [#op 3]) => [RRC  reg8(op)],
        ([0,0,0,1,0] [#op 3]) => [RR   reg8(op)],
        ([0,0,0,1,1] [#op 3]) => [RL   reg8(op)],
        ([0,0,1,0,0] [#op 3]) => [SRA  reg8(op)],
        ([0,0,1,0,1] [#op 3]) => [SLA  reg8(op)],
        ([0,0,1,1,0] [#op 3]) => [SWAP reg8(op)],
        ([0,0,1,1,1] [#op 3]) => [SRL  reg8(op)],
        // | opcode(2) | imm_op(3) | op(3)
        ([0,1] [#imm_op 3] [#op 3]) => [BIT imm_op, reg8(op)],
        ([1,0] [#imm_op 3] [#op 3]) => [RES imm_op, reg8(op)],
        ([1,1] [#imm_op 3] [#op 3]) => [SET imm_op, reg8(op)],
    );
    match cont {
        Done(i) => i,
        _       => panic!("CB instructions should not take immediate data")
    }
}

pub fn decode(opcode: u8) -> Cont<Instruction> {
    match_rule!(opcode,
        (#0x76) => [HALT],

        ([0,0] [#dest 3] [1,1,0])   => [LD  reg8(dest), I#],
        ([0,0] [#op   3] [1,0,0])   => [INC reg8(op)],
        ([0,0] [#op   3] [1,0,1])   => [DEC reg8(op)],
        ([0,1] [#to   3] [#from 3]) => [LD  reg8(to), reg8(from)],
        ([1,1] [#op   3] [1,1,1])   => [RST reg8(op)],

        ([0,0,1] [#op 2] [0,0,0])   => [JR   A#, cond(op)],
        ([1,1,0] [#op 2] [0,0,0])   => [RET      cond(op)],
        ([1,1,0] [#op 2] [0,1,0])   => [JP   A#, cond(op)],
        ([1,1,0] [#op 2] [1,0,0])   => [CALL A#, cond(op)],

        ([1,0,0,0,0] [#op 3])       => [ADD reg8(op)],
        ([1,0,0,0,1] [#op 3])       => [ADC reg8(op)],
        ([1,0,0,1,0] [#op 3])       => [SUB reg8(op)],
        ([1,0,0,1,1] [#op 3])       => [SBC reg8(op)],
        ([1,0,1,0,0] [#op 3])       => [AND reg8(op)],
        ([1,0,1,0,1] [#op 3])       => [XOR reg8(op)],
        ([1,0,1,1,0] [#op 3])       => [CP  reg8(op)],
        ([1,0,1,1,1] [#op 3])       => [OR  reg8(op)],

        // 16 bit
        ([0,0] [#op 2] [0,0,0,1])   => [LD16  reg16(op, false), I16#],
        ([0,0] [#op 2] [0,0,1,1])   => [INC16 reg16(op, false)],
        ([0,0] [#op 2] [1,0,0,1])   => [ADDHL reg16(op, false)],
        ([0,0] [#op 2] [1,0,1,1])   => [DEC16 reg16(op, false)],
        ([1,1] [#op 2] [0,0,0,1])   => [POP   reg16(op, true)],
        ([1,1] [#op 2] [0,1,0,1])   => [PUSH  reg16(op, true)],

        (#0xE8) => [ADDSP I#],

        // control
        (#0x18) => [JR A#, Cond::None],
        (#0xC3) => [JP A#, Cond::None],
        (#0xC9) => [RET Cond::None],
        (#0xCD) => [CALL A#, Cond::None],
        (#0xE9) => [JP Ind(IndirectAddr::HL), Cond::None],

        (#0x27) => [DAA],
        (#0x2F) => [CPL],
        (#0x3F) => [CCF],
        (#0x37) => [SCF],
        (#0x00) => [NOP],
        (#0x10) => [STOP],
        (#0xF3) => [DI],
        (#0xFB) => [EI],

        (#0x02) => [LD Ind(IndirectAddr::BC), Reg(A)],
        (#0x12) => [LD Ind(IndirectAddr::DE), Reg(A)],
        (#0xEA) => [LD A#, Reg(A)],
        (#0xF2) => [LD Ind(IndirectAddr::C), Reg(A)],

        (#0x1A) => [LD Reg(A), Ind(IndirectAddr::DE)],
        (#0xFA) => [LD Reg(A), A#],
        (#0xE2) => [LD Reg(A), Ind(IndirectAddr::C)],
        (#0x0A) => [LD Reg(A), Ind(IndirectAddr::BC)],

        (#0x3A) => [LDD Reg(A), Ind(IndirectAddr::HL)],
        (#0x32) => [LDD Ind(IndirectAddr::HL), Reg(A)],

        (#0x2A) => [LDI Reg(A), Ind(IndirectAddr::HL)],
        (#0x22) => [LDI Ind(IndirectAddr::HL), Reg(A)],

        (#0xE0) => [LDH A#, Reg(A)],
        (#0xF0) => [LDH Reg(A), A#],
        (#0xF9) => [LDSPHL],
        (#0xF8) => [LDHL I#], // TODO: immediate should be signed!
        (#0x08) => [LDSP A16#],

        (#0xC6) => [ADD I#],
        (#0xCE) => [ADC I#],
        (#0xD6) => [SUB I#],
        (#0xDE) => [SBC I#],
        (#0xE6) => [AND I#],
        (#0xEE) => [XOR I#],
        (#0xF6) => [OR  I#],
        (#0xFE) => [CP  I#],

        // rotates and shifts
        (#0x0F) => [RRCA],
        (#0x1f) => [RRA],
        (#0xCB) => (Partial8(Box::new(|cb| decode_cb(cb)))),

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
