/* Decoder */

use cpu::Cond;
use cpu::ops::Op8;
use cpu::ops::Op8::*;
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

macro_rules! mask_test {
    ($acc_m: expr, $acc_w: expr, $op: expr, ) => {$op & $acc_w == $acc_m};

    ($acc_m: expr, $acc_w: expr, $op: expr, [#$n: ident, $size: expr] $($ts: tt)*) => {
        mask_test!(  ($acc_m << $size as u8)
                   , ($acc_w << $size as u8)
                   , $op
                   , $($ts)*)
    };

    ($acc_m: expr, $acc_w: expr, $op: expr, [$n: expr, $size: expr] $($ts: tt)*) => {
        mask_test!(  ($acc_m << $size as u8) | $n as u8
                   , ($acc_w << $size as u8) | ((1 << $size as u8) - 1 as u8)
                   , $op
                   , $($ts)*)
    }
}

macro_rules! op_shift {
    () => {0};
    ([#$n: ident, $size: expr] $($ts: tt)*) => {$size + op_shift!($($ts)*)};
    ([$n: expr, $size: expr] $($ts: tt)*) => {$size + op_shift!($($ts)*)}
}

macro_rules! transform {
    ($op: expr, () => ($leaf: expr)) => {$leaf};
    ($op: expr, () => [$($leaf: tt)+]) => {instr!($($leaf)+)};

    ($op: expr, ([#$n: ident, $size: expr] $($ts: tt)*) => $leaf: tt) => {{
        let shift: u8 = op_shift!($($ts)*);
        let mask:  u8 = ((1 << $size as u8) - 1) << shift;
        let $n:    u8 = ($op & mask) >> shift;

        transform!($op, ($($ts)*) => $leaf)
    }};

    ($op: expr, ($t: tt $($ts: tt)*) => $leaf: tt) => {{
        transform!($op, ($($ts)*) => $leaf)
    }};
}

macro_rules! matches {
    ($op: expr, #$e: expr) => {$op == $e};
    ($op: expr, $($t: tt)+) => {
        mask_test!(0, 0, $op, $($t)+)
    };
}

macro_rules! match_rule {
    ($op: expr, $(($($t: tt)+) => $e: tt,)+) => {
        $( if matches!($op, $($t)+) {
                transform!($op, ($($t)+) => $e)
            } else
        )+ {panic!("No match: {}", $op)}
    }
}

fn shift_left(operand: u8, by: u8) -> u8 {
    operand << by
}

fn reg8(operand: u8) -> Op8 {
    match operand {
        0x0 => Reg(B),
        0x1 => Reg(C),
        0x2 => Reg(D),
        0x3 => Reg(E),
        0x4 => Reg(H),
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
        ([0b00000, 5] [#op, 3]) => [RLC  reg8(op)],
        ([0b00001, 5] [#op, 3]) => [RRC  reg8(op)],
        ([0b00010, 5] [#op, 3]) => [RR   reg8(op)],
        ([0b00011, 5] [#op, 3]) => [RL   reg8(op)],
        ([0b00100, 5] [#op, 3]) => [SRA  reg8(op)],
        ([0b00101, 5] [#op, 3]) => [SLA  reg8(op)],
        ([0b00110, 5] [#op, 3]) => [SWAP reg8(op)],
        ([0b00111, 5] [#op, 3]) => [SRL  reg8(op)],
        // | opcode(2) | imm_op(3) | op(3)
        ([0b01, 2] [#imm_op, 3] [#op, 3]) => [BIT imm_op, reg8(op)],
        ([0b10, 2] [#imm_op, 3] [#op, 3]) => [RES imm_op, reg8(op)],
        ([0b11, 2] [#imm_op, 3] [#op, 3]) => [SET imm_op, reg8(op)],
    );
    match cont {
        Done(i) => i,
        _       => panic!("CB instructions should not take immediate data")
    }
}

pub fn decode(opcode: u8) -> Cont<Instruction> {
    match_rule!(opcode,
        (#0x76) => [HALT],

        ([0b00, 2] [#dst, 3] [0b110, 3]) => [LD  reg8(dst), I#],
        ([0b00, 2] [#op,  3] [0b100, 3]) => [INC reg8(op)],
        ([0b00, 2] [#op,  3] [0b101, 3]) => [DEC reg8(op)],
        ([0b01, 2] [#to,  3] [#from, 3]) => [LD  reg8(to), reg8(from)],
        ([0b11, 2] [#op,  3] [0b111, 3]) => [RST shift_left(op, 3)],

        ([0b001, 3] [#op, 2] [0b000, 3]) => [JR   cond(op), A#],
        ([0b110, 3] [#op, 2] [0b000, 3]) => [RET  cond(op)],
        ([0b110, 3] [#op, 2] [0b010, 3]) => [JP   cond(op), A#],
        ([0b110, 3] [#op, 2] [0b100, 3]) => [CALL cond(op), A#],

        ([0b10000, 5] [#op, 3])          => [ADD reg8(op)],
        ([0b10001, 5] [#op, 3])          => [ADC reg8(op)],
        ([0b10010, 5] [#op, 3])          => [SUB reg8(op)],
        ([0b10011, 5] [#op, 3])          => [SBC reg8(op)],
        ([0b10100, 5] [#op, 3])          => [AND reg8(op)],
        ([0b10101, 5] [#op, 3])          => [XOR reg8(op)],
        ([0b10110, 5] [#op, 3])          => [OR  reg8(op)],
        ([0b10111, 5] [#op, 3])          => [CP  reg8(op)],

        // 16 bit
        ([0b00, 2] [#op, 2] [0b0001, 4]) => [LD16  reg16(op, false), I16#],
        ([0b00, 2] [#op, 2] [0b0011, 4]) => [INC16 reg16(op, false)],
        ([0b00, 2] [#op, 2] [0b1001, 4]) => [ADDHL reg16(op, false)],
        ([0b00, 2] [#op, 2] [0b1011, 4]) => [DEC16 reg16(op, false)],
        ([0b11, 2] [#op, 2] [0b0001, 4]) => [POP   reg16(op, true)],
        ([0b11, 2] [#op, 2] [0b0101, 4]) => [PUSH  reg16(op, true)],

        (#0xE8) => [ADDSP I#],

        // control
        (#0x18) => [JR   Cond::None, A#],
        (#0xC3) => [JP   Cond::None, A#],
        (#0xC9) => [RET  Cond::None],
        (#0xCD) => [CALL Cond::None, A#],
        (#0xE9) => [JP   Cond::None, Ind(IndirectAddr::HL)],

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
        (#0xF2) => [LD Reg(A), Ind(IndirectAddr::C)],

        (#0x1A) => [LD Reg(A), Ind(IndirectAddr::DE)],
        (#0xFA) => [LD Reg(A), A#],
        (#0xE2) => [LD Ind(IndirectAddr::C), Reg(A)],
        (#0x0A) => [LD Reg(A), Ind(IndirectAddr::BC)],

        (#0x3A) => [LDD Reg(A), Ind(IndirectAddr::HL)],
        (#0x32) => [LDD Ind(IndirectAddr::HL), Reg(A)],

        (#0x2A) => [LDI Reg(A), Ind(IndirectAddr::HL)],
        (#0x22) => [LDI Ind(IndirectAddr::HL), Reg(A)],

        (#0xE0) => [LDH A#, Reg(A)],
        (#0xF0) => [LDH Reg(A), A#],
        (#0xF9) => [LDSPHL],
        (#0xF8) => [LDHLSP I#], // TODO: immediate should be signed!
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
        (#0x17) => [RLA],

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

#[cfg(test)]
mod test {
    use cpu::decoder;
    use cpu::ops::Instruction;
    use cpu::ops::Instruction::*;
    use cpu::ops::Cont;
    use cpu::Cond;
    use cpu::Op8::*;
    use cpu::IndirectAddr;
    use cpu::registers::Reg8::*;
    use cpu::registers::Reg16::*;

    fn fetch_custom(op: u8) -> Instruction {
        match decoder::decode(op) {
            Cont::Partial8(f)  => f(0),
            Cont::Partial16(f) => f(0),
            Cont::Done(d)      => d,
        }
    }

    // Panics if any of the instructions is not handled in the decoder
    #[test]
    fn decode_totality() {
        for code in 0x00..0xFF {
            decoder::decode(code);
        }
        decoder::decode(0xFF);
    }

    #[test]
    fn decode_cb_totality() {
        for code in 0x00..0xFF {
            decoder::decode_cb(code);
        }
        decoder::decode(0xFF);
    }

    #[test]
    fn decode_correctness() {
        for code in 0x00..0xFF {
            if code == 0xCB {
                continue;
            }
            assert_eq!(fetch_custom(code), REF[code as usize]);
        }
        assert_eq!(fetch_custom(0xFF), REF[0xFF]);
    }

// Decode reference output (adapted from the manual):
// The opcode is the index within the array
static REF: &'static [Instruction] = &[
    NOP,
    LD16(BC, 0),
    LD(Ind(IndirectAddr::BC), Reg(A)),
    INC16(BC),
    INC(Reg(B)),
    DEC(Reg(B)),
    LD(Reg(B), Imm(0)),
    RLCA,
    LDSP(0),
    ADDHL(BC),
    LD(Reg(A), Ind(IndirectAddr::BC)),
    DEC16(BC),
    INC(Reg(C)),
    DEC(Reg(C)),
    LD(Reg(C), Imm(0)),
    RRCA,
    STOP,
    LD16(DE, 0),
    LD(Ind(IndirectAddr::DE), Reg(A)),
    INC16(DE),
    INC(Reg(D)),
    DEC(Reg(D)),
    LD(Reg(D), Imm(0)),
    RLA,
    JR(Cond::None, Ind(IndirectAddr::Imm8(0))),
    ADDHL(DE),
    LD(Reg(A), Ind(IndirectAddr::DE)),
    DEC16(DE),
    INC(Reg(E)),
    DEC(Reg(E)),
    LD(Reg(E), Imm(0)),
    RRA,
    JR(Cond::NZ, Ind(IndirectAddr::Imm8(0))),
    LD16(HL, 0),
    LDI(Ind(IndirectAddr::HL), Reg(A)),
    INC16(HL),
    INC(Reg(H)),
    DEC(Reg(H)),
    LD(Reg(H), Imm(0)),
    DAA,
    JR(Cond::Z, Ind(IndirectAddr::Imm8(0))),
    ADDHL(HL),
    LDI(Reg(A), Ind(IndirectAddr::HL)),
    DEC16(HL),
    INC(Reg(L)),
    DEC(Reg(L)),
    LD(Reg(L), Imm(0)),
    CPL,
    JR(Cond::NC, Ind(IndirectAddr::Imm8(0))),
    LD16(SP, 0),
    LDD(Ind(IndirectAddr::HL), Reg(A)),
    INC16(SP),
    INC(Ind(IndirectAddr::HL)),
    DEC(Ind(IndirectAddr::HL)),
    LD(Ind(IndirectAddr::HL), Imm(0)),
    SCF,
    JR(Cond::C, Ind(IndirectAddr::Imm8(0))),
    ADDHL(SP),
    LDD(Reg(A), Ind(IndirectAddr::HL)),
    DEC16(SP),
    INC(Reg(A)),
    DEC(Reg(A)),
    LD(Reg(A), Imm(0)),
    CCF,
    LD(Reg(B), Reg(B)),
    LD(Reg(B), Reg(C)),
    LD(Reg(B), Reg(D)),
    LD(Reg(B), Reg(E)),
    LD(Reg(B), Reg(H)),
    LD(Reg(B), Reg(L)),
    LD(Reg(B), Ind(IndirectAddr::HL)),
    LD(Reg(B), Reg(A)),
    LD(Reg(C), Reg(B)),
    LD(Reg(C), Reg(C)),
    LD(Reg(C), Reg(D)),
    LD(Reg(C), Reg(E)),
    LD(Reg(C), Reg(H)),
    LD(Reg(C), Reg(L)),
    LD(Reg(C), Ind(IndirectAddr::HL)),
    LD(Reg(C), Reg(A)),
    LD(Reg(D), Reg(B)),
    LD(Reg(D), Reg(C)),
    LD(Reg(D), Reg(D)),
    LD(Reg(D), Reg(E)),
    LD(Reg(D), Reg(H)),
    LD(Reg(D), Reg(L)),
    LD(Reg(D), Ind(IndirectAddr::HL)),
    LD(Reg(D), Reg(A)),
    LD(Reg(E), Reg(B)),
    LD(Reg(E), Reg(C)),
    LD(Reg(E), Reg(D)),
    LD(Reg(E), Reg(E)),
    LD(Reg(E), Reg(H)),
    LD(Reg(E), Reg(L)),
    LD(Reg(E), Ind(IndirectAddr::HL)),
    LD(Reg(E), Reg(A)),
    LD(Reg(H), Reg(B)),
    LD(Reg(H), Reg(C)),
    LD(Reg(H), Reg(D)),
    LD(Reg(H), Reg(E)),
    LD(Reg(H), Reg(H)),
    LD(Reg(H), Reg(L)),
    LD(Reg(H), Ind(IndirectAddr::HL)),
    LD(Reg(H), Reg(A)),
    LD(Reg(L), Reg(B)),
    LD(Reg(L), Reg(C)),
    LD(Reg(L), Reg(D)),
    LD(Reg(L), Reg(E)),
    LD(Reg(L), Reg(H)),
    LD(Reg(L), Reg(L)),
    LD(Reg(L), Ind(IndirectAddr::HL)),
    LD(Reg(L), Reg(A)),
    LD(Ind(IndirectAddr::HL), Reg(B)),
    LD(Ind(IndirectAddr::HL), Reg(C)),
    LD(Ind(IndirectAddr::HL), Reg(D)),
    LD(Ind(IndirectAddr::HL), Reg(E)),
    LD(Ind(IndirectAddr::HL), Reg(H)),
    LD(Ind(IndirectAddr::HL), Reg(L)),
    HALT,
    LD(Ind(IndirectAddr::HL), Reg(A)),
    LD(Reg(A), Reg(B)),
    LD(Reg(A), Reg(C)),
    LD(Reg(A), Reg(D)),
    LD(Reg(A), Reg(E)),
    LD(Reg(A), Reg(H)),
    LD(Reg(A), Reg(L)),
    LD(Reg(A), Ind(IndirectAddr::HL)),
    LD(Reg(A), Reg(A)),
    ADD(Reg(B)),
    ADD(Reg(C)),
    ADD(Reg(D)),
    ADD(Reg(E)),
    ADD(Reg(H)),
    ADD(Reg(L)),
    ADD(Ind(IndirectAddr::HL)),
    ADD(Reg(A)),
    ADC(Reg(B)),
    ADC(Reg(C)),
    ADC(Reg(D)),
    ADC(Reg(E)),
    ADC(Reg(H)),
    ADC(Reg(L)),
    ADC(Ind(IndirectAddr::HL)),
    ADC(Reg(A)),
    SUB(Reg(B)),
    SUB(Reg(C)),
    SUB(Reg(D)),
    SUB(Reg(E)),
    SUB(Reg(H)),
    SUB(Reg(L)),
    SUB(Ind(IndirectAddr::HL)),
    SUB(Reg(A)),
    SBC(Reg(B)),
    SBC(Reg(C)),
    SBC(Reg(D)),
    SBC(Reg(E)),
    SBC(Reg(H)),
    SBC(Reg(L)),
    SBC(Ind(IndirectAddr::HL)),
    SBC(Reg(A)),
    AND(Reg(B)),
    AND(Reg(C)),
    AND(Reg(D)),
    AND(Reg(E)),
    AND(Reg(H)),
    AND(Reg(L)),
    AND(Ind(IndirectAddr::HL)),
    AND(Reg(A)),
    XOR(Reg(B)),
    XOR(Reg(C)),
    XOR(Reg(D)),
    XOR(Reg(E)),
    XOR(Reg(H)),
    XOR(Reg(L)),
    XOR(Ind(IndirectAddr::HL)),
    XOR(Reg(A)),
    OR(Reg(B)),
    OR(Reg(C)),
    OR(Reg(D)),
    OR(Reg(E)),
    OR(Reg(H)),
    OR(Reg(L)),
    OR(Ind(IndirectAddr::HL)),
    OR(Reg(A)),
    CP(Reg(B)),
    CP(Reg(C)),
    CP(Reg(D)),
    CP(Reg(E)),
    CP(Reg(H)),
    CP(Reg(L)),
    CP(Ind(IndirectAddr::HL)),
    CP(Reg(A)),
    RET(Cond::NZ),
    POP(BC),
    JP(Cond::NZ, Ind(IndirectAddr::Imm8(0))),
    JP(Cond::None, Ind(IndirectAddr::Imm8(0))),
    CALL(Cond::NZ, Ind(IndirectAddr::Imm8(0))),
    PUSH(BC),
    ADD(Imm(0)),
    RST(0),
    RET(Cond::Z),
    RET(Cond::None),
    JP(Cond::Z, Ind(IndirectAddr::Imm8(0))),
    NOP, // CB
    CALL(Cond::Z, Ind(IndirectAddr::Imm8(0))),
    CALL(Cond::None, Ind(IndirectAddr::Imm8(0))),
    ADC(Imm(0)),
    RST(8),
    RET(Cond::NC),
    POP(DE),
    JP(Cond::NC, Ind(IndirectAddr::Imm8(0))),
    NOP,
    CALL(Cond::NC, Ind(IndirectAddr::Imm8(0))),
    PUSH(DE),
    SUB(Imm(0)),
    RST(16),
    RET(Cond::C),
    RETI,
    JP(Cond::C, Ind(IndirectAddr::Imm8(0))),
    NOP,
    CALL(Cond::C, Ind(IndirectAddr::Imm8(0))),
    NOP,
    SBC(Imm(0)),
    RST(24),
    LDH(Ind(IndirectAddr::Imm8(0)), Reg(A)),
    POP(HL),
    LD(Ind(IndirectAddr::C), Reg(A)),
    NOP,
    NOP,
    PUSH(HL),
    AND(Imm(0)),
    RST(32),
    ADDSP(Imm(0)),
    JP(Cond::None, Ind(IndirectAddr::HL)),
    LD(Ind(IndirectAddr::Imm8(0)), Reg(A)),
    NOP,
    NOP,
    NOP,
    XOR(Imm(0)),
    RST(40),
    LDH(Reg(A), Ind(IndirectAddr::Imm8(0))),
    POP(AF),
    LD(Reg(A), Ind(IndirectAddr::C)),
    DI,
    NOP,
    PUSH(AF),
    OR(Imm(0)),
    RST(48),
    LDHLSP(Imm(0)),
    LDSPHL,
    LD(Reg(A), Ind(IndirectAddr::Imm8(0))),
    EI,
    NOP,
    NOP,
    CP(Imm(0)),
    RST(56)
];

}
