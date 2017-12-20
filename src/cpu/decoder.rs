/* Decoder */

use cpu::Cond;
use cpu::ops::Arg8;
use cpu::ops::Arg16;
use cpu::ops::Cont;
use cpu::ops::Cont::*;
use cpu::ops::Instruction;
use cpu::ops::Instruction::*;
use cpu::registers::Reg8::*;
use cpu::registers::Reg16::*;
use cpu::IndirectAddr;

/*
 * Examples for instruction decoding:
 *
 * (#0xAB) => [Instr]
 *  If opcode matches 0xAB, decode as Instr
 *
 * ([0b01000,5] [#arg, 3]) => [Instr arg]
 *  If opcode matches 0b01000xxx, then extract xxx as the argument and decode as Instr
 *
 * ([0b10, 2] [#arg, 3] [0b110, 3]) => [Instr arg ]
 *  If opcode matches 0b10xxx110, then extract xxx as the argument and decode as Instr
 *
 * Instructions themselves can sometimes require more than 8 bits. Therefore, the [..] syntax can
 * instead build a function that constructions an instruction given more data. For example:
 *  - [Instr] becomes Done(Instr)
 *  - [Instr arg] becomes Done(Instr(arg))
 *  - [Instr a, b] becomes Done(Instr(a, b))
 *  - [Instr A#, arg] becomes Partial8(|data| Instr(Ind(IndirectAddr::Imm8(data)), arg))
 *  - [Instr arg, I16#] becomes Partial16(|data| Instr(arg, data))
 */

macro_rules! imm_type {
    (A   -> $i: ident) => { Arg8::Ind(IndirectAddr::Imm8($i)) };
    (I   -> $i: ident) => { Arg8::Imm($i) };
    (A16 -> $i: ident) => { Arg16::Ind(IndirectAddr::Imm16($i)) };
    (I16 -> $i: ident) => { Arg16::Imm($i) };
    (SI  -> $i: ident) => { $i as i8 }
}

macro_rules! partial_type {
    (A)   => { Partial8  };
    (I)   => { Partial8  };
    (SI)  => { Partial8  };
    (I16) => { Partial16 };
    (A16) => { Partial16 }
}

// A convenience macro that builds a Cont<Instruction> from an instruction mmemonic and arguments
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

// Calculate the bit shift of an argument from the op code to extract it
// e.g. Given  [0b010, 3] [#arg, 3] [0b10, 2]. Extract arg by shifting by 2.
macro_rules! op_shift {
    () => {0};
    ([#$n: ident, $size: expr] $($ts: tt)*) => {$size + op_shift!($($ts)*)};
    ([$n: expr, $size: expr] $($ts: tt)*) => {$size + op_shift!($($ts)*)}
}

// Transform an instruction rule into code which extracts the op-code data
macro_rules! transform {
    // If we have parsed the left hand side...

    // If the RHS is an expression in parentheses, then return it literally
    ($op: expr, () => ($leaf: expr)) => {$leaf};

    // Otherwise, build the Cond<Instruction> using the instr! macro
    ($op: expr, () => [$($leaf: tt)+]) => {instr!($($leaf)+)};

    // If we reach an argument [#ident, <size>] then extract it as ident
    ($op: expr, ([#$n: ident, $size: expr] $($ts: tt)*) => $leaf: tt) => {{
        let shift: u8 = op_shift!($($ts)*);
        let mask:  u8 = ((1 << $size as u8) - 1) << shift;
        let $n:    u8 = ($op & mask) >> shift;

        transform!($op, ($($ts)*) => $leaf)
    }};

    // If we reach a [<pattern>, <size>] node, skip it
    ($op: expr, ($t: tt $($ts: tt)*) => $leaf: tt) => {{
        transform!($op, ($($ts)*) => $leaf)
    }};
}

// Construct a test which determines whether the op-code matches a given pattern
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

// Does the op-code given match the given op-code expression?
macro_rules! matches {
    ($op: expr, #$e: expr) => {$op == $e};
    ($op: expr, $($t: tt)+) => {
        mask_test!(0, 0, $op, $($t)+)
    };
}

// Transform rules into a series of if-else statements
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

fn reg8(operand: u8) -> Arg8 {
    match operand {
        0x0 => Arg8::Reg(B),
        0x1 => Arg8::Reg(C),
        0x2 => Arg8::Reg(D),
        0x3 => Arg8::Reg(E),
        0x4 => Arg8::Reg(H),
        0x5 => Arg8::Reg(L),
        0x6 => Arg8::Ind(IndirectAddr::HL),
        0x7 => Arg8::Reg(A),
        _   => panic!("Invalid operand")
    }
}

// PUSH and POP might use the AF register instead of SP
// AF and SP are mutually exclusive for a given type of instruction
fn reg16(operand: u8, has_af: bool) -> Arg16 {
    match operand {
         0x0 => Arg16::Reg(BC),
         0x1 => Arg16::Reg(DE),
         0x2 => Arg16::Reg(HL),
         0x3 if has_af => Arg16::Reg(AF),
         0x3 => Arg16::Reg(SP),
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
        ([0b00010, 5] [#op, 3]) => [RL   reg8(op)],
        ([0b00011, 5] [#op, 3]) => [RR   reg8(op)],
        ([0b00100, 5] [#op, 3]) => [SLA  reg8(op)],
        ([0b00101, 5] [#op, 3]) => [SRA  reg8(op)],
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

        ([0b001, 3] [#op, 2] [0b000, 3]) => [JR   cond(op), SI#],
        ([0b110, 3] [#op, 2] [0b000, 3]) => [RET  cond(op)],
        ([0b110, 3] [#op, 2] [0b010, 3]) => [JP   cond(op), I16#],
        ([0b110, 3] [#op, 2] [0b100, 3]) => [CALL cond(op), I16#],

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
        ([0b00, 2] [#op, 2] [0b1001, 4]) => [ADD16 reg16(op, false)],
        ([0b00, 2] [#op, 2] [0b1011, 4]) => [DEC16 reg16(op, false)],
        ([0b11, 2] [#op, 2] [0b0001, 4]) => [POP   reg16(op, true)],
        ([0b11, 2] [#op, 2] [0b0101, 4]) => [PUSH  reg16(op, true)],

        (#0xE8) => [ADD16SP SI#],

        // control
        (#0x18) => [JR   Cond::None, SI#],
        (#0xC3) => [JP   Cond::None, I16#],
        (#0xC9) => [RET  Cond::None],
        (#0xCD) => [CALL Cond::None, I16#],
        (#0xE9) => [JP   Cond::None, Arg16::Ind(IndirectAddr::HL)],

        (#0x27) => [DAA],
        (#0x2F) => [CPL],
        (#0x3F) => [CCF],
        (#0x37) => [SCF],
        (#0x00) => [NOP],
        (#0x10) => [STOP],
        (#0xF3) => [DI],
        (#0xFB) => [EI],

        (#0x02) => [LD Arg8::Ind(IndirectAddr::BC), Arg8::Reg(A)],
        (#0x12) => [LD Arg8::Ind(IndirectAddr::DE), Arg8::Reg(A)],
        (#0xEA) => [LD A#, Arg8::Reg(A)],
        (#0xF2) => [LD Arg8::Reg(A), Arg8::Ind(IndirectAddr::C)],

        (#0x1A) => [LD Arg8::Reg(A), Arg8::Ind(IndirectAddr::DE)],
        (#0xFA) => [LD Arg8::Reg(A), A#],
        (#0xE2) => [LD Arg8::Ind(IndirectAddr::C), Arg8::Reg(A)],
        (#0x0A) => [LD Arg8::Reg(A), Arg8::Ind(IndirectAddr::BC)],

        (#0x3A) => [LDD Arg8::Reg(A), Arg8::Ind(IndirectAddr::HL)],
        (#0x32) => [LDD Arg8::Ind(IndirectAddr::HL), Arg8::Reg(A)],

        (#0x2A) => [LDI Arg8::Reg(A), Arg8::Ind(IndirectAddr::HL)],
        (#0x22) => [LDI Arg8::Ind(IndirectAddr::HL), Arg8::Reg(A)],

        (#0xE0) => [LDH A#, Arg8::Reg(A)],
        (#0xF0) => [LDH Arg8::Reg(A), A#],
        (#0xF9) => [LD16 Arg16::Reg(SP), Arg16::Reg(HL)],
        (#0xF8) => [LDHL16 SI#],
        (#0x08) => [LD16 A16#, Arg16::Reg(SP)],

        (#0xC6) => [ADD I#],
        (#0xCE) => [ADC I#],
        (#0xD6) => [SUB I#],
        (#0xDE) => [SBC I#],
        (#0xE6) => [AND I#],
        (#0xEE) => [XOR I#],
        (#0xF6) => [OR  I#],
        (#0xFE) => [CP  I#],

        // rotates and shifts
        (#0x0F) => [RRC Arg8::Reg(A)],
        (#0x1f) => [RR Arg8::Reg(A)],
        (#0xCB) => (Partial8(Box::new(|cb| decode_cb(cb)))),

        (#0xD9) => [RETI],
        (#0x07) => [RLC Arg8::Reg(A)],
        (#0x17) => [RL Arg8::Reg(A)],

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
    use cpu::ops::Arg8;
    use cpu::ops::Arg16;
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
        LD16(Arg16::Reg(BC), Arg16::Imm(0)),
        LD(Arg8::Ind(IndirectAddr::BC), Arg8::Reg(A)),
        INC16(Arg16::Reg(BC)),
        INC(Arg8::Reg(B)),
        DEC(Arg8::Reg(B)),
        LD(Arg8::Reg(B), Arg8::Imm(0)),
        RLC(Arg8::Reg(A)),
        LD16(Arg16::Ind(IndirectAddr::Imm16(0)), Arg16::Reg(SP)),
        ADD16(Arg16::Reg(BC)),
        LD(Arg8::Reg(A), Arg8::Ind(IndirectAddr::BC)),
        DEC16(Arg16::Reg(BC)),
        INC(Arg8::Reg(C)),
        DEC(Arg8::Reg(C)),
        LD(Arg8::Reg(C), Arg8::Imm(0)),
        RRC(Arg8::Reg(A)),
        STOP,
        LD16(Arg16::Reg(DE), Arg16::Imm(0)),
        LD(Arg8::Ind(IndirectAddr::DE), Arg8::Reg(A)),
        INC16(Arg16::Reg(DE)),
        INC(Arg8::Reg(D)),
        DEC(Arg8::Reg(D)),
        LD(Arg8::Reg(D), Arg8::Imm(0)),
        RL(Arg8::Reg(A)),
        JR(Cond::None, 0),
        ADD16(Arg16::Reg(DE)),
        LD(Arg8::Reg(A), Arg8::Ind(IndirectAddr::DE)),
        DEC16(Arg16::Reg(DE)),
        INC(Arg8::Reg(E)),
        DEC(Arg8::Reg(E)),
        LD(Arg8::Reg(E), Arg8::Imm(0)),
        RR(Arg8::Reg(A)),
        JR(Cond::NZ, 0),
        LD16(Arg16::Reg(HL), Arg16::Imm(0)),
        LDI(Arg8::Ind(IndirectAddr::HL), Arg8::Reg(A)),
        INC16(Arg16::Reg(HL)),
        INC(Arg8::Reg(H)),
        DEC(Arg8::Reg(H)),
        LD(Arg8::Reg(H), Arg8::Imm(0)),
        DAA,
        JR(Cond::Z, 0),
        ADD16(Arg16::Reg(HL)),
        LDI(Arg8::Reg(A), Arg8::Ind(IndirectAddr::HL)),
        DEC16(Arg16::Reg(HL)),
        INC(Arg8::Reg(L)),
        DEC(Arg8::Reg(L)),
        LD(Arg8::Reg(L), Arg8::Imm(0)),
        CPL,
        JR(Cond::NC, 0),
        LD16(Arg16::Reg(SP), Arg16::Imm(0)),
        LDD(Arg8::Ind(IndirectAddr::HL), Arg8::Reg(A)),
        INC16(Arg16::Reg(SP)),
        INC(Arg8::Ind(IndirectAddr::HL)),
        DEC(Arg8::Ind(IndirectAddr::HL)),
        LD(Arg8::Ind(IndirectAddr::HL), Arg8::Imm(0)),
        SCF,
        JR(Cond::C, 0),
        ADD16(Arg16::Reg(SP)),
        LDD(Arg8::Reg(A), Arg8::Ind(IndirectAddr::HL)),
        DEC16(Arg16::Reg(SP)),
        INC(Arg8::Reg(A)),
        DEC(Arg8::Reg(A)),
        LD(Arg8::Reg(A), Arg8::Imm(0)),
        CCF,
        LD(Arg8::Reg(B), Arg8::Reg(B)),
        LD(Arg8::Reg(B), Arg8::Reg(C)),
        LD(Arg8::Reg(B), Arg8::Reg(D)),
        LD(Arg8::Reg(B), Arg8::Reg(E)),
        LD(Arg8::Reg(B), Arg8::Reg(H)),
        LD(Arg8::Reg(B), Arg8::Reg(L)),
        LD(Arg8::Reg(B), Arg8::Ind(IndirectAddr::HL)),
        LD(Arg8::Reg(B), Arg8::Reg(A)),
        LD(Arg8::Reg(C), Arg8::Reg(B)),
        LD(Arg8::Reg(C), Arg8::Reg(C)),
        LD(Arg8::Reg(C), Arg8::Reg(D)),
        LD(Arg8::Reg(C), Arg8::Reg(E)),
        LD(Arg8::Reg(C), Arg8::Reg(H)),
        LD(Arg8::Reg(C), Arg8::Reg(L)),
        LD(Arg8::Reg(C), Arg8::Ind(IndirectAddr::HL)),
        LD(Arg8::Reg(C), Arg8::Reg(A)),
        LD(Arg8::Reg(D), Arg8::Reg(B)),
        LD(Arg8::Reg(D), Arg8::Reg(C)),
        LD(Arg8::Reg(D), Arg8::Reg(D)),
        LD(Arg8::Reg(D), Arg8::Reg(E)),
        LD(Arg8::Reg(D), Arg8::Reg(H)),
        LD(Arg8::Reg(D), Arg8::Reg(L)),
        LD(Arg8::Reg(D), Arg8::Ind(IndirectAddr::HL)),
        LD(Arg8::Reg(D), Arg8::Reg(A)),
        LD(Arg8::Reg(E), Arg8::Reg(B)),
        LD(Arg8::Reg(E), Arg8::Reg(C)),
        LD(Arg8::Reg(E), Arg8::Reg(D)),
        LD(Arg8::Reg(E), Arg8::Reg(E)),
        LD(Arg8::Reg(E), Arg8::Reg(H)),
        LD(Arg8::Reg(E), Arg8::Reg(L)),
        LD(Arg8::Reg(E), Arg8::Ind(IndirectAddr::HL)),
        LD(Arg8::Reg(E), Arg8::Reg(A)),
        LD(Arg8::Reg(H), Arg8::Reg(B)),
        LD(Arg8::Reg(H), Arg8::Reg(C)),
        LD(Arg8::Reg(H), Arg8::Reg(D)),
        LD(Arg8::Reg(H), Arg8::Reg(E)),
        LD(Arg8::Reg(H), Arg8::Reg(H)),
        LD(Arg8::Reg(H), Arg8::Reg(L)),
        LD(Arg8::Reg(H), Arg8::Ind(IndirectAddr::HL)),
        LD(Arg8::Reg(H), Arg8::Reg(A)),
        LD(Arg8::Reg(L), Arg8::Reg(B)),
        LD(Arg8::Reg(L), Arg8::Reg(C)),
        LD(Arg8::Reg(L), Arg8::Reg(D)),
        LD(Arg8::Reg(L), Arg8::Reg(E)),
        LD(Arg8::Reg(L), Arg8::Reg(H)),
        LD(Arg8::Reg(L), Arg8::Reg(L)),
        LD(Arg8::Reg(L), Arg8::Ind(IndirectAddr::HL)),
        LD(Arg8::Reg(L), Arg8::Reg(A)),
        LD(Arg8::Ind(IndirectAddr::HL), Arg8::Reg(B)),
        LD(Arg8::Ind(IndirectAddr::HL), Arg8::Reg(C)),
        LD(Arg8::Ind(IndirectAddr::HL), Arg8::Reg(D)),
        LD(Arg8::Ind(IndirectAddr::HL), Arg8::Reg(E)),
        LD(Arg8::Ind(IndirectAddr::HL), Arg8::Reg(H)),
        LD(Arg8::Ind(IndirectAddr::HL), Arg8::Reg(L)),
        HALT,
        LD(Arg8::Ind(IndirectAddr::HL), Arg8::Reg(A)),
        LD(Arg8::Reg(A), Arg8::Reg(B)),
        LD(Arg8::Reg(A), Arg8::Reg(C)),
        LD(Arg8::Reg(A), Arg8::Reg(D)),
        LD(Arg8::Reg(A), Arg8::Reg(E)),
        LD(Arg8::Reg(A), Arg8::Reg(H)),
        LD(Arg8::Reg(A), Arg8::Reg(L)),
        LD(Arg8::Reg(A), Arg8::Ind(IndirectAddr::HL)),
        LD(Arg8::Reg(A), Arg8::Reg(A)),
        ADD(Arg8::Reg(B)),
        ADD(Arg8::Reg(C)),
        ADD(Arg8::Reg(D)),
        ADD(Arg8::Reg(E)),
        ADD(Arg8::Reg(H)),
        ADD(Arg8::Reg(L)),
        ADD(Arg8::Ind(IndirectAddr::HL)),
        ADD(Arg8::Reg(A)),
        ADC(Arg8::Reg(B)),
        ADC(Arg8::Reg(C)),
        ADC(Arg8::Reg(D)),
        ADC(Arg8::Reg(E)),
        ADC(Arg8::Reg(H)),
        ADC(Arg8::Reg(L)),
        ADC(Arg8::Ind(IndirectAddr::HL)),
        ADC(Arg8::Reg(A)),
        SUB(Arg8::Reg(B)),
        SUB(Arg8::Reg(C)),
        SUB(Arg8::Reg(D)),
        SUB(Arg8::Reg(E)),
        SUB(Arg8::Reg(H)),
        SUB(Arg8::Reg(L)),
        SUB(Arg8::Ind(IndirectAddr::HL)),
        SUB(Arg8::Reg(A)),
        SBC(Arg8::Reg(B)),
        SBC(Arg8::Reg(C)),
        SBC(Arg8::Reg(D)),
        SBC(Arg8::Reg(E)),
        SBC(Arg8::Reg(H)),
        SBC(Arg8::Reg(L)),
        SBC(Arg8::Ind(IndirectAddr::HL)),
        SBC(Arg8::Reg(A)),
        AND(Arg8::Reg(B)),
        AND(Arg8::Reg(C)),
        AND(Arg8::Reg(D)),
        AND(Arg8::Reg(E)),
        AND(Arg8::Reg(H)),
        AND(Arg8::Reg(L)),
        AND(Arg8::Ind(IndirectAddr::HL)),
        AND(Arg8::Reg(A)),
        XOR(Arg8::Reg(B)),
        XOR(Arg8::Reg(C)),
        XOR(Arg8::Reg(D)),
        XOR(Arg8::Reg(E)),
        XOR(Arg8::Reg(H)),
        XOR(Arg8::Reg(L)),
        XOR(Arg8::Ind(IndirectAddr::HL)),
        XOR(Arg8::Reg(A)),
        OR(Arg8::Reg(B)),
        OR(Arg8::Reg(C)),
        OR(Arg8::Reg(D)),
        OR(Arg8::Reg(E)),
        OR(Arg8::Reg(H)),
        OR(Arg8::Reg(L)),
        OR(Arg8::Ind(IndirectAddr::HL)),
        OR(Arg8::Reg(A)),
        CP(Arg8::Reg(B)),
        CP(Arg8::Reg(C)),
        CP(Arg8::Reg(D)),
        CP(Arg8::Reg(E)),
        CP(Arg8::Reg(H)),
        CP(Arg8::Reg(L)),
        CP(Arg8::Ind(IndirectAddr::HL)),
        CP(Arg8::Reg(A)),
        RET(Cond::NZ),
        POP(Arg16::Reg(BC)),
        JP(Cond::NZ, Arg16::Imm(0)),
        JP(Cond::None, Arg16::Imm(0)),
        CALL(Cond::NZ, Arg16::Imm(0)),
        PUSH(Arg16::Reg(BC)),
        ADD(Arg8::Imm(0)),
        RST(0),
        RET(Cond::Z),
        RET(Cond::None),
        JP(Cond::Z, Arg16::Imm(0)),
        NOP, // CB
        CALL(Cond::Z, Arg16::Imm(0)),
        CALL(Cond::None, Arg16::Imm(0)),
        ADC(Arg8::Imm(0)),
        RST(8),
        RET(Cond::NC),
        POP(Arg16::Reg(DE)),
        JP(Cond::NC, Arg16::Imm(0)),
        NOP,
        CALL(Cond::NC, Arg16::Imm(0)),
        PUSH(Arg16::Reg(DE)),
        SUB(Arg8::Imm(0)),
        RST(16),
        RET(Cond::C),
        RETI,
        JP(Cond::C, Arg16::Imm(0)),
        NOP,
        CALL(Cond::C, Arg16::Imm(0)),
        NOP,
        SBC(Arg8::Imm(0)),
        RST(24),
        LDH(Arg8::Ind(IndirectAddr::Imm8(0)), Arg8::Reg(A)),
        POP(Arg16::Reg(HL)),
        LD(Arg8::Ind(IndirectAddr::C), Arg8::Reg(A)),
        NOP,
        NOP,
        PUSH(Arg16::Reg(HL)),
        AND(Arg8::Imm(0)),
        RST(32),
        ADD16SP(0),
        JP(Cond::None, Arg16::Ind(IndirectAddr::HL)),
        LD(Arg8::Ind(IndirectAddr::Imm8(0)), Arg8::Reg(A)),
        NOP,
        NOP,
        NOP,
        XOR(Arg8::Imm(0)),
        RST(40),
        LDH(Arg8::Reg(A), Arg8::Ind(IndirectAddr::Imm8(0))),
        POP(Arg16::Reg(AF)),
        LD(Arg8::Reg(A), Arg8::Ind(IndirectAddr::C)),
        DI,
        NOP,
        PUSH(Arg16::Reg(AF)),
        OR(Arg8::Imm(0)),
        RST(48),
        LDHL16(0),
        LD16(Arg16::Reg(SP), Arg16::Reg(HL)),
        LD(Arg8::Reg(A), Arg8::Ind(IndirectAddr::Imm8(0))),
        EI,
        NOP,
        NOP,
        CP(Arg8::Imm(0)),
        RST(56)
    ];
}
