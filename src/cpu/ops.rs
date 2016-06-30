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
use cpu::Op8;
use cpu::registers::*;

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

// Instruction decoding is implemented in a continuation passing style.
pub enum Cont<R> {
    Partial8(Box<Fn(u8) -> R>),
    Partial16(Box<Fn(u16) -> R>),
    Done(R)
}

// TODO: this could be simplified
#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    ADC(Op8),
    ADDSP(Op8),
    ADD(Op8),
    AND(Op8),
    BIT(u8, Op8),
    CALL(Cond, Op8),
    CCF,
    CP(Op8),
    CPL,
    DAA,
    DEC(Op8),
    DI,
    EI,
    HALT,
    INC(Op8),
    JP(Cond, Op8),
    JR(Cond, Op8),
    LD(Op8, Op8),
    LDD(Op8, Op8),
    LDH(Op8, Op8),
    LDHL(Op8),
    LDI(Op8, Op8),
    NOP,
    OR(Op8),
    RES(u8, Op8),
    RET(Cond),
    RETI,
    RL(Op8),
    RLC(Op8),
    RLCA,
    RLA,
    RR(Op8),
    RRA,
    RRC(Op8),
    RRCA,
    RST(u8),
    SBC(Op8),
    SCF,
    SET(u8, Op8),
    SLA(Op8),
    SRA(Op8),
    SRL(Op8),
    STOP,
    SUB(Op8),
    SWAP(Op8),
    XOR(Op8),
    // 16 bit
    ADDHL(Reg16),
    ADD16(Reg16),
    AND16(Reg16),
    CP16(Reg16),
    DEC16(Reg16),
    INC16(Reg16),
    LD16(Reg16, u16),
    LDSP(u16),
    LDSPHL,
    LDHLSP(Op8),
    OR16(Reg16),
    POP(Reg16),
    PUSH(Reg16),
}

// TODO: this could be generated from the enum using a macro
pub trait CpuOps {
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
