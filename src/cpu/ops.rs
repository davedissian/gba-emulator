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

use cpu::Cond;
use cpu::IndirectAddr;
use cpu::registers::*;

// Instruction arguments
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arg8 {
    Reg(Reg8),
    Ind(IndirectAddr),
    Imm(u8)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arg16 {
    Reg(Reg16),
    Ind(IndirectAddr),
    Imm(u16)
}

// Instruction decoding is implemented in a continuation passing style.
pub enum Cont<R> {
    Partial8(Box<Fn(u8) -> R>),
    Partial16(Box<Fn(u16) -> R>),
    Done(R)
}

// Synchronised with the trait below
#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    // 8-bit load
    LD(Arg8, Arg8),     // LD out, in
    LDD(Arg8, Arg8),    // LDD out, in
    LDI(Arg8, Arg8),    // LDI out, in
    LDH(Arg8, Arg8),    // LDH out, in
    // 16-bit load
    LD16(Arg16, Arg16), // LD out, in   (nn -> reg16)
    LD16SPHL,           // LD SP, HL    (HL -> SP)
    LDHL16(Arg8),       // LD HL, SP+n  (SP+n -> HL, n is signed)
    LDSP(Arg16),        // LD (nn), SP  (put SP into addr nn)
    PUSH(Arg16),        // PUSH reg     (add contents of reg to stack, SP -= 2)
    POP(Arg16),         // POP reg      (copy contents at SP to reg, SP += 2)
    // 8-bit arithmetic
    ADD(Arg8),          // ADD A, in
    ADC(Arg8),          // ADC A, in
    SUB(Arg8),          // SUB A, in
    SBC(Arg8),          // SBC A, in
    AND(Arg8),          // AND A, in
    OR(Arg8),           // OR A, in
    XOR(Arg8),          // XOR A, in
    CP(Arg8),           // CP A, in     (compare)
    INC(Arg8),          // INC reg
    DEC(Arg8),          // DEC reg
    // 16-bit arithmetic
    ADD16(Arg16),       // ADD HL, in
    ADDSP(Arg8),        // ADD SP, in
    INC16(Arg16),       // INC reg
    DEC16(Arg16),       // DEC reg
    // misc
    NOP,                // NOP
    DAA,                // DAA
    CPL,                // CPL
    CCF,                // CCF
    SCF,                // SCF
    HALT,               // HALT
    STOP,               // STOP
    EI,                 // EI
    DI,                 // DI
    // rotate and shift
    RLC(Arg8),          // RLC inout
    RL(Arg8),           // RL inout
    RRC(Arg8),          // RRC inout
    RR(Arg8),           // RR inout
    SLA(Arg8),          // SLA inout
    SRA(Arg8),          // SRA inout
    SWAP(Arg8),         // SWAP inout
    SRL(Arg8),          // SRL inout
    // bit manipulation
    BIT(u8, Arg8),      // BIT b, reg
    SET(u8, Arg8),      // SET b, reg
    RES(u8, Arg8),      // RES b, reg
    // control
    JP(Cond, Arg8),     // JP nn / JP cond nn
    JR(Cond, Arg8),     // JR nn / JR cond nn
    CALL(Cond, Arg8),   // CALL nn / CALL cond nn
    RST(u8),            // RST n
    RET(Cond),          // RET / RET cond
    RETI,               // RETI
}
    
pub trait CpuOps {
    fn read_arg8(&self, arg: Arg8) -> u8;
    fn write_arg8(&mut self, arg: Arg8, data: u8);
    fn read_arg16(&self, arg: Arg16) -> u16;
    fn write_arg16(&mut self, arg: Arg16, data: u16);
    // 8-bit load
    fn ld(&mut self, o: Arg8, i: Arg8);
    fn ldd(&mut self, o: Arg8, i: Arg8);
    fn ldi(&mut self, o: Arg8, i: Arg8);
    fn ldh(&mut self, o: Arg8, i: Arg8);
    // 16-bit load
    fn ld16(&mut self, o: Arg16, i: Arg16);
    fn ld16_hlsp(&mut self, offset: i8);
    fn push(&mut self, i: Arg16);
    fn pop(&mut self, o: Arg16);
    // 8-bit arithmetic
    fn add(&mut self, i: Arg8);
    fn adc(&mut self, i: Arg8);
    fn sub(&mut self, i: Arg8);
    fn sbc(&mut self, i: Arg8);
    fn and(&mut self, i: Arg8);
    fn or(&mut self, i: Arg8);
    fn xor(&mut self, i: Arg8);
    fn cp(&mut self, i: Arg8);
    fn inc(&mut self, io: Arg8);
    fn dec(&mut self, io: Arg8);
    // 16-bit arithmetic
    fn add16(&mut self, i: Arg16);
    fn add16_sp(&mut self, i: u8);
    fn inc16(&mut self, io: Arg16);
    fn dec16(&mut self, io: Arg16);
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
    fn rlc(&mut self, io: Arg8);
    fn rl(&mut self, io: Arg8);
    fn rrc(&mut self, io: Arg8);
    fn rr(&mut self, io: Arg8);
    fn sla(&mut self, io: Arg8);
    fn sra(&mut self, io: Arg8);
    fn swap(&mut self, io: Arg8);
    fn srl(&mut self, io: Arg8);
    // bit manipulation
    fn bit(&mut self, bit_id: u8, o: Arg8);
    fn set(&mut self, bit_id: u8, o: Arg8);
    fn res(&mut self, bit_id: u8, o: Arg8);
    // control
    fn jp(&mut self, dest: u16, cond: Cond);        // JP n
    fn jp_hl(&mut self);                            // JP (HL)
    fn jr(&mut self, offset: u8, cond: Cond);       // TODO(David): is this a signed offset?
    fn call(&mut self, dest: u16, cond: Cond);
    fn rst(&mut self, offset: u8);
    fn ret(&mut self, cond: Cond);
    fn reti(&mut self);
}

// Dispatch function call
pub fn dispatch<T>(instr: Instruction, ops: &CpuOps) {
    match instr {
        _ => panic!("Unimplemented")
    }
}
