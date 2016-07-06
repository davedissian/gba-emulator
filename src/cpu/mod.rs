mod registers;
mod ops;
mod fetcher;
mod decoder;

// Implementations
pub mod interpreter;

/*
    Instruction Layout
    ====================

    This analysis is based upon the Zilog Z80 manual with modifications done by the GB/GBC
    such as removing the instructions involving the IX and IY registers.

    [] = 1 byte

    Addressing Modes
    --------------------
    Immediate Addressing: [op][operand] (1 or 2 byte op code)
        Operand is a single byte, such as loading the accumulator with a constant.
    Immediate Extended Addressing: [op][low][high] (1 or 2 byte op code)
        Operand is split into two bytes, such as to load the HL register pair with 16 bits
        of data.
    Register Addressing:
        Many Z80 opcodes specify register to registers directly
    Implied Addressing:
        This indicates OP codes imply registers, such as arithmetic instructions always
        implying that the destination is the accumulator (A in AF register).
    Register Indirect Addressing: [op] (1 or 2 byte op code)
        This specifies a 16 bit register pair to be used as a pointer to any location in
        memory. Such as loading the accumulator with data pointed to in the HR register.


    Modified Page Zero Addressing: [op] (1 byte op code)
        Eight special locations in page 0 of memory, depending on which version of CALL is
        called.
    Relative Addressing: [op][displacement] (1 byte op code)
        Used for Jump Relative, displacement is 8 bit twos complement offset from A+2 (where
        A is the current PC value)
    Extended Addressing: [op][lowaddr][highaddr] (1 or 2 byte op code)
        Used to jump to any location in 16 bit memory

*/

// Control Conditions
#[derive(Debug, PartialEq, Eq)]
pub enum Cond {
    None, NZ, Z, NC, C
}

// Indirect Addressing
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndirectAddr {
    BC, DE, HL,     // (BC/DE/HL)
    C,              // (FF00 + C)
    Imm8(u8),       // (FF00 + n)
    Imm16(u16),     // (nn)
}

