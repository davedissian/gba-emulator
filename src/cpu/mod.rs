mod registers;
mod ops;
mod fetcher;
mod decoder;

// Implementations
pub mod interpreter;

// Control Conditions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

