use gbc::memory;

// TODO: Move this elsewhere
// Extract bits between min and max inclusive
// eg: Extract bits 2-4 of 0b0111100 should yield 0b1110000
fn get_bits(number: u16, min: u16, max: u16) -> u16 {
    return (number >> min) & ((max - min) - 1);
}

fn get_bit(number: u16, bit: u16) -> u16 {
    return (number >> bit) & 0x1;
}

// Address modes for instructions
#[derive(Debug)]
enum AddressMode {
    // Register
    A, B, C, D, E, F, H, L,

    // Register Indirect
    BC, DE, HL,

    // External
    Ext(u16),

    // Immediate
    Imm(u8),
}

// Arithmetic operators
#[derive(Debug)]
enum Arithmetic {
    ADD,    // Add
    ADC,    // Add with Carry
    SUB,    // Sub
    SBC,    // Sub with Carry
    AND,
    XOR,
    OR,
    CP,     // Compare
    INC,    // Increment
    DEC,    // Decrement
}

// Jump, Call and Return Conditions
#[derive(Debug)]
enum Condition {
    None,
    Carry,
    NonCarry,
    Zero,
    NonZero,
    ParityEven,
    ParityOdd,
    SignNeg,
    SignPos,
    RegB0 // ???
}

// High 8 - LHS, Low 8 - RHS
// eg: BC register has B in high 8 and C in low 8
struct Registers {
    /*
        Flag Register:

        Bit  Name  Set Clr  Expl.
        7    zf    Z   NZ   Zero Flag
        6    n     -   -    Add/Sub-Flag (BCD)
        5    h     -   -    Half Carry Flag (BCD)
        4    cy    C   NC   Carry Flag
        3-0  -     -   -    Not used (always zero)
    */
    af: u16,    // Accumulator and Flags

    bc: u16,    // BC
    de: u16,    // DE
    hl: u16,    // HL
    sp: u16,    // Stack Pointer
    pc: u16     // Program Counter
}

pub struct CPU {
    pub running: bool,
    registers: Registers
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            running: true,

            // Initialise registers
            registers: Registers {
                af: 0,
                bc: 0,
                de: 0,
                hl: 0,
                sp: 0,
                pc: 0
            }
        }
    }

    pub fn tick(&mut self, memory: &mut memory::Memory) {
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
            Modified Page Zero Addressing: [op] (1 byte op code)
                Eight special locations in page 0 of memory, depending on which version of CALL is
                called.
            Relative Addressing: [op][displacement] (1 byte op code)
                Used for Jump Relative, displacement is 8 bit twos complement offset from A+2 (where
                A is the current PC value)
            Extended Addressing: [op][lowaddr][highaddr] (1 or 2 byte op code)
                Used to jump to any location in 16 bit memory
            Register Addressing:
                Many Z80 opcodes specify register to registers directly
            Implied Addressing:
                This indicates OP codes imply registers, such as arithmetic instructions always
                implying that the destination is the accumulator (A in AF register).
            Register Indirect Addressing: [op] (1 or 2 byte op code)
                This specifies a 16 bit register pair to be used as a pointer to any location in
                memory. Such as loading the accumulator with data pointed to in the HR register.


        */

        // Fetch an instruction opcode from memory
        let opcode_bytes = [
            memory.read(self.registers.pc),
            memory.read(self.registers.pc + 1)
        ];

        // Decode and execute
        self.registers.pc += match opcode_bytes[0] {
            // No op
            0x00 => { 1 },


            // Load
            // --------------------

            // Implied
            // TODO: Unsure how to handle this
            0xED => panic!("Implied IR Load instruction - unsure how to handle this"),

            // Register Destination
            0x7F => { self.dispatch_load(AddressMode::A, AddressMode::A); 1 },
            0x78 => { self.dispatch_load(AddressMode::B, AddressMode::A); 1 },
            0x79 => { self.dispatch_load(AddressMode::C, AddressMode::A); 1 },
            0x7A => { self.dispatch_load(AddressMode::D, AddressMode::A); 1 },
            0x7B => { self.dispatch_load(AddressMode::E, AddressMode::A); 1 },
            0x7C => { self.dispatch_load(AddressMode::F, AddressMode::A); 1 },
            0x7D => { self.dispatch_load(AddressMode::L, AddressMode::A); 1 },
            0x7E => { self.dispatch_load(AddressMode::HL, AddressMode::A); 1 },
            0x0A => { self.dispatch_load(AddressMode::BC, AddressMode::A); 1 },
            0x1A => { self.dispatch_load(AddressMode::DE, AddressMode::A); 1 },
            0xFD => match opcode_bytes[1] {
                0x3A => {
                    let ext_addr = AddressMode::Ext(memory.read_word(self.registers.pc + 2));
                    self.dispatch_load(ext_addr, AddressMode::A); 4
                },
                0x2E => {
                    let imm_addr = AddressMode::Imm(memory.read(self.registers.pc + 2));
                    self.dispatch_load(imm_addr, AddressMode::A); 3
                },
                _ => {
                    println!("warning: Unknown opcode 0x{:02x}{:02x}",
                             opcode_bytes[0],
                             opcode_bytes[1]); 2
                }
            },

            0x47 => { self.dispatch_load(AddressMode::A, AddressMode::B); 1 },
            0x40 => { self.dispatch_load(AddressMode::B, AddressMode::B); 1 },
            0x41 => { self.dispatch_load(AddressMode::C, AddressMode::B); 1 },
            0x42 => { self.dispatch_load(AddressMode::D, AddressMode::B); 1 },
            0x43 => { self.dispatch_load(AddressMode::E, AddressMode::B); 1 },
            0x44 => { self.dispatch_load(AddressMode::F, AddressMode::B); 1 },
            0x45 => { self.dispatch_load(AddressMode::L, AddressMode::B); 1 },
            0x46 => { self.dispatch_load(AddressMode::HL, AddressMode::B); 1 },

            0x4F => { self.dispatch_load(AddressMode::A, AddressMode::C); 1 },
            0x48 => { self.dispatch_load(AddressMode::B, AddressMode::C); 1 },
            0x49 => { self.dispatch_load(AddressMode::C, AddressMode::C); 1 },
            0x4A => { self.dispatch_load(AddressMode::D, AddressMode::C); 1 },
            0x4B => { self.dispatch_load(AddressMode::E, AddressMode::C); 1 },
            0x4C => { self.dispatch_load(AddressMode::F, AddressMode::C); 1 },
            0x4D => { self.dispatch_load(AddressMode::L, AddressMode::C); 1 },
            0x4E => { self.dispatch_load(AddressMode::HL, AddressMode::C); 1 },

            0x57 => { self.dispatch_load(AddressMode::A, AddressMode::D); 1 },
            0x50 => { self.dispatch_load(AddressMode::B, AddressMode::D); 1 },
            0x51 => { self.dispatch_load(AddressMode::C, AddressMode::D); 1 },
            0x52 => { self.dispatch_load(AddressMode::D, AddressMode::D); 1 },
            0x53 => { self.dispatch_load(AddressMode::E, AddressMode::D); 1 },
            0x54 => { self.dispatch_load(AddressMode::F, AddressMode::D); 1 },
            0x55 => { self.dispatch_load(AddressMode::L, AddressMode::D); 1 },
            0x56 => { self.dispatch_load(AddressMode::HL, AddressMode::D); 1 },

            0x5F => { self.dispatch_load(AddressMode::A, AddressMode::E); 1 },
            0x58 => { self.dispatch_load(AddressMode::B, AddressMode::E); 1 },
            0x59 => { self.dispatch_load(AddressMode::C, AddressMode::E); 1 },
            0x5A => { self.dispatch_load(AddressMode::D, AddressMode::E); 1 },
            0x5B => { self.dispatch_load(AddressMode::E, AddressMode::E); 1 },
            0x5C => { self.dispatch_load(AddressMode::F, AddressMode::E); 1 },
            0x5D => { self.dispatch_load(AddressMode::L, AddressMode::E); 1 },
            0x5E => { self.dispatch_load(AddressMode::HL, AddressMode::E); 1 },

            0x67 => { self.dispatch_load(AddressMode::A, AddressMode::H); 1 },
            0x60 => { self.dispatch_load(AddressMode::B, AddressMode::H); 1 },
            0x61 => { self.dispatch_load(AddressMode::C, AddressMode::H); 1 },
            0x62 => { self.dispatch_load(AddressMode::D, AddressMode::H); 1 },
            0x63 => { self.dispatch_load(AddressMode::E, AddressMode::H); 1 },
            0x64 => { self.dispatch_load(AddressMode::F, AddressMode::H); 1 },
            0x65 => { self.dispatch_load(AddressMode::L, AddressMode::H); 1 },
            0x66 => { self.dispatch_load(AddressMode::HL, AddressMode::H); 1 },

            0x6F => { self.dispatch_load(AddressMode::A, AddressMode::L); 1 },
            0x68 => { self.dispatch_load(AddressMode::B, AddressMode::L); 1 },
            0x69 => { self.dispatch_load(AddressMode::C, AddressMode::L); 1 },
            0x6A => { self.dispatch_load(AddressMode::D, AddressMode::L); 1 },
            0x6B => { self.dispatch_load(AddressMode::E, AddressMode::L); 1 },
            0x6C => { self.dispatch_load(AddressMode::F, AddressMode::L); 1 },
            0x6D => { self.dispatch_load(AddressMode::L, AddressMode::L); 1 },
            0x6E => { self.dispatch_load(AddressMode::HL, AddressMode::L); 1 },

            // All these instructions are immediate addressing
            0xDD => {
                let imm_addr = AddressMode::Imm(memory.read(self.registers.pc + 2));
                match opcode_bytes[1] {
                    0xD5 => self.dispatch_load(imm_addr, AddressMode::B),
                    0xDE => self.dispatch_load(imm_addr, AddressMode::C),
                    0x1B => self.dispatch_load(imm_addr, AddressMode::D),
                    0x1E => self.dispatch_load(imm_addr, AddressMode::E),
                    0x2B => self.dispatch_load(imm_addr, AddressMode::H),
                    0x36 => self.dispatch_load(imm_addr, AddressMode::L),
                    0x78 => self.dispatch_load(imm_addr, AddressMode::HL), // Indirect Destination
                    _ => panic!("Unknown opcode 0x{:02x}{:02x}", opcode_bytes[0], opcode_bytes[1])
                };
                3 // All consume 3 bytes
            }

            // Register Indirect Destination
            0x77 => { self.dispatch_load(AddressMode::A, AddressMode::HL); 1 },
            0x70 => { self.dispatch_load(AddressMode::B, AddressMode::HL); 1 },
            0x71 => { self.dispatch_load(AddressMode::C, AddressMode::HL); 1 },
            0x72 => { self.dispatch_load(AddressMode::D, AddressMode::HL); 1 },
            0x73 => { self.dispatch_load(AddressMode::E, AddressMode::HL); 1 },
            0x74 => { self.dispatch_load(AddressMode::F, AddressMode::HL); 1 },
            0x75 => { self.dispatch_load(AddressMode::L, AddressMode::HL); 1 },

            0x02 => { self.dispatch_load(AddressMode::A, AddressMode::BC); 1 },

            0x12 => { self.dispatch_load(AddressMode::A, AddressMode::DE); 1 },

            // External Address Destination
            0x32 => {
                let ext_addr = AddressMode::Ext(memory.read_word(self.registers.pc + 1));
                self.dispatch_load(AddressMode::A, ext_addr);
                3
            },

            // --------------------


            // Arithmetic
            // --------------------

            // Add
            0x87 => { self.dispatch_arithmetic(Arithmetic::ADD, AddressMode::A); 1 },
            0x80 => { self.dispatch_arithmetic(Arithmetic::ADD, AddressMode::B); 1 },
            0x81 => { self.dispatch_arithmetic(Arithmetic::ADD, AddressMode::C); 1 },
            0x82 => { self.dispatch_arithmetic(Arithmetic::ADD, AddressMode::D); 1 },
            0x83 => { self.dispatch_arithmetic(Arithmetic::ADD, AddressMode::E); 1 },
            0x84 => { self.dispatch_arithmetic(Arithmetic::ADD, AddressMode::F); 1 },
            0x85 => { self.dispatch_arithmetic(Arithmetic::ADD, AddressMode::L); 1 },
            0x86 => { self.dispatch_arithmetic(Arithmetic::ADD, AddressMode::HL); 1 },
            0xC6 => {
                let imm_addr = AddressMode::Imm(memory.read(self.registers.pc + 1));
                self.dispatch_arithmetic(Arithmetic::ADD, imm_addr); 2
            },

            // Add with Carry
            0x8F => { self.dispatch_arithmetic(Arithmetic::ADC, AddressMode::A); 1 },
            0x88 => { self.dispatch_arithmetic(Arithmetic::ADC, AddressMode::B); 1 },
            0x89 => { self.dispatch_arithmetic(Arithmetic::ADC, AddressMode::C); 1 },
            0x8A => { self.dispatch_arithmetic(Arithmetic::ADC, AddressMode::D); 1 },
            0x8B => { self.dispatch_arithmetic(Arithmetic::ADC, AddressMode::E); 1 },
            0x8C => { self.dispatch_arithmetic(Arithmetic::ADC, AddressMode::F); 1 },
            0x8D => { self.dispatch_arithmetic(Arithmetic::ADC, AddressMode::L); 1 },
            0x8E => { self.dispatch_arithmetic(Arithmetic::ADC, AddressMode::HL); 1 },
            0xCE => {
                let imm_addr = AddressMode::Imm(memory.read(self.registers.pc + 1));
                self.dispatch_arithmetic(Arithmetic::ADC, imm_addr); 2
            },

            // Subtract
            0x97 => { self.dispatch_arithmetic(Arithmetic::SUB, AddressMode::A); 1 },
            0x90 => { self.dispatch_arithmetic(Arithmetic::SUB, AddressMode::B); 1 },
            0x91 => { self.dispatch_arithmetic(Arithmetic::SUB, AddressMode::C); 1 },
            0x92 => { self.dispatch_arithmetic(Arithmetic::SUB, AddressMode::D); 1 },
            0x93 => { self.dispatch_arithmetic(Arithmetic::SUB, AddressMode::E); 1 },
            0x94 => { self.dispatch_arithmetic(Arithmetic::SUB, AddressMode::F); 1 },
            0x95 => { self.dispatch_arithmetic(Arithmetic::SUB, AddressMode::L); 1 },
            0x96 => { self.dispatch_arithmetic(Arithmetic::SUB, AddressMode::HL); 1 },
            0xD6 => {
                let imm_addr = AddressMode::Imm(memory.read(self.registers.pc + 1));
                self.dispatch_arithmetic(Arithmetic::SUB, imm_addr); 2
            },

            // Subtract with Carry
            0x9F => { self.dispatch_arithmetic(Arithmetic::SBC, AddressMode::A); 1 },
            0x98 => { self.dispatch_arithmetic(Arithmetic::SBC, AddressMode::B); 1 },
            0x99 => { self.dispatch_arithmetic(Arithmetic::SBC, AddressMode::C); 1 },
            0x9A => { self.dispatch_arithmetic(Arithmetic::SBC, AddressMode::D); 1 },
            0x9B => { self.dispatch_arithmetic(Arithmetic::SBC, AddressMode::E); 1 },
            0x9C => { self.dispatch_arithmetic(Arithmetic::SBC, AddressMode::F); 1 },
            0x9D => { self.dispatch_arithmetic(Arithmetic::SBC, AddressMode::L); 1 },
            0x9E => { self.dispatch_arithmetic(Arithmetic::SBC, AddressMode::HL); 1 },
            0xDE => {
                let imm_addr = AddressMode::Imm(memory.read(self.registers.pc + 1));
                self.dispatch_arithmetic(Arithmetic::SBC, imm_addr); 2
            },

            // AND
            0xA7 => { self.dispatch_arithmetic(Arithmetic::AND, AddressMode::A); 1 },
            0xA0 => { self.dispatch_arithmetic(Arithmetic::AND, AddressMode::B); 1 },
            0xA1 => { self.dispatch_arithmetic(Arithmetic::AND, AddressMode::C); 1 },
            0xA2 => { self.dispatch_arithmetic(Arithmetic::AND, AddressMode::D); 1 },
            0xA3 => { self.dispatch_arithmetic(Arithmetic::AND, AddressMode::E); 1 },
            0xA4 => { self.dispatch_arithmetic(Arithmetic::AND, AddressMode::F); 1 },
            0xA5 => { self.dispatch_arithmetic(Arithmetic::AND, AddressMode::L); 1 },
            0xA6 => { self.dispatch_arithmetic(Arithmetic::AND, AddressMode::HL); 1 },
            0xE6 => {
                let imm_addr = AddressMode::Imm(memory.read(self.registers.pc + 1));
                self.dispatch_arithmetic(Arithmetic::AND, imm_addr); 2
            },

            // XOR
            0xAF => { self.dispatch_arithmetic(Arithmetic::XOR, AddressMode::A); 1 },
            0xA8 => { self.dispatch_arithmetic(Arithmetic::XOR, AddressMode::B); 1 },
            0xA9 => { self.dispatch_arithmetic(Arithmetic::XOR, AddressMode::C); 1 },
            0xAA => { self.dispatch_arithmetic(Arithmetic::XOR, AddressMode::D); 1 },
            0xAB => { self.dispatch_arithmetic(Arithmetic::XOR, AddressMode::E); 1 },
            0xAC => { self.dispatch_arithmetic(Arithmetic::XOR, AddressMode::F); 1 },
            0xAD => { self.dispatch_arithmetic(Arithmetic::XOR, AddressMode::L); 1 },
            0xAE => { self.dispatch_arithmetic(Arithmetic::XOR, AddressMode::HL); 1 },
            0xEE => {
                let imm_addr = AddressMode::Imm(memory.read(self.registers.pc + 1));
                self.dispatch_arithmetic(Arithmetic::XOR, imm_addr); 2
            },

            // OR
            0xB7 => { self.dispatch_arithmetic(Arithmetic::OR, AddressMode::A); 1 },
            0xB0 => { self.dispatch_arithmetic(Arithmetic::OR, AddressMode::B); 1 },
            0xB1 => { self.dispatch_arithmetic(Arithmetic::OR, AddressMode::C); 1 },
            0xB2 => { self.dispatch_arithmetic(Arithmetic::OR, AddressMode::D); 1 },
            0xB3 => { self.dispatch_arithmetic(Arithmetic::OR, AddressMode::E); 1 },
            0xB4 => { self.dispatch_arithmetic(Arithmetic::OR, AddressMode::F); 1 },
            0xB5 => { self.dispatch_arithmetic(Arithmetic::OR, AddressMode::L); 1 },
            0xB6 => { self.dispatch_arithmetic(Arithmetic::OR, AddressMode::HL); 1 },
            0xF6 => {
                let imm_addr = AddressMode::Imm(memory.read(self.registers.pc + 1));
                self.dispatch_arithmetic(Arithmetic::OR, imm_addr); 2
            },

            // Compare
            0xBF => { self.dispatch_arithmetic(Arithmetic::CP, AddressMode::A); 1 },
            0xB8 => { self.dispatch_arithmetic(Arithmetic::CP, AddressMode::B); 1 },
            0xB9 => { self.dispatch_arithmetic(Arithmetic::CP, AddressMode::C); 1 },
            0xBA => { self.dispatch_arithmetic(Arithmetic::CP, AddressMode::D); 1 },
            0xBB => { self.dispatch_arithmetic(Arithmetic::CP, AddressMode::E); 1 },
            0xBC => { self.dispatch_arithmetic(Arithmetic::CP, AddressMode::F); 1 },
            0xBD => { self.dispatch_arithmetic(Arithmetic::CP, AddressMode::L); 1 },
            0xBE => { self.dispatch_arithmetic(Arithmetic::CP, AddressMode::HL); 1 },
            0xFE => {
                let imm_addr = AddressMode::Imm(memory.read(self.registers.pc + 1));
                self.dispatch_arithmetic(Arithmetic::CP, imm_addr); 2
            },

            // Increment
            0x3C => { self.dispatch_arithmetic(Arithmetic::INC, AddressMode::A); 1 },
            0x04 => { self.dispatch_arithmetic(Arithmetic::INC, AddressMode::B); 1 },
            0x0C => { self.dispatch_arithmetic(Arithmetic::INC, AddressMode::C); 1 },
            0x14 => { self.dispatch_arithmetic(Arithmetic::INC, AddressMode::D); 1 },
            0x1C => { self.dispatch_arithmetic(Arithmetic::INC, AddressMode::E); 1 },
            0x24 => { self.dispatch_arithmetic(Arithmetic::INC, AddressMode::F); 1 },
            0x2C => { self.dispatch_arithmetic(Arithmetic::INC, AddressMode::L); 1 },
            0x34 => { self.dispatch_arithmetic(Arithmetic::INC, AddressMode::HL); 1 },

            // Decrement
            0x3D => { self.dispatch_arithmetic(Arithmetic::DEC, AddressMode::A); 1 },
            0x05 => { self.dispatch_arithmetic(Arithmetic::DEC, AddressMode::B); 1 },
            0x0D => { self.dispatch_arithmetic(Arithmetic::DEC, AddressMode::C); 1 },
            0x15 => { self.dispatch_arithmetic(Arithmetic::DEC, AddressMode::D); 1 },
            0x1D => { self.dispatch_arithmetic(Arithmetic::DEC, AddressMode::E); 1 },
            0x25 => { self.dispatch_arithmetic(Arithmetic::DEC, AddressMode::F); 1 },
            0x2D => { self.dispatch_arithmetic(Arithmetic::DEC, AddressMode::L); 1 },
            0x35 => { self.dispatch_arithmetic(Arithmetic::DEC, AddressMode::HL); 1 },


            // --------------------

            // 16-bit Arithmetic
            // --------------------


            // TODO

            // --------------------


            // Rotate and Shift
            // --------------------

            // TODO

            // --------------------


            // Bit Manipulation
            // --------------------

            // TODO

            // --------------------


            // Jump, Call and return
            // --------------------

            0xC3 => {
                let ext_addr = memory.read_word(self.registers.pc + 1);
                self.dispatch_jump(ext_addr, Condition::None); 3
            },
            0xD8 => {
                let ext_addr = memory.read_word(self.registers.pc + 1);
                self.dispatch_jump(ext_addr, Condition::Carry); 3
            },
            0xD2 => {
                let ext_addr = memory.read_word(self.registers.pc + 1);
                self.dispatch_jump(ext_addr, Condition::NonCarry); 3
            },
            0xCA => {
                let ext_addr = memory.read_word(self.registers.pc + 1);
                self.dispatch_jump(ext_addr, Condition::Zero); 3
            },
            0xC2 => {
                let ext_addr = memory.read_word(self.registers.pc + 1);
                self.dispatch_jump(ext_addr, Condition::NonZero); 3
            },
            0xEA => {
                let ext_addr = memory.read_word(self.registers.pc + 1);
                self.dispatch_jump(ext_addr, Condition::ParityEven); 3
            },
            0xE2 => {
                let ext_addr = memory.read_word(self.registers.pc + 1);
                self.dispatch_jump(ext_addr, Condition::ParityOdd); 3
            },
            0xFA => {
                let ext_addr = memory.read_word(self.registers.pc + 1);
                self.dispatch_jump(ext_addr, Condition::SignNeg); 3
            },
            0xF2 => {
                let ext_addr = memory.read_word(self.registers.pc + 1);
                self.dispatch_jump(ext_addr, Condition::SignPos); 3
            },

            // --------------------


            // Unknown
            _ => { println!("warning: Unknown opcode 0x{:02x}", opcode_bytes[0]); 1 }
        };

        // Stop execution for the lols
        if self.registers.pc > 256 {
            self.running = false;
            self.dump_state();
        }
    }

    pub fn dump_state(&self) {
        println!("Dumping current CPU state");
        println!("Registers:");
        println!("- AF: 0x{:04x}", self.registers.af);
        println!("- BC: 0x{:04x}", self.registers.bc);
        println!("- DE: 0x{:04x}", self.registers.de);
        println!("- HL: 0x{:04x}", self.registers.hl);
        println!("- SP: 0x{:04x}", self.registers.sp);
        println!("- PC: 0x{:04x}", self.registers.pc);
        println!("Flags:");
        println!("- Zero: {}", get_bit(self.registers.af, 7));
        println!("- Add/Sub: {}", get_bit(self.registers.af, 6));
        println!("- Half Carry: {}", get_bit(self.registers.af, 5));
        println!("- Carry Flag {}", get_bit(self.registers.af, 4));
    }

    // Dispatched instruction handlers
    pub fn dispatch_load(&self, src: AddressMode, dest: AddressMode) {
        println!("status: Load - src: {:?} dst: {:?}", src, dest);
    }

    pub fn dispatch_arithmetic(&self, op: Arithmetic, src: AddressMode) {
        println!("status: {:?} - src: {:?}", op, src);
    }

    pub fn dispatch_jump(&self, address: u16, cond: Condition) {
        println!("status: Jump - address: 0x{:04x} cond: {:?}", address, cond);
    }
}
