// Extract bits between min and max inclusive
// eg: Extract bits 2-4 of 0b0111100 should yield 0b1110000
fn get_bits(u16 number, u16 min, u16 max) -> u16 {
    return (number >> min) & ((max - min) - 1);
}

fn get_bit(u16 number, u16 bit) -> u16 {
    return (number >> min) & 0x1;
}

// High 8 - LHS, Low 8 - RHS
// eg: BC register has B in high 8 and C in low 8
struct Registers {
    af: u16,    // Accumulator and Flags
    bc: u16,    // BC
    de: u16,    // DE
    hl: u16,    // HL
    sp: u16,    // Stack Pointer
    pc: u16     // Program Counter
}

/*
    Flag Register:

    Bit  Name  Set Clr  Expl.
    7    zf    Z   NZ   Zero Flag
    6    n     -   -    Add/Sub-Flag (BCD)
    5    h     -   -    Half Carry Flag (BCD)
    4    cy    C   NC   Carry Flag
    3-0  -     -   -    Not used (always zero)
*/

pub struct CPU {
    registers: Registers
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
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

    pub fn tick(&mut self) {
        // Instruction Layout:
            

        // Fetch an instruction from memory
        let instr = 0x1123;
        self.registers.pc += 4;

        // Decode and execute instruction
        let op_code = //
    }

    pub fn dump_state(&self) {
        println!("Dumping current CPU state");
        println!("Registers:");
        println!("- AF: 0x{:x}", self.registers.af);
        println!("- BC: 0x{:x}", self.registers.bc);
        println!("- DE: 0x{:x}", self.registers.de);
        println!("- HL: 0x{:x}", self.registers.hl);
        println!("- SP: 0x{:x}", self.registers.sp);
        println!("- PC: 0x{:x}", self.registers.pc);
        println!("Flags:");
        println!("- Zero: {}", get_bit(self.af, 7));
        println!("- Add/Sub: {}", get_bit(self.af, 6));
        println!("- Half Carry: {}", get_bit(self.af, 5));
        println!("- Carry Flag {}", get_bit(self.af, 4));
    }
}