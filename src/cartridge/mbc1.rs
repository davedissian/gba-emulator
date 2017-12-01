use cartridge::MemoryBankController;

pub struct MBC1 {
    rom: Vec<u8>,
    rom_bank: u8,
    enable_ram: bool,
    mode: u8,
    ram: [u8; 0x8000], // 4 x 0x2000 RAM banks
    ram_bank: u8
}

impl MemoryBankController for MBC1 {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x3FFF => self.rom[addr as usize],
            0x4000...0x7FFF => self.rom[self.rom_bank_addr(addr - 0x4000, self.rom_bank)],
            0xA000...0xBFFF => self.ram[self.ram_bank_addr(addr - 0xA000, self.ram_bank)],
            _ => panic!("ERROR: Attempting to read from invalid address: 0x{:X}", addr)
        }
    }

    fn write_u8(&mut self, addr: u16, value: u8) {
        match addr {
            // RAM Enable.
            0x0000...0x1FFF => {
                // Lower 4 bits == 0xA to enable, any other value to disable.
                self.enable_ram = value & 0b1111 == 0xA;
            },
            // ROM bank.
            0x2000...0x3FFF => {
                // Sets lower 5 bits of ROM bank.
                self.rom_bank = (self.rom_bank & 0b1110_0000) | (value & 0b0001_1111);
                self.rom_bank = match self.rom_bank {
                    0x0 => 0x1,
                    0x20 => 0x21,
                    0x40 => 0x41,
                    0x60 => 0x61,
                    _ => self.rom_bank
                };
                println!("MBC1: Set ROM bank to 0x{:X}", self.rom_bank);
            },
            // RAM bank or upper bits of ROM bank (depending on mode).
            0x4000...0x5FFF => {
                let masked_value = value & 0b11;
                if self.mode == 0 {
                    self.ram_bank = masked_value;
                    println!("MBC1: Set RAM bank to 0x{:X}", self.ram_bank);
                } else {
                    self.rom_bank = (self.rom_bank & 0b0001_1111) | (masked_value << 5);
                    self.rom_bank = match self.rom_bank {
                        0x20 => 0x21,
                        0x40 => 0x41,
                        0x60 => 0x61,
                        _ => self.rom_bank
                    };
                    println!("MBC1: Set ROM bank to 0x{:X}", self.rom_bank);
                }
            }
            // ROM/RAM mode select.
            0x6000...0x7FFF => {
                self.mode = value & 0b1;
                println!("MBC1: Set mode to {}", self.mode);
            }
            // RAM.
            0xA000...0xBFFF => {
                self.ram[self.ram_bank_addr(addr - 0xA000, self.ram_bank)] = value;
            },
            _ => panic!("ERROR: Attempting to write to invalid address: 0x{:X}", addr)
        }
    }
}

impl MBC1 {
    pub fn new(data: &[u8]) -> MBC1 {
        MBC1 {
            rom: data.to_vec(),
            rom_bank: 1,
            enable_ram: false,
            mode: 0,
            ram: [0; 0x8000],
            ram_bank: 0
        }
    }

    #[inline]
    fn rom_bank_addr(&self, rel_addr: u16, bank: u8) -> usize {
        // bank * 0x4000 (rom bank size).
        (bank as usize) * 0x2000 + (rel_addr as usize)
    }

    #[inline]
    fn ram_bank_addr(&self, rel_addr: u16, bank: u8) -> usize {
        // bank * 0x2000 (ram bank size).
        (bank as usize) * 0x2000 + (rel_addr as usize)
    }
}