use cartridge::MemoryBankController;

pub struct ROM {
    rom: [u8; 0x8000]
}

impl MemoryBankController for ROM {
    fn read_u8(&self, addr: u16) -> u8 {
        self.rom[addr as usize]
    }

    fn write_u8(&mut self, addr: u16, value: u8) {
        println!("WARNING: Writing to a read-only memory region. Addr: {:04X} - Value: {:02X}", addr, value);
    }
}

impl ROM {
    pub fn new(data: &[u8]) -> ROM {
        let mut rom = ROM {
            rom: [0; 0x8000]
        };
        rom.rom.copy_from_slice(data);
        rom
    }
}
