use cartridge::MemoryBankController;

pub struct ROM {
    rom: [u8; 0x8000]
}

impl MemoryBankController for ROM {
    fn read_u8(&self, addr: u16) -> u8 {
        self.rom[addr as usize]
    }

    fn write_u8(&self, addr: u16, value: u8) {
        println!("WARNING: Writing to a read-only memory region");
    }
}

impl ROM {
    pub fn new(rom: [u8; 0x8000]) -> ROM {
        ROM {
            rom: rom
        }
    }
}
