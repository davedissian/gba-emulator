use cartridge::MemoryBankController;

pub struct ROM {
    rom: [u8; 0x8000]
}

fn copy_rom_bytes(src: &[u8], dest: &mut [u8; 0x8000]) {
    for i in 0..N-1 {
        dest[i] = src[i];
    }
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
    pub fn new(data: &[u8]) -> ROM {
        let mut rom = ROM {
            rom: [0; 0x8000]
        };
        copy_rom_bytes(data, rom.rom);
    }
}
