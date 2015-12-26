use cartridge::Cartridge;

pub struct Memory {
    // A optional weak reference to a cartridge
    // This needs to be public to allow the GBC device to set this to point to the cartridge
    pub cartridge: Option<Cartridge>,

    // Internal RAM structures
    vram: [u8; 8192],
    bank: [u8; 8192],
    internal: [u8; 8192],
    oam: [u8; 160],
    ier: u8,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            cartridge: None,
            vram: [0u8; 8192],
            bank: [0u8; 8192],
            internal: [0u8; 8192],
            oam: [0u8; 160],
            ier: 0,
        }
    }

    // Memory Writing
    pub fn write_u8(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000...0x7FFF => panic!("ERROR: Cannot write to a cartridge!"),
            0x8000...0x9FFF => self.vram[addr as usize - 0x8000] = value,
            0xA000...0xBFFF => self.bank[addr as usize - 0xA000] = value,
            0xC000...0xDFFF => self.internal[addr as usize - 0xC000] = value,
            0xE000...0xFDFF => self.internal[addr as usize - 0xE000] = value,
            0xFE00...0xFE9F => self.oam[addr as usize - 0xFE00] = value,
            0xFFFF => self.ier = value,
            _ => panic!("ERROR: Out of bounds memory write"),
        }
    }

    pub fn write_u16(&mut self, addr: u16, value: u16) {
        self.write_u8(addr + 0, ((value >> 0) & 0xFF) as u8);
        self.write_u8(addr + 1, ((value >> 8) & 0xFF) as u8);
    }

    // Memory Reading
    pub fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x7FFF => {
                match self.cartridge {
                    Some(ref c) => c.rom[addr as usize],
                    None => panic!("ERROR: No cartridge is loaded yet"),
                }
            }
            0x8000...0x9FFF => self.vram[addr as usize - 0x8000],
            0xA000...0xBFFF => self.bank[addr as usize - 0xA000],
            0xC000...0xDFFF => self.internal[addr as usize - 0xC000],
            0xE000...0xFDFF => self.internal[addr as usize - 0xE000],
            0xFE00...0xFE9F => self.oam[addr as usize - 0xFE00],
            0xFFFF => self.ier,
            _ => panic!("ERROR: Out of bounds memory read"),
        }
    }

    pub fn read_u16(&self, value: u16) -> u16 {
        return self.read_u8(value) as u16 + ((self.read_u8(value + 1) as u16) << 8);
    }
}
