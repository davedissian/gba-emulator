pub struct Memory {
    vram: [u8; 8192],
    bank: [u8; 8192],
    internal: [u8; 8192],
    oam: [u8; 160],
    ier: u8
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            vram: [0u8; 8192],
            bank: [0u8; 8192],
            internal: [0u8; 8192],
            oam: [0u8; 160],
            ier: 0
        }
    }

    // Memory Writing
    pub fn write(&mut self, addr: u16, value: u8) {
        match addr {
            0x8000 ... 0x9FFF => self.vram[addr as usize - 0x8000] = value,
            0xA000 ... 0xBFFF => self.bank[addr as usize - 0xA000] = value,
            0xC000 ... 0xDFFF => self.internal[addr as usize - 0xC000] = value,
            0xE000 ... 0xFDFF => self.internal[addr as usize - 0xE000] = value,
            0xFE00 ... 0xFE9F => self.oam[addr as usize - 0xFE00] = value,
            0xFFFF            => self.ier = value,
            _                 => panic!("ERROR: Out of bounds memory write")
        }
    }

    pub fn write_word(&mut self, addr: u16, word: u16) {
        self.write(addr + 0, ((word >> 0) & 0xFF) as u8);
        self.write(addr + 1, ((word >> 8) & 0xFF) as u8);
    }

    pub fn write_dword(&mut self, addr: u16, dword: u32) {
        self.write(addr + 0, ((dword >>  0) & 0xFF) as u8);
        self.write(addr + 1, ((dword >>  8) & 0xFF) as u8);
        self.write(addr + 2, ((dword >> 16) & 0xFF) as u8);
        self.write(addr + 3, ((dword >> 24) & 0xFF) as u8);
    }

    // Memory Reading
    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0x8000 ... 0x9FFF => self.vram[addr as usize - 0x8000],
            0xA000 ... 0xBFFF => self.bank[addr as usize - 0xA000],
            0xC000 ... 0xDFFF => self.internal[addr as usize - 0xC000],
            0xE000 ... 0xFDFF => self.internal[addr as usize - 0xE000],
            0xFE00 ... 0xFE9F => self.oam[addr as usize - 0xFE00],
            0xFFFF            => self.ier,
            _                 => panic!("ERROR: Out of bounds memory read")
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        return
            ((self.read(addr + 0) as u16) << 0) +
            ((self.read(addr + 1) as u16) << 8)
    }

    pub fn read_dword(&self, addr: u16) -> u32 {
        return
            ((self.read(addr + 0) as u32) <<  0) +
            ((self.read(addr + 1) as u32) <<  8) +
            ((self.read(addr + 2) as u32) << 16) +
            ((self.read(addr + 3) as u32) << 24)
    }
}