/*
Interrupt Enable Register
--------------------------- FFFF
Internal RAM
--------------------------- FF80
Empty but unusable for I/O
--------------------------- FF4C
I/O ports
--------------------------- FF00
Empty but unusable for I/O
--------------------------- FEA0
Sprite Attrib Memory (OAM)
--------------------------- FE00
Echo of 8kB Internal RAM
--------------------------- E000
8kB Internal RAM
--------------------------- C000
8kB switchable RAM bank
--------------------------- A000
8kB Video RAM
--------------------------- 8000
32kB Cartridge
--------------------------- 0000
*/
struct Memory {
    vram: [u8; 8192],
    bank: [u8; 8192],
    internal: [u8; 8192],
    oam: [u8; 160],
    ier: u8,
    dummy: u8
}

impl Memory {
    fn new() -> Memory {
        Memory {
            vram: [0u8; 8192],
            bank: [0u8; 8192],
            internal: [0u8; 8192],
            oam: [0u8; 160],
            ier: 0,
            dummy: 0
        }
    }

    fn map(&mut self, addr: u16) -> &mut u8 {
        match addr {
            0x8000 ... 0x9FFF => &mut self.vram[addr as usize - 0x8000],
            0xA000 ... 0xBFFF => &mut self.bank[addr as usize - 0xA000],
            0xC000 ... 0xDFFF => &mut self.internal[addr as usize - 0xC000],
            0xE000 ... 0xFDFF => &mut self.internal[addr as usize - 0xE000],
            0xFE00 ... 0xFE9F => &mut self.oam[addr as usize - 0xFE00],
            0xFFFF => &mut self.ier,
            _ => {
                println!("ERROR: Out of bounds memory write");
                &mut self.dummy
            } // TODO: error
        }
    }

    fn write(&mut self, addr: u16, value: u8) {
        *self.map(addr) = value;
    }

    fn write_word(&mut self, addr: u16, word: u16) {
        self.write(addr + 0, ((word >> 0) & 0xFF) as u8);
        self.write(addr + 1, ((word >> 8) & 0xFF) as u8);
    }

    // TODO: Make these not mutable
    fn read(&mut self, addr: u16) -> u8 {
        return *self.map(addr);
    }

    fn read_word(&mut self, addr: u16) -> u16 {
        return
            ((self.read(addr + 0) as u16) << 0) +
            ((self.read(addr + 1) as u16) << 8)
    }
}

fn main() {
    let mut memory = Memory::new();
    memory.write_word(0x8010, 32);
    println!("{}", memory.read_word(0x8010));
}