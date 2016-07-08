use cartridge::Cartridge;

pub struct Memory {
    pub boot_mode: bool,

    cartridge: Cartridge,

    // Internal RAM structures
    bios: [u8; 0x100],
    vram: [u8; 8192],
    bank: [u8; 8192],
    internal: [u8; 8192],
    oam: [u8; 160],
    zero_page_ram: [u8; 126],

    // Registers
    dmg_status: u8,
    interrupts_enabled: u8,
}

const DMG_STATUS_REG: u16               = 0xFF50;
const CGB_INFRARED_PORT_REG: u16        = 0xFF56;
const CGB_WRAM_BANK_SELECT: u16         = 0xFF70;
const CGB_DOUBLE_SPEED_PREP_REG: u16    = 0xFF4D;
const CGB_HDMA_SOURCE_HIGH_REG: u16     = 0xFF51;
const CGB_HDMA_SOURCE_LOW_REG: u16      = 0xFF52;
const CGB_HDMA_DEST_HIGH_REG: u16       = 0xFF53;
const CGB_HDMA_DEST_LOW_REG: u16        = 0xFF54;
const CGB_HDMA_REG: u16                 = 0xFF55;

impl Memory {
    pub fn new(rom: &str) -> Memory {
        let cartridge = match Cartridge::load(rom) {
            Ok(c) => c,
            Err(e) => panic!("ERROR: {}", e)
        };
        Memory {
            bios: [0u8; 0x100],
            cartridge: cartridge,
            vram: [0u8; 8192],
            bank: [0u8; 8192],
            internal: [0u8; 8192],
            oam: [0u8; 160],
            zero_page_ram: [0u8; 126],
            dmg_status: 0,
            interrupts_enabled: 0,
            boot_mode: true
        }
    }

    pub fn new_blank() -> Memory {
        Memory {
            bios: [0u8; 0x100],
            cartridge: Cartridge::new_blank(),
            vram: [0u8; 8192],
            bank: [0u8; 8192],
            internal: [0u8; 8192],
            oam: [0u8; 160],
            zero_page_ram: [0u8; 126],
            dmg_status: 0,
            interrupts_enabled: 0,
            boot_mode: true
        }
    }

    // Load BIOS
    pub fn load_bios(&mut self, bios: [u8; 0x100]) {
        self.bios = bios;
    }

    pub fn set_boot_mode(&mut self, boot: bool) {
        self.boot_mode = boot;
    }

    // Memory Reading
    pub fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x7FFF => {
                if addr < 0x100 && self.boot_mode {
                    self.bios[addr as usize]
                } else {
                    self.cartridge.rom[addr as usize]
                }
            },
            0x8000...0x9FFF => self.vram[addr as usize - 0x8000],
            0xA000...0xBFFF => self.bank[addr as usize - 0xA000],
            0xC000...0xDFFF => self.internal[addr as usize - 0xC000],
            0xE000...0xFDFF => self.internal[addr as usize - 0xE000],
            0xFE00...0xFE9F => self.oam[addr as usize - 0xFE00],
            0xFF4C...0xFF79 => self.read_u8_register(addr),
            0xFF80...0xFFFE => self.zero_page_ram[addr as usize - 0xFF80],
            0xFFFF => self.interrupts_enabled,
            _ => panic!("ERROR: Out of bounds memory read. Addr = {:x}", addr),
        }
    }

    pub fn read_u16(&self, value: u16) -> u16 {
        return self.read_u8(value) as u16 + ((self.read_u8(value + 1) as u16) << 8);
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
            0xFF4C...0xFF79 => self.write_u8_register(addr, value),
            0xFF80...0xFFFE => self.zero_page_ram[addr as usize - 0xFF80] = value,
            0xFFFF => self.interrupts_enabled = value,
            _ => panic!("ERROR: Out of bounds memory write. Addr = {:x}", addr),
        }
    }

    pub fn write_u16(&mut self, addr: u16, value: u16) {
        self.write_u8(addr + 0, ((value >> 0) & 0xFF) as u8);
        self.write_u8(addr + 1, ((value >> 8) & 0xFF) as u8);
    }


    // Registers
    fn read_u8_register(&self, addr: u16) -> u8{
        match addr {
	        DMG_STATUS_REG              => self.dmg_status, 
            CGB_INFRARED_PORT_REG       => 0,
            CGB_WRAM_BANK_SELECT        => 0,
            CGB_DOUBLE_SPEED_PREP_REG   => 0,
            CGB_HDMA_SOURCE_HIGH_REG    => 0,
            CGB_HDMA_SOURCE_LOW_REG     => 0,
            CGB_HDMA_DEST_HIGH_REG      => 0,
            CGB_HDMA_DEST_LOW_REG       => 0,
            CGB_HDMA_REG                => 0,
            _                           => panic!("Unknown MMU register: {}", addr)
        }
    }
    
    fn write_u8_register(&mut self, addr: u16, value: u8) {
        match addr {
	        DMG_STATUS_REG              => self.dmg_status = value, 
            CGB_INFRARED_PORT_REG       => {},
            CGB_WRAM_BANK_SELECT        => {},
            CGB_DOUBLE_SPEED_PREP_REG   => {},
            CGB_HDMA_SOURCE_HIGH_REG    => {},
            CGB_HDMA_SOURCE_LOW_REG     => {},
            CGB_HDMA_DEST_HIGH_REG      => {},
            CGB_HDMA_DEST_LOW_REG       => {},
            CGB_HDMA_REG                => {},
            _                           => panic!("Unknown MMU register: {}", addr)
        }
    }
}
