use cartridge::Cartridge;
use gpu::Gpu;
use std::ops::Range;

pub struct Memory {
    pub boot_mode: bool,

    // Hardware
    cartridge: Option<Cartridge>,
    gpu: Gpu,

    // Internal RAM structures
    bios: [u8; 0x100],
    //vram: [u8; 8192],
    bank: [u8; 8192],
    internal: [u8; 8192],
    //oam: [u8; 160],
    zero_page_ram: [u8; 127],

    // Registers
    dmg_status: u8,
    interrupts_enabled: u8,

    // Gameboy Colour specifics
    cgb_enabled: bool,
    cgb_wram_bank_select: u8,
    cgb_double_speed_prep: u8,
    cgb_hdma_src_high: u8,
    cgb_hdma_src_low: u8,
    cgb_hdma_dest_high: u8,
    cgb_hdma_dest_low: u8
}

// Registers
pub const DMG_STATUS_REG: u16               = 0xFF50;
pub const CGB_INFRARED_PORT_REG: u16        = 0xFF56;
pub const CGB_WRAM_BANK_SELECT: u16         = 0xFF70;
pub const CGB_DOUBLE_SPEED_PREP_REG: u16    = 0xFF4D;
pub const CGB_HDMA_SOURCE_HIGH_REG: u16     = 0xFF51;
pub const CGB_HDMA_SOURCE_LOW_REG: u16      = 0xFF52;
pub const CGB_HDMA_DEST_HIGH_REG: u16       = 0xFF53;
pub const CGB_HDMA_DEST_LOW_REG: u16        = 0xFF54;
pub const CGB_HDMA_REG: u16                 = 0xFF55;
pub const INTERRUPTS_ENABLED_REG: u16       = 0xFFFF;

// Interrupt Enable masks
pub const INTERRUPT_ENABLE_VBLANK: u8       = 0b00000001;
pub const INTERRUPT_ENABLE_LCDC: u8         = 0b00000010;
pub const INTERRUPT_ENABLE_TIMER: u8        = 0b00000100;
pub const INTERRUPT_ENABLE_SERIAL_IO: u8    = 0b00001000;

impl Memory {
    pub fn new() -> Memory {
        Memory {
            boot_mode: true,
            bios: [0u8; 0x100],

            cartridge: None,
            gpu: Gpu::new(),

            bank: [0u8; 8192],
            internal: [0u8; 8192],
            zero_page_ram: [0u8; 127],

            dmg_status: 0,
            interrupts_enabled: 0,

            cgb_enabled: false,
            cgb_wram_bank_select: 0,
            cgb_double_speed_prep: 0,
            cgb_hdma_src_low: 0,
            cgb_hdma_src_high: 0,
            cgb_hdma_dest_low: 0,
            cgb_hdma_dest_high: 0
        }
    }

    // Load cartridge
    pub fn load_cartridge(&mut self, cartridge: Cartridge) {
        self.cgb_enabled = cartridge.is_cgb_enabled();
        self.cartridge = Some(cartridge);
    }

    // Load BIOS
    pub fn load_bios(&mut self, bios: [u8; 0x100]) {
        self.bios = bios;
    }

    pub fn set_boot_mode(&mut self, boot: bool) {
        self.boot_mode = boot;
    }

    // Tick
    pub fn tick(&mut self, cycles: u32) -> bool {
        self.gpu.tick(cycles)
    }

    // Memory Reading
    pub fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x7FFF => {
                if addr < 0x100 && self.boot_mode {
                    self.bios[addr as usize]
                } else {
                    if let Some(ref c) = self.cartridge {
                        c.read_u8(addr)
                    } else {
                        panic!("ERROR: No cartridge is loaded!");
                    }
                }
            },
            0x8000...0x9FFF => self.gpu.read_u8(addr),
            0xA000...0xBFFF => {
                if let Some(ref c) = self.cartridge {
                    c.read_u8(addr)
                } else {
                    panic!("ERROR: No cartridge is loaded!");
                }
            },
            0xC000...0xDFFF => self.internal[addr as usize - 0xC000],
            0xE000...0xFDFF => self.internal[addr as usize - 0xE000],
            0xFE00...0xFE9F => self.gpu.read_u8(addr),
            0xFEA0...0xFEFF => { println!("WARNING: Reading from unused memory area. Addr = 0x{:X}", addr); 0 },
            // TODO: 0xFF00..0xFF7F => IO PORTS IN CUSTOM IO MODULE (which should contain registers).
            0xFF00...0xFF7F => self.read_u8_io(addr),
            0xFF80...0xFFFE => self.zero_page_ram[addr as usize - 0xFF80],
            0xFFFF => self.interrupts_enabled,
            _ => panic!("ERROR: Out of bounds memory read. Addr = 0x{:X}", addr),
        }
    }

    pub fn read_u16(&self, value: u16) -> u16 {
        return self.read_u8(value) as u16 + ((self.read_u8(value + 1) as u16) << 8);
    }

    // Memory Writing
    pub fn write_u8(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000...0x7FFF => {
                if let Some(ref mut c) = self.cartridge {
                    c.write_u8(addr, value);
                } else {
                    panic!("ERROR: No cartridge is loaded!");
                }
            },
            0x8000...0x9FFF => self.gpu.write_u8(addr, value),
            0xA000...0xBFFF =>  {
                if let Some(ref mut c) = self.cartridge {
                    c.write_u8(addr, value);
                } else {
                    panic!("ERROR: No cartridge is loaded!");
                }
            },
            0xC000...0xDFFF => self.internal[addr as usize - 0xC000] = value,
            0xE000...0xFDFF => self.internal[addr as usize - 0xE000] = value,
            0xFE00...0xFE9F => self.gpu.write_u8(addr, value),
            0xFEA0...0xFEFF => println!("WARNING: Writing to unused memory area. Addr = 0x{:X}", addr),
            0xFF00...0xFF7F => self.write_u8_io(addr, value),
            0xFF80...0xFFFE => self.zero_page_ram[addr as usize - 0xFF80] = value,
            0xFFFF => self.interrupts_enabled = value,
            _ => panic!("ERROR: Out of bounds memory write. Addr = 0x{:X}", addr),
        }
    }

    pub fn write_u16(&mut self, addr: u16, value: u16) {
        self.write_u8(addr + 0, ((value >> 0) & 0xFF) as u8);
        self.write_u8(addr + 1, ((value >> 8) & 0xFF) as u8);
    }

    // Registers
    fn read_u8_io(&self, addr: u16) -> u8{
        match addr {
            0xFF40...0xFF4F => self.gpu.read_u8(addr),
            DMG_STATUS_REG => self.dmg_status,
            _ => if self.cgb_enabled {
                match addr {
                    CGB_INFRARED_PORT_REG => {
                        println!("WARNING: CGB Infrared Unsupported");
                        0
                    },
                    CGB_WRAM_BANK_SELECT => self.cgb_wram_bank_select,
                    CGB_DOUBLE_SPEED_PREP_REG => self.cgb_double_speed_prep,
                    CGB_HDMA_SOURCE_HIGH_REG => self.cgb_hdma_src_high,
                    CGB_HDMA_SOURCE_LOW_REG => self.cgb_hdma_src_low,
                    CGB_HDMA_DEST_HIGH_REG => self.cgb_hdma_dest_high,
                    CGB_HDMA_DEST_LOW_REG => self.cgb_hdma_dest_low,
                    CGB_HDMA_REG => {
                        println!("WARNING: HDMA unsupported");
                        0
                    },
                    _ => {
                        println!("WARNING: Reading from unknown MMU register: 0x{:X}", addr);
                        0
                    }
                }
            } else {
                println!("WARNING: Cannot read from 0x{:08X} (CGB register) in non-CGB mode!", addr);
                0
            }
        }
    }
    
    fn write_u8_io(&mut self, addr: u16, value: u8) {
        match addr {
            0xFF40...0xFF4F => self.gpu.write_u8(addr, value),
            DMG_STATUS_REG => self.dmg_status = value,
            _ => if self.cgb_enabled {
                match addr {
                    CGB_INFRARED_PORT_REG => println!("WARNING: CGB Infrared Unsupported"),
                    CGB_WRAM_BANK_SELECT => self.cgb_wram_bank_select = value,
                    CGB_DOUBLE_SPEED_PREP_REG => self.cgb_double_speed_prep = value,
                    CGB_HDMA_SOURCE_HIGH_REG => self.cgb_hdma_src_high = value,
                    CGB_HDMA_SOURCE_LOW_REG => self.cgb_hdma_src_low = value,
                    CGB_HDMA_DEST_HIGH_REG => self.cgb_hdma_dest_high = value,
                    CGB_HDMA_DEST_LOW_REG => self.cgb_hdma_dest_low = value,
                    CGB_HDMA_REG => println!("WARNING: HDMA unsupported"),
                    _ => println!("WARNING: Writing to unknown MMU register: 0x{:X}", addr)
                }
            } else {
                println!("WARNING: Cannot write to 0x{:X} (CGB register) in non-CGB mode!", addr);
            }
        }
    }

    // Debugging.
    pub fn dump_state(&self, addr_range: Range<u16>) -> String {
        let mut out_str = String::new();
        let mut counter = 0;
        let line_length = 16;
        let mut addr_line = String::new();
        for addr in addr_range {
            addr_line.push_str(&format!("{:02x} ", self.read_u8(addr)));
            counter += 1;
            if counter >= line_length {
                counter = 0;
                out_str.push_str(&format!("0x{:04x}-0x{:04x} | {}\n", addr - line_length + 1, addr, addr_line.as_str()));
                addr_line = String::new();
            }
        }
        return out_str;
    }
}
