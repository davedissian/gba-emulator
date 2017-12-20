mod rom;
mod mbc1;

use std::fs::File;
use std::io::Read;

use cartridge::rom::ROM;
use cartridge::mbc1::MBC1;

pub trait MemoryBankController {
    fn read_u8(&self, addr: u16) -> u8;
    fn write_u8(&mut self, addr: u16, data: u8);
}

pub struct Cartridge {
    pub title: String,
    pub mbc: Box<MemoryBankController>,
    cgb_enabled: bool
}

impl Cartridge {
    pub fn new() -> Cartridge {
        Cartridge {
            title: String::new(),
            mbc: Box::new(ROM::new(&[0; 0x8000])),
            cgb_enabled: false
        }
    }

    pub fn load(filename: &str) -> Result<Cartridge, String> {
        // Open a cartridge file
        let mut f = match File::open(filename) {
            Ok(f) => f,
            Err(_) => return Err("Unable to open cartridge file".to_string()),
        };

        // Read content from the file
        let mut contents = Vec::new();
        if let Err(_) = f.read_to_end(&mut contents) {
            return Err("Unable to read cartridge file".to_string());
        }

        // Verify the Nintendo Logo bytes
        // TODO

        // Grab the game title from bytes 0134-0143
        let title = String::from_utf8(contents[0x0134..0x0143].to_vec()).unwrap();

        // Calculate ROM size by shifting 32k by the value at 0x148
        let rom_size = (32 * 1024) << contents[0x148];
        println!("status: ROM size is {} KB", rom_size / 1024);
        println!("status: Actual ROM size is {} KB", contents.len() / 1024);

        // Verify cartridge checksum
        // TODO

        // Enable CGB mode.
        let cgb_enabled = contents[0x0143] & 0b1000_0000 != 0;
        println!("status: CGB enabled: {}", cgb_enabled);
        
        // Parse cartridge type.
        let cartridge_type = contents[0x0147];
        println!("status: Cartridge Type: {:x}", cartridge_type);
        let mbc: Box<MemoryBankController> = match cartridge_type {
            0x0 | 0x8 | 0x9  => Box::new(ROM::new(&contents[0..0x8000])),
            0x1 | 0x2 | 0x3 => Box::new(MBC1::new(contents.as_slice())),
            _ => panic!("ERROR: unknown cartridge type")
        };

        // Return a new cartridge object
        Ok(Cartridge {
            title: title,
            mbc: mbc,
            cgb_enabled: cgb_enabled
        })
    }

    pub fn read_u8(&self, addr: u16) -> u8 {
        self.mbc.read_u8(addr)
    }

    pub fn write_u8(&mut self, addr: u16, value: u8) {
        self.mbc.write_u8(addr, value);
    }

    pub fn is_cgb_enabled(&self) -> bool {
        self.cgb_enabled
    }
}
