mod rom;

use std::fs::File;
use std::io::Read;
use cartridge::rom::ROM;

pub trait MemoryBankController {
    fn read_u8(&self, addr: u16) -> u8;
    fn write_u8(&self, addr: u16, data: u8);
}

pub struct Cartridge {
    pub title: String,
    pub mbc: Box<MemoryBankController>
}

impl Cartridge {
    pub fn new() -> Cartridge {
        Cartridge {
            title: String::new(),
            mbc: Box::new(ROM::new(&[0; 0x8000]))
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

        // Verify cartridge checksum
        // TODO
        
        // Set up MBC
        let cartridge_type = contents[0x0147];
        println!("status: Cartridge Type: {}", cartridge_type);
        let mbc: Box<MemoryBankController> = match cartridge_type {
            0 => Box::new(ROM::new(&contents[0..0x7FFF])),
            _ => panic!("ERROR: unknown cartridge type")
        };

        // Return a new cartridge object
        Ok(Cartridge {
            title: title,
            mbc: mbc
        })
    }

    pub fn read_u8(&self, addr: u16) -> u8 {
        self.mbc.read_u8(addr)
    }

    pub fn write_u8(&self, addr: u16, value: u8) {
        self.mbc.write_u8(addr, value);
    }
}
