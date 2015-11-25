mod cpu;
mod memory;
mod cartridge;

pub struct GBC {
    cpu: cpu::CPU,
    memory: memory::Memory,
    cartridge: Option<Box<cartridge::Cartridge>>
}

impl GBC {
    pub fn new() -> GBC {
        GBC {
            cpu: cpu::CPU::new(),
            memory: memory::Memory::new(),
            cartridge: None
        }
    }

    pub fn load_cartridge(&mut self, filename: &str) {
        match cartridge::Cartridge::load(filename) {
            Ok(c) => {
                println!("status: Loaded ROM '{}'", c.title);
                self.cartridge = Some(Box::new(c));
            }
            Err(e) => panic!("error: Unable to load cartridge. Reason: '{}'", e)
        }
    }

    pub fn tick(&mut self) {
        match self.cartridge {
            Some(ref c) => println!("{:x}{:x}{:x}{:x}", c.rom[0], c.rom[1], c.rom[2], c.rom[3]),
            _ => {}
        }
    }
}