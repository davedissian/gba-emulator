mod cpu;
mod memory;
mod cartridge;

use std::rc::Rc;

pub struct GBC {
    cpu: cpu::CPU,
    memory: memory::Memory,
    cartridge: Option<Rc<cartridge::Cartridge>>
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
                let rc_ptr = Rc::new(c);
                self.memory.cartridge = Some(Rc::downgrade(&rc_ptr));
                self.cartridge = Some(rc_ptr);
            }
            Err(e) => panic!("error: Unable to load cartridge. Reason: '{}'", e)
        }
    }

    pub fn tick(&mut self) {
        while self.cpu.running {
            self.cpu.tick(&mut self.memory);
        }
    }
}