use std::rc::Rc;

use cpu::CPU;
use memory::Memory;
use cartridge::Cartridge;

pub struct Machine {
    cpu: CPU,
    memory: Memory,
    cartridge: Option<Rc<Cartridge>>,
}

impl Machine {
    pub fn new() -> Machine {
        Machine {
            cpu: CPU::new(),
            memory: Memory::new(),
            cartridge: None,
        }
    }

    pub fn load_cartridge(&mut self, filename: &str) {
        match Cartridge::load(filename) {
            Ok(c) => {
                println!("status: Loaded ROM '{}'", c.title);
                let rc_ptr = Rc::new(c);
                self.memory.cartridge = Some(Rc::downgrade(&rc_ptr));
                self.cartridge = Some(rc_ptr);
            }
            Err(e) => panic!("error: Unable to load cartridge. Reason: '{}'", e),
        }
    }

    pub fn tick(&mut self) {
        loop {
            self.cpu.tick(&mut self.memory);
            if !self.cpu.running {
                break;
            }
        }
    }
}
