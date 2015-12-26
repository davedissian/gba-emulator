use std::rc::Rc;
use std::cell::RefCell;

use cpu::Cpu;
use memory::Memory;
use cartridge::Cartridge;

pub struct Machine {
    cpu: Cpu,
    memory: Rc<RefCell<Memory>>,
}

impl Machine {
    pub fn new() -> Machine {
        let m = Rc::new(RefCell::new(Memory::new()));
        Machine {
            cpu: Cpu::new(m.clone()),
            memory: m.clone()
        }
    }

    pub fn load_cartridge(&mut self, filename: &str) {
        match Cartridge::load(filename) {
            Ok(c) => {
                println!("status: Loaded ROM '{}'", c.title);
                self.memory.borrow_mut().cartridge = Some(c);
            }
            Err(e) => panic!("error: Unable to load cartridge. Reason: '{}'", e)
        }
    }

    pub fn tick(&mut self) {
        loop {
            self.cpu.tick();
            if !self.cpu.running {
                break;
            }
        }
    }
}