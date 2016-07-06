use std::rc::Rc;
use std::cell::RefCell;

use cpu::interpreter::Cpu;
use memory::Memory;

pub struct Machine {
    cpu: Cpu,
    memory: Rc<RefCell<Memory>>,
}

impl Machine {
    pub fn new(rom: &str) -> Machine {
        let m = Rc::new(RefCell::new(Memory::new(rom)));
        Machine {
            cpu: Cpu::new(m.clone()),
            memory: m.clone()
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
