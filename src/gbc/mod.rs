mod cpu;
mod memory;

pub struct GBC {
    cpu: cpu::CPU,
    memory: memory::Memory,
}

impl GBC {
    pub fn new() -> GBC {
        GBC {
            cpu: cpu::CPU::new(),
            memory: memory::Memory::new()
        }
    }

    pub fn tick(&mut self) {
        self.memory.write_dword(0x8000, 32);
        println!("{}", self.memory.read_dword(0x8000));
        self.cpu.tick()
    }
}