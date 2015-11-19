pub struct CPU {
    dummy: u8
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            dummy: 0
        }
    }

    pub fn tick(&self) {
        println!("tick!");
    }
}