#![allow(dead_code)]

mod gbc;

fn main() {
    let mut device = gbc::GBC::new();
    device.load_cartridge("roms/pokemon-gold.gbc");
    device.tick();
}