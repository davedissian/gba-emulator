mod gbc;

fn main() {
    let mut device = gbc::GBC::new();
    device.tick();
}