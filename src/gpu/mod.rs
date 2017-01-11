extern crate softrender;

use self::softrender::driver;

pub use self::softrender::driver::Colour;

struct Display {
    driver: Box<driver::Driver>,
    window: Box<driver::Window>,
    scale: usize
}

impl Display {
    fn new(scale: usize) -> Display {
        let gbc_screen_size = (160, 144);
        let driver = driver::create(driver::DriverType::GL);
        Display {
            window: driver.create_window(gbc_screen_size.0 * scale, gbc_screen_size.1 * scale, "GBC Emulator"),
            driver: driver,
            scale: scale
        }
    }

    fn put_pixel(&mut self, x: usize, y: usize, colour: &Colour) {
        // Naive implementation. Upstream should instead have a way of specifying the
        // "backbuffer" size in addition to the window size.
        for y_offset in 0..self.scale {
            for x_offset in 0..self.scale {
                self.window.draw_pixel(x + x_offset, y + y_offset, colour);
            }
        }
    }

    fn update(&mut self) -> bool {
        self.window.update()
    }
}

pub struct Gpu {
    display: Display
}

impl Gpu {
    pub fn new() -> Gpu {
        Gpu {
            display: Display::new(4)
        }
    }

    pub fn tick(&mut self) -> bool {
        self.display.update()
    }
}
