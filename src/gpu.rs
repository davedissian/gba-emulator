extern crate rasteriser;

use self::rasteriser::driver;

pub use self::rasteriser::driver::{Colour, KeyCode, KeyState};

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
                self.window.draw_pixel(x * self.scale + x_offset, y * self.scale + y_offset, colour);
            }
        }
    }

    fn update(&mut self) -> bool {
        self.window.update(false)
    }
}

pub struct Gpu {
    display: Display,
    modeclock: u32,
    mode: u8, // see http://gbdev.gg8.se/files/docs/mirrors/pandocs.html#lcdstatusregister
    line: u8,

    vram: [u8; 8192],
    oam: [u8; 160],

    switchbg: u8,
    bgmap: u8,
    bgtile: u8,
    switchlcd: u8,

    // Registers.
    r_scx: u8, // Screen X
    r_scy: u8, // Screen Y
    r_bgp: u8, // BG palette register
}

impl Gpu {
    pub fn new() -> Gpu {
        Gpu {
            display: Display::new(2),
            modeclock: 0,
            mode: 0,
            line: 0,
            vram: [0u8; 8192],
            oam: [0u8; 160],
            switchbg: 0,
            bgmap: 0,
            bgtile: 0,
            switchlcd: 0,
            r_scx: 0,
            r_scy: 0,
            r_bgp: 0,
        }
    }

    pub fn set_key_callback(&mut self, callback: Box<FnMut(KeyCode, KeyState)>) {
        self.display.window.set_input_callback(callback);
    }

    pub fn tick(&mut self, cycles: u32) -> bool {
        self.modeclock = self.modeclock.wrapping_add(cycles);
        match self.mode {
            // OAM read mode.
            2 => if self.modeclock >= 80 { self.modeclock = 0; self.mode = 3; },
            // VRAM read mode, scanline active. Treat end of mode 3 as end of scanline.
            3 => if self.modeclock >= 172 {
                self.modeclock = 0;
                self.mode = 0;
                self.render_scanline();
            },
            // Hblank. After the last Hblank, update the screen.
            0 => if self.modeclock >= 204 {
                self.modeclock = 0;
                self.line += 1;
                if self.line == 143 {
                    self.mode = 1;
                    if !self.display_image() {
                        return false;
                    }
                } else {
                    self.mode = 2;
                }
            },
            // Vblank (10 lines).
            1 => if self.modeclock >= 456 {
                self.modeclock = 0;
                self.line += 1;
                if self.line > 153 {
                    // Restart scanning modes.
                    self.mode = 2;
                    self.line = 0;
                }
            },
            _ => panic!("ERROR: Invalid GPU mode {}", self.mode)
        };
        true
    }

    pub fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x8000...0x9FFF => self.vram[addr as usize - 0x8000],
            0xFE00...0xFE9F => self.oam[addr as usize - 0xFE00],
            0xFF40 => {
                self.switchlcd * 0x80 |
                self.bgtile * 0x10 |
                self.bgmap * 0x08 |
                self.switchbg
            },
            0xFF41 => {
                self.mode
            },
            0xFF42 => self.r_scy,
            0xFF43 => self.r_scx,
            0xFF44 => self.line,
            0xFF47 => { println!("WARNING: Attempting to read from background palette (write only)"); 0 },
            _ => { println!("WARNING: GPU cannot read from this memory address. Addr = 0x{:X}", addr); 0},
        }
    }

    pub fn write_u8(&mut self, addr: u16, value: u8) {
        match addr {
            0x8000...0x9FFF => self.vram[addr as usize - 0x8000] = value,
            0xFE00...0xFE9F => self.oam[addr as usize - 0xFE00] = value,
            0xFF40 => {
                self.switchlcd = if value & 0x80 != 0 { 1 } else { 0 };
                self.bgtile = if value & 0x10 != 0 { 1 } else { 0 };
                self.bgmap = if value & 0x08 != 0 { 1 } else { 0 };
                self.switchbg = if value & 0x01 != 0 { 1 } else { 0 };
                println!("WARNING: Writing to $FF40: {:b} bgtile: {} bgmap: {}", value, self.bgtile, self.bgmap);
            },
            0xFF41 => {

            },
            0xFF42 => { println!("New Screen Y: {}", value); self.r_scy = value },
            0xFF43 => { println!("New Screen X: {}", value); self.r_scx = value },
            0xFF44 => println!("WARNING: Attempting to write to current scan line (read only)"),
            0xFF47 => self.r_bgp = value,
            _ => println!("WARNING: GPU cannot write to this memory address. Addr = 0x{:X}", addr),
        }
    }

    fn render_scanline(&mut self) {
        // VRAM offset of the tile map.
        let tile_map_offset = if self.bgmap == 0 { 0x9800u16 } else { 0x9C00u16 };

        let screen_y = self.line;

        let tile_y = screen_y.wrapping_add(self.r_scy) >> 3; // Tile Y.
        let pixel_y = screen_y.wrapping_add(self.r_scy) & 0x7; // Pixel Y within tile.

        // For each pixel on this scanline.
        for screen_x in 0u8..160 {
            let tile_row_start = tile_map_offset + (tile_y as u16) * 32;

            let tile_x = screen_x.wrapping_add(self.r_scx) >> 3; // Tile X.
            let pixel_x = screen_x.wrapping_add(self.r_scx) & 0x7; // Pixel X within tile.

            // Get tile number.
            let tile = self.read_u8(tile_row_start + tile_x as u16);

            // Get row data within tile.
            // Assuming tile set 1.
            let tile_data_start = if self.bgtile == 1 {
                0x8000u16 + (tile as u16) * 16
            } else {
                0x8800u16 + ((((tile as i8) as i16) + 128) as u16) * 16
            };
            let tile_row_offset = (pixel_y as u16) << 1; // Row offset within tile. 2 bytes per row.
            let tile_data_low = self.read_u8(tile_data_start + tile_row_offset);
            let tile_data_high = self.read_u8(tile_data_start + tile_row_offset + 1);

            // Read pixel data.
            let bit_index = 7 - pixel_x;
            let colour_data = ((tile_data_low >> bit_index) & 0b1) | ((tile_data_high >> bit_index) & 0b1) << 1;

            // Map to real colour based on palette.
            let palette = self.r_bgp;
            let colour = (palette >> (colour_data * 2)) & 0b11;
            let display_colour = match colour {
                0 => Colour::RGBA(255, 255, 255, 255),
                1 => Colour::RGBA(192, 192, 192, 255),
                2 => Colour::RGBA(96, 96, 96, 255),
                3 => Colour::RGBA(0, 0, 0, 255),
                _ => panic!("ERROR: Unknown display colour {}", colour)
            };
            self.display.put_pixel(screen_x as usize, screen_y as usize, &display_colour);
        }
    }

    fn display_image(&mut self) -> bool {
        self.display.update()
    }
}
