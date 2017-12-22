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

    pub fn tick(&mut self, cycles: u32) -> bool {
        self.modeclock = self.modeclock.wrapping_add(cycles);
        //println!("self.modeclock {}", self.modeclock);
        match self.mode {
            // OAM read mode.
            2 => if self.modeclock >= 80 { self.modeclock = 0; self.mode = 3; },
            // VRAM read mode, scanline active. Treat end of mode 3 as end of scanline.
            3 => if self.modeclock >= 172 {
                self.modeclock = 0;
                self.mode = 0;
                self.renderscanline();
            },
            // Hblank. After the last Hblank, update the screen.
            0 => if self.modeclock >= 204 {
                self.modeclock = 0;
                self.line += 1;
                if self.line == 143 {
                    self.mode = 1;
                    if !self.putimagedata() {
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
                println!("WARNING: Writing to $FF40: {:b}", value);
                self.switchlcd = if value & 0x80 != 0 { 1 } else { 0 };
                self.bgtile = if value & 0x10 != 0 { 1 } else { 0 };
                self.bgmap = if value & 0x08 != 0 { 1 } else { 0 };
                self.switchbg = if value & 0x01 != 0 { 1 } else { 0 };
            },
            0xFF41 => {

            },
            0xFF42 => self.r_scy = value,
            0xFF43 => self.r_scx = value,
            0xFF44 => println!("WARNING: Attempting to write to current scan line (read only)"),
            0xFF47 => self.r_bgp = value,
            _ => println!("WARNING: GPU cannot write to this memory address. Addr = 0x{:X}", addr),
        }
    }

    fn renderscanline(&mut self) {
        // VRAM offset of the tile map.
        let mut tile_map_offset = if self.bgmap == 0 { 0x1800u16 } else { 0x1C00u16 };
        tile_map_offset += ((self.line as u16 + self.r_scy as u16) & 0xff) >> 3;

        // Which tile to start with.
        let mut tile_offset = (self.r_scx >> 3) as u16;

        // LOCATION WITHIN TILE:

        // Which line of pixels to use in the tiles
        let y = (self.line + self.r_scy) & 0x7;

        // Where in the tileline to start
        let mut x = self.r_scx & 0x7;

        // Read tile index from VRAM.
        let mut tile = self.read_u8(0x8000u16 + tile_map_offset + tile_offset) as u16;
        if self.bgtile == 0{
            tile = (((tile as u8) as i8) as i16 + 256) as u16;
        }

        // For each pixel on this scanline.
        for i in 0..160 {
            let palette = self.r_bgp;

            // Read tile data.
            let tile_data_offset = tile << 4; // 16-bytes per tile.
            let tile_row_offset = (y << 1) as u16;
            let tile_data_low = self.read_u8(0x8000u16 + tile_data_offset + tile_row_offset);
            let tile_data_high = self.read_u8(0x8000u16 + tile_data_offset + tile_row_offset + 1);

            // Tile row: 2 bytes (where tile[n][x] is the low bit and tile[n+1][x] is the high bit)

            // Extract colour
            let bit_index = 7 - x;
            let colour_data = ((tile_data_low >> bit_index) & 0b1) | ((tile_data_high >> bit_index) & 0b1) << 1;

            // Map to real colour based on palette.
            let colour = (palette >> (colour_data * 2)) & 0b11;
            let display_colour = match colour {
                0 => Colour::RGBA(255, 255, 255, 255),
                1 => Colour::RGBA(192, 192, 192, 255),
                2 => Colour::RGBA(96, 96, 96, 255),
                3 => Colour::RGBA(0, 0, 0, 255),
                _ => panic!("ERROR: Unknown display colour {}", colour)
            };
            self.display.put_pixel(i, self.line as usize, &display_colour);

            println!("tile {} {} (at {:x}) - x,y: {},{} - data start {:x} - colour {} - tile offset {}/32",
                     tile,
                     ((0x8000u16 + tile_data_offset + tile_row_offset) >> 4) & 511,
                     0x8000u16 + tile_data_offset + tile_row_offset,
                        x, y,
                     0x8000u16 + tile_data_offset,
                     colour, tile_offset);

            // Increment x position in tile.
            x += 1;

            // Reached end of tile?
            if x == 8 {
                x = 0;

                // Advance tile.
                tile_offset = (tile_offset + 1) & 0x1f;

                // Read tile index from VRAM.
                tile = self.read_u8(0x8000 + tile_map_offset + tile_offset) as u16;
                if self.bgtile == 0 {
                    tile = (((tile as u8) as i8) as i16 + 256) as u16;
                }
            }
        }
    }

    fn putimagedata(&mut self) -> bool {
        self.display.update()
    }
}
