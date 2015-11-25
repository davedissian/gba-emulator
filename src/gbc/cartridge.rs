use std::str;
use std::fs::File;
use std::io::Read;

pub struct Cartridge {
    pub rom: Vec<u8>,
    pub title: String
}

impl Cartridge {
    pub fn load(filename: &str) -> Result<Cartridge, String> {
        // Open a cartridge file
        let mut f = match File::open(filename) {
            Ok(f) => f,
            Err(_) => return Err("Unable to open cartridge file".to_string())
        };

        // Read content from the file
        let mut contents = Vec::new();
        if let Err(_) = f.read_to_end(&mut contents) {
            return Err("Unable to read cartridge file".to_string());
        }

        // Verify the Nintendo Logo bytes
        // TODO

        // Grab the game title from bytes 0134-0143
        let title_bytes = contents[0x0134..0x0143].iter().cloned().collect();
        let title = String::from_utf8(title_bytes).unwrap();

        // Return a new cartridge object
        Ok(Cartridge {
            rom: contents,
            title: title
        })
    }
}