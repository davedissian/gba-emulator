Game Boy Information
---------------------

Specification:
----------
    CPU - 8-bit (Similar to the Z80 processor)
    Clock Speed - 4.194304MHz (4.295454MHz for SGB, max. 8.4MHz for CGB)
    Work RAM - 8K Byte (32K Byte for CGB)
    Video RAM - 8K Byte (16K Byte for CGB)
    Screen Size - 2.6"
    Resolution - 160x144 (20x18 tiles)
    Max sprites - Max 40 per screen, 10 per line
    Sprite sizes - 8x8 or 8x16
    Palettes - 1x4 BG, 2x3 OBJ (for CGB: 8x4 BG, 8x3 OBJ)
    Colors - 4 grayshades (32768 colors for CGB)
    Horiz Sync - 9198 KHz (9420 KHz for SGB)
    Vert Sync - 59.73 Hz (61.17 Hz for SGB)
    Sound - 4 channels with stereo sound
    Power - DC6V 0.7W (DC3V 0.7W for GB Pocket, DC3V 0.6W for CGB)

General Memory Map
-----------
16 bit address bus

    0000-3FFF   16KB ROM Bank 00 (in cartridge, fixed at bank 00)
    4000-7FFF   16KB ROM Bank 01..NN (in cartridge, switchable bank number)
    8000-9FFF   8KB Video RAM (VRAM) (switchable bank 0-1 in CGB Mode)
    A000-BFFF   8KB External RAM (in cartridge, switchable bank, if any)
    C000-CFFF   4KB Work RAM Bank 0 (WRAM)
    D000-DFFF   4KB Work RAM Bank 1 (WRAM) (switchable bank 1-7 in CGB Mode)
    E000-FDFF   Same as C000-DDFF (ECHO) (typically not used) 
    FE00-FE9F   Sprite Attribute Table (OAM)
    FEA0-FEFF   Not Usable
    FF00-FF7F   I/O Ports
    FF80-FFFE   High RAM (HRAM)
    FFFF        Interrupt Enable Register 