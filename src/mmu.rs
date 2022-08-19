use std::{path::PathBuf, io::Read};

use log::info;

#[derive(Default)]
pub struct MMU {
    memory: Box<[u8]>,
}

impl MMU {
    pub fn new() -> Self {
        Self {
            memory: Box::new([0u8; 0xffff]),
        }
    }

    /// 从指定地址读取一个字节
    pub fn read_byte(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }
    /// 向指定地址写入一个字节
    pub fn write_byte(&mut self, addr: u16, val: u8) {
        self.memory[addr as usize] = val;
    }
    pub fn read_word(&self, addr: u16) -> u16 {
        self.memory[addr as usize] as u16 | (self.memory[(addr + 1) as usize] as u16) << 8
    }
    pub fn write_word(&mut self, addr: u16, val: u16) {
        self.memory[addr as usize] = (val & 0x00ff) as u8;
        self.memory[(addr + 1) as usize] = (val >> 8) as u8;
    }
}

pub fn load_rom(path: PathBuf) -> Result<(), std::io::Error> {
    info!("正在加载rom:{:?}",path);
    use std::fs::File;
    let mut f = File::open(path)?;
    let mut rom = Vec::new();
    f.read_to_end(&mut rom).unwrap();
    if rom.len() < 0x150 {
        panic!("Missing required information area which located at 0100-014F")
    }
    let rom_max = rom_size(rom[0x0148]);
    if rom.len() > rom_max {
        panic!("Rom size more than {}", rom_max);
    }
    Ok(())
}
// ROM:
// 0100-0103 - Entry Point
// 0104-0133 - Nintendo Logo
// 0134-0143 - Title (013F-0142 - Manufacturer Code  0143 - CGB Flag)
// 0147 - Cartridge Type
// 0148 - ROM Size
// 0149 - RAM Size Specifies the size of the external RAM in the cartridge (if any).
// 014A - Destination Code
// 014B - Old Licensee Code
// 014D - Header Checksum
// 014E-014F - Global Checksum


// Specifies the ROM Size of the cartridge. Typically calculated as "32KB shl N".
//   00h -  32KByte (no ROM banking)
//   01h -  64KByte (4 banks)
//   02h - 128KByte (8 banks)
//   03h - 256KByte (16 banks)
//   04h - 512KByte (32 banks)
//   05h -   1MByte (64 banks)  - only 63 banks used by MBC1
//   06h -   2MByte (128 banks) - only 125 banks used by MBC1
//   07h -   4MByte (256 banks)
//   52h - 1.1MByte (72 banks)
//   53h - 1.2MByte (80 banks)
//   54h - 1.5MByte (96 banks)
fn rom_size(b: u8) -> usize {
    let bank = 16384;
    match b {
        0x00 => bank * 2,
        0x01 => bank * 4,
        0x02 => bank * 8,
        0x03 => bank * 16,
        0x04 => bank * 32,
        0x05 => bank * 64,
        0x06 => bank * 128,
        0x07 => bank * 256,
        0x08 => bank * 512,
        0x52 => bank * 72,
        0x53 => bank * 80,
        0x54 => bank * 96,
        n => panic!("Unsupported rom size: 0x{:02x}", n),
    }
}
