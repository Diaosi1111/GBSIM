use std::{
    io::{Read, Write},
    path::{Path, PathBuf}, fs::File,
};

use crate::bus::Memory;
use log::{debug, info, warn};
pub trait Cartridge: Memory + SaveRam {
    // Title of the game in UPPER CASE ASCII. If it is less than 16 characters then the remaining bytes are filled with
    // 00's. When inventing the CGB, Nintendo has reduced the length of this area to 15 characters, and some months
    // later they had the fantastic idea to reduce it to 11 characters only. The new meaning of the ex-title bytes is
    // described below.
    fn title(&self) -> String {
        let mut buf = String::new();
        let ic = 0x0134;
        let oc = if self.read_byte(0x0143) == 0x80 {
            0x013e
        } else {
            0x0143
        };
        for i in ic..oc {
            match self.read_byte(i) {
                0 => break,
                v => buf.push(v as char),
            }
        }
        buf
    }
}

pub trait SaveRam {
    fn sav(&self) {}
}
pub struct RomOnly {
    rom: Vec<u8>,
}
impl RomOnly {
    pub fn init(rom: Vec<u8>) -> Self {
        Self { rom }
    }
}
impl Memory for RomOnly {
    fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..0x8000 => self.rom[addr as usize],
            _ => panic!("地址超出范围! addr:{}", addr),
        }
    }

    fn write_byte(&mut self, addr: u16, v: u8) {
        warn!(
            "RomOnly should not write this address: {} ,value: {}",
            addr, v
        )
    }
}
impl SaveRam for RomOnly {
    fn sav(&self) {
        debug!("RomOnly Can't Save Ram");
        // if self.sav_path.to_str().unwrap().is_empty() {
        //     return;
        // }
        // File::create(self.sav_path.clone())
        //     .and_then(|mut f| f.write_all(&self.ram))
        //     .unwrap()
    }
}
impl Cartridge for RomOnly {}

pub struct MBC1 {
    rom: Vec<u8>,
    ram: Vec<u8>,
    ram_enable: bool,
    bank: u8,
    bank_mode: bool,
    sav_path: PathBuf,
}

impl MBC1 {
    pub fn init(rom: Vec<u8>, ram: Vec<u8>, sav_path: impl AsRef<Path>) -> Self {
        Self {
            rom,
            ram,
            ram_enable: false,
            bank: 0x01,
            bank_mode: false,
            sav_path: PathBuf::from(sav_path.as_ref()),
        }
    }

    fn rom_bank(&self) -> usize {
        match self.bank_mode {
            true => (self.bank & 0x1f) as usize,  // 4M/32k
            false => (self.bank & 0x7f) as usize, //16M/8k
        }
    }

    fn ram_bank(&self) -> usize {
        match self.bank_mode {
            true => ((self.bank & 0x60) >> 5) as usize,
            false => 0x00,
        }
    }
}

impl Memory for MBC1 {
    fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..0x4000 => self.rom[addr as usize],
            0x4000..0x8000 => self.rom[self.rom_bank() * 0x4000 + addr as usize - 0x4000],
            0xa000..0xc000 => {
                if self.ram_enable {
                    self.ram[self.ram_bank() * 0x2000 + addr as usize - 0xa000]
                } else {
                    0x00
                }
            }
            _ => panic!("地址超出范围! addr:{}", addr),
        }
    }

    fn write_byte(&mut self, addr: u16, v: u8) {
        match addr {
            0x0000..0x2000 => self.ram_enable = v & 0x0f == 0x0a,
            0x2000..0x4000 => {
                let n = v & 0x1f;
                let n = match n {
                    0x00 => 0x01,
                    _ => n,
                };
                self.bank = (self.bank & 0x60) | n;
            }
            0x4000..0x6000 => {
                let n = v & 0x03;
                self.bank = self.bank & 0x9f | (n << 5)
            }
            0x6000..0x8000 => {
                self.bank_mode = match v {
                    0x00 => false,
                    0x01 => true,
                    _ => panic!("非法的Bank Mode值: {}", v),
                }
            }
            0xa000..0xc000 => {
                if self.ram_enable {
                    let i = self.ram_bank() * 0x2000 + addr as usize - 0xa000;
                    self.ram[i] = v
                }
            }
            _ => panic!("地址超出范围! addr:{}", addr),
        }
    }
}
impl SaveRam for MBC1 {
    fn sav(&self) {
        debug!("RomOnly Can't Save Ram");
        if self.sav_path.to_str().unwrap().is_empty() {
            return;
        }
        File::create(self.sav_path.clone())
            .and_then(|mut f| f.write_all(&self.ram))
            .unwrap()
    }
}
impl Cartridge for MBC1 {}
/*
MBC2 (max 256KByte ROM and 512x4 bits RAM)

0000-3FFF - ROM Bank 00 (Read Only)
Same as for MBC1.

4000-7FFF - ROM Bank 01-0F (Read Only)
Same as for MBC1, but only a total of 16 ROM banks is supported.

A000-A1FF - 512x4bits RAM, built-in into the MBC2 chip (Read/Write)
The MBC2 doesn't support external RAM, instead it includes 512x4 bits of built-in RAM (in the MBC2 chip itself). It still requires an external battery to save data during power-off though.
As the data consists of 4bit values, only the lower 4 bits of the "bytes" in this memory area are used.

0000-1FFF - RAM Enable (Write Only)
The least significant bit of the upper address byte must be zero to enable/disable cart RAM. For example the following addresses can be used to enable/disable cart RAM: 0000-00FF, 0200-02FF, 0400-04FF, ..., 1E00-1EFF.
The suggested address range to use for MBC2 ram enable/disable is 0000-00FF.

2000-3FFF - ROM Bank Number (Write Only)
Writing a value (XXXXBBBB - X = Don't cares, B = bank select bits) into 2000-3FFF area will select an appropriate ROM bank at 4000-7FFF.

The least significant bit of the upper address byte must be one to select a ROM bank. For example the following addresses can be used to select a ROM bank: 2100-21FF, 2300-23FF, 2500-25FF, ..., 3F00-3FFF.
The suggested address range to use for MBC2 rom bank selection is 2100-21FF.

*/
pub struct MBC2 {
    rom: Vec<u8>,
    ram: Vec<u8>,
    ram_enable: bool,
    rom_bank: usize,
}

impl MBC2 {}

impl Memory for MBC2 {
    fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..0x4000 => self.rom[addr as usize],
            0x4000..0x8000 => self.rom[self.rom_bank * 0x4000 + addr as usize - 0x4000],
            0xa000..0xa200 => {
                if self.ram_enable {
                    self.ram[(addr - 0xa000) as usize]
                } else {
                    0x00
                }
            }
            _ => panic!("地址超出范围! addr:{}", addr),
        }
    }

    fn write_byte(&mut self, addr: u16, v: u8) {
        match addr {
            0x0000..0x2000 => {
                if addr & 0x0100 == 0 {
                    self.ram_enable = v & 0x0f == 0x0a
                }
            }
            0x2000..0x4000 => {
                if addr & 0x0100 != 0 {
                    self.rom_bank = (v & 0x0f) as usize
                }
            }
            0xa000..0xa200 => {
                if self.ram_enable {
                    self.ram[(addr - 0xa000) as usize] = v & 0x0f
                }
            }
            _ => panic!("地址超出范围! addr:{}", addr),
        }
    }
}

// 0x0147:
//   00h  ROM ONLY                 13h  MBC3+RAM+BATTERY
//   01h  MBC1                     15h  MBC4
//   02h  MBC1+RAM                 16h  MBC4+RAM
//   03h  MBC1+RAM+BATTERY         17h  MBC4+RAM+BATTERY
//   05h  MBC2                     19h  MBC5
//   06h  MBC2+BATTERY             1Ah  MBC5+RAM
//   08h  ROM+RAM                  1Bh  MBC5+RAM+BATTERY
//   09h  ROM+RAM+BATTERY          1Ch  MBC5+RUMBLE
//   0Bh  MMM01                    1Dh  MBC5+RUMBLE+RAM
//   0Ch  MMM01+RAM                1Eh  MBC5+RUMBLE+RAM+BATTERY
//   0Dh  MMM01+RAM+BATTERY        FCh  POCKET CAMERA
//   0Fh  MBC3+TIMER+BATTERY       FDh  BANDAI TAMA5
//   10h  MBC3+TIMER+RAM+BATTERY   FEh  HuC3
//   11h  MBC3                     FFh  HuC1+RAM+BATTERY
//   12h  MBC3+RAM
pub fn load(path: impl AsRef<Path>) -> Box<dyn Cartridge> {
    info!("正在加载rom:{:?}", path.as_ref());
    let mut f = File::open(path.as_ref()).unwrap();
    let mut rom = Vec::new();
    f.read_to_end(&mut rom).unwrap();

    if rom.len() < 0x150 {
        panic!("ROM 格式错误: 过短")
    }
    let rom_max = rom_size(rom[0x0148]);
    if rom.len() > rom_max {
        panic!("ROM 格式错误: 过大 {}", rom_max);
    }
    let cart: Box<dyn Cartridge> = match rom[0x0147] {
        0x00 => Box::new(RomOnly::init(rom)),
        0x01 => Box::new(MBC1::init(rom, vec![], "")),
        0x02 => {
            let ram_max = ram_size(rom[0x0149]);
            Box::new(MBC1::init(rom, vec![0; ram_max], ""))
        }
        0x03 => {
            let ram_max = ram_size(rom[0x0149]);
            let sav_path = path.as_ref().to_path_buf().with_extension("sav");
            let ram = load_ram(sav_path.clone(), ram_max);
            Box::new(MBC1::init(rom, ram, sav_path))
        }
        n => panic!("错误的卡带类型: 0x{:02x}", n),
    };
    debug!("加载卡带: {}", cart.title());
    debug!("卡带类型: {}", mbc_info(cart.read_byte(0x0147)));
    ensure_logo(cart.as_ref());
    ensure_header_checksum(cart.as_ref());
    cart
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

// 0148 - ROM Size
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

// 0149 - RAM Size
// Specifies the size of the external RAM in the cartridge (if any).
//   00h - None
//   01h - 2 KBytes
//   02h - 8 Kbytes
//   03h - 32 KBytes (4 banks of 8KBytes each)
// When using a MBC2 chip 00h must be specified in this entry, even though the MBC2 includes a built-in RAM of 512 x 4 bits.
fn ram_size(b: u8) -> usize {
    match b {
        0x00 => 0,
        0x01 => 1024 * 2,
        0x02 => 1024 * 8,
        0x03 => 1024 * 32,
        0x04 => 1024 * 128, //fixme
        0x05 => 1024 * 64,
        n => panic!("Unsupported ram size: 0x{:02x}", n),
    }
}

/// 读取外置带电池的Ram中的数据
fn load_ram(path: impl AsRef<Path>, size: usize) -> Vec<u8> {
    match File::open(path) {
        Ok(mut ok) => {
            let mut ram = Vec::new();
            ok.read_to_end(&mut ram).unwrap();
            ram
        }
        Err(_) => vec![0; size],
    }
}

// Readable form of MBC representation
fn mbc_info(b: u8) -> String {
    String::from(match b {
        0x00 => "ROM ONLY",
        0x01 => "MBC1",
        0x02 => "MBC1+RAM",
        0x03 => "MBC1+RAM+BATTERY",
        0x05 => "MBC2",
        0x06 => "MBC2+BATTERY",
        0x08 => "ROM+RAM",
        0x09 => "ROM+RAM+BATTERY",
        0x0b => "MMM01",
        0x0c => "MMM01+RAM",
        0x0d => "MMM01+RAM+BATTERY",
        0x0f => "MBC3+TIMER+BATTERY",
        0x10 => "MBC3+TIMER+RAM+BATTERY",
        0x11 => "MBC3",
        0x12 => "MBC3+RAM",
        0x13 => "MBC3+RAM+BATTERY",
        0x15 => "MBC4",
        0x16 => "MBC4+RAM",
        0x17 => "MBC4+RAM+BATTERY",
        0x19 => "MBC5",
        0x1a => "MBC5+RAM",
        0x1b => "MBC5+RAM+BATTERY",
        0x1c => "MBC5+RUMBLE",
        0x1d => "MBC5+RUMBLE+RAM",
        0x1e => "MBC5+RUMBLE+RAM+BATTERY",
        0xfc => "POCKET CAMERA",
        0xfd => "BANDAI TAMA5",
        0xfe => "HuC3",
        0x1f => "HuC1+RAM+BATTERY",
        n => panic!("Unsupported cartridge type: 0x{:02x}", n),
    })
}

// These bytes define the bitmap of the Nintendo logo that is displayed when the gameboy gets turned on.
// The reason for joining is because if the pirates copy the cartridge, they must also copy Nintendo's LOGO,
// which infringes the trademark law. In the early days, the copyright law is not perfect for the determination of
// electronic data.
// The hexdump of this bitmap is:
const NINTENDO_LOGO: [u8; 48] = [
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

// Ensure Nintendo Logo.
fn ensure_logo(cart: &dyn Cartridge) {
    for i in 0..48 {
        if cart.read_byte(0x0104 + i as u16) != NINTENDO_LOGO[i as usize] {
            panic!("Nintendo logo is incorrect")
        }
    }
}
// In position 0x14d, contains an 8 bit checksum across the cartridge header bytes 0134-014C. The checksum is
// calculated as follows:
//
//   x=0:FOR i=0134h TO 014Ch:x=x-MEM[i]-1:NEXT
//
// The lower 8 bits of the result must be the same than the value in this entry. The GAME WON'T WORK if this
// checksum is incorrect.
fn ensure_header_checksum(cart: &dyn Cartridge) {
    let mut v: u8 = 0;
    for i in 0x0134..0x014d {
        v = v.wrapping_sub(cart.read_byte(i)).wrapping_sub(1);
    }
    if cart.read_byte(0x014d) != v {
        panic!("Cartridge's header checksum is incorrect")
    }
}
