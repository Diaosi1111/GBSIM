use std::{cell::RefCell, path::Path, rc::Rc};

use log::warn;

use crate::{
    apu::Apu,
    cartridge::{self, Cartridge},
    gpu::{HdmaMode, GPU, HDMA},
    intf::Intf,
    joypad::Joypad,
    serial::Serial,
    term::Term,
    timer::Timer,
};

pub trait Memory {
    fn read_byte(&self, addr: u16) -> u8;
    fn write_byte(&mut self, addr: u16, val: u8);

    fn read_word(&self, addr: u16) -> u16 {
        self.read_byte(addr) as u16 | ((self.read_byte(addr + 1) as u16) << 8)
    }

    fn write_word(&mut self, addr: u16, val: u16) {
        self.write_byte(addr, (val & 0xFF) as u8);
        self.write_byte(addr + 1, (val >> 8) as u8)
    }
}
#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Speed {
    Normal = 0x01,
    Double = 0x02,
}
pub struct Bus {
    pub cartridge: Box<dyn Cartridge>,
    pub term: Term,
    pub gpu: GPU,
    pub joypad: Joypad,
    pub speed: Speed,
    apu: Option<Apu>,
    timer: Timer,
    serial: Serial,
    hdma: HDMA,
    inte: u8,
    intf: Rc<RefCell<Intf>>,
    hram: [u8; 0x7f],
    wram: [u8; 0x8000],
    wram_bank: usize,
}

impl Bus {
    pub fn power_up(path: impl AsRef<Path>) -> Self {
        let cart = cartridge::load(path);
        let term = match cart.read_byte(0x0143) & 0x80 {
            0x80 => Term::GBC,
            _ => Term::GB,
        };
        let intf = Rc::new(RefCell::new(Intf::new()));
        let mut bus = Self {
            term,
            cartridge: cart,
            apu: None,
            gpu: GPU::power_up(term, intf.clone()),
            joypad: Joypad::power_up(intf.clone()),
            speed: Speed::Normal,
            timer: Timer::power_up(intf.clone()),
            serial: Serial::power_up(intf.clone()),
            hdma: HDMA::new(),
            inte: 0x00,
            intf,
            hram: [0x00; 0x7f],
            wram: [0x00; 0x8000],
            wram_bank: 0x01,
        };
        bus.write_byte(0xff05, 0x00);
        bus.write_byte(0xff06, 0x00);
        bus.write_byte(0xff07, 0x00);
        bus.write_byte(0xff10, 0x80);
        bus.write_byte(0xff11, 0xbf);
        bus.write_byte(0xff12, 0xf3);
        bus.write_byte(0xff14, 0xbf);
        bus.write_byte(0xff16, 0x3f);
        bus.write_byte(0xff16, 0x3f);
        bus.write_byte(0xff17, 0x00);
        bus.write_byte(0xff19, 0xbf);
        bus.write_byte(0xff1a, 0x7f);
        bus.write_byte(0xff1b, 0xff);
        bus.write_byte(0xff1c, 0x9f);
        bus.write_byte(0xff1e, 0xff);
        bus.write_byte(0xff20, 0xff);
        bus.write_byte(0xff21, 0x00);
        bus.write_byte(0xff22, 0x00);
        bus.write_byte(0xff23, 0xbf);
        bus.write_byte(0xff24, 0x77);
        bus.write_byte(0xff25, 0xf3);
        bus.write_byte(0xff26, 0xf1);
        bus.write_byte(0xff40, 0x91);
        bus.write_byte(0xff42, 0x00);
        bus.write_byte(0xff43, 0x00);
        bus.write_byte(0xff45, 0x00);
        bus.write_byte(0xff47, 0xfc);
        bus.write_byte(0xff48, 0xff);
        bus.write_byte(0xff49, 0xff);
        bus.write_byte(0xff4a, 0x00);
        bus.write_byte(0xff4b, 0x00);
        bus
    }

    pub fn step(&mut self, cycles: u32) -> u32 {
        let cpu_divider = self.speed as u32;
        let vram_cycles = self.run_dma();
        let gpu_cycles = cycles / cpu_divider + vram_cycles;
        let cpu_cycles = cycles + vram_cycles * cpu_divider;
        self.timer.next(cpu_cycles);
        self.gpu.next(gpu_cycles);
        self.apu.as_mut().map_or((), |s| s.next(gpu_cycles));
        gpu_cycles
    }
    fn run_dma(&mut self) -> u32 {
        if !self.hdma.active {
            return 0;
        }
        match self.hdma.mode {
            HdmaMode::Gdma => {
                let len = u32::from(self.hdma.remain) + 1;
                for _ in 0..len {
                    self.run_dma_hrampart();
                }
                self.hdma.active = false;
                len * 8
            }
            HdmaMode::Hdma => {
                if !self.gpu.h_blank {
                    return 0;
                }
                self.run_dma_hrampart();
                if self.hdma.remain == 0x7f {
                    self.hdma.active = false;
                }
                8
            }
        }
    }

    fn run_dma_hrampart(&mut self) {
        let mmu_src = self.hdma.src;
        for i in 0..0x10 {
            let b: u8 = self.read_byte(mmu_src + i);
            self.gpu.write_byte(self.hdma.dst + i, b);
        }
        self.hdma.src += 0x10;
        self.hdma.dst += 0x10;
        if self.hdma.remain == 0 {
            self.hdma.remain = 0x7f;
        } else {
            self.hdma.remain -= 1;
        }
    }
}

impl Memory for Bus {
    /*
    General Memory Map
      0000-3FFF   16KB ROM Bank 00     (in cartridge, fixed at bank 00)
      4000-7FFF   16KB ROM Bank 01..NN (in cartridge, switchable bank number)
      8000-9FFF   8KB Video RAM (VRAM) (switchable bank 0-1 in CGB Mode)
      A000-BFFF   8KB External RAM     (in cartridge, switchable bank, if any)
      C000-CFFF   4KB Work RAM Bank 0 (WRAM)
      D000-DFFF   4KB Work RAM Bank 1 (WRAM)  (switchable bank 1-7 in CGB Mode)
      E000-FDFF   Same as C000-DDFF (ECHO)    (typically not used)
      FE00-FE9F   Sprite Attribute Table (OAM)
      FEA0-FEFF   Not Usable
      FF00-FF7F   I/O Ports
      FF80-FFFE   High RAM (HRAM)
      FFFF        Interrupt Enable Register
      */
    /// 从指定地址读取一个字节
    fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..0x8000 => self.cartridge.read_byte(addr),
            0x8000..0xa000 => self.gpu.read_byte(addr), //Vram
            0xa000..0xc000 => self.read_byte(addr),     //Eram
            0xc000..0xd000 => self.wram[addr as usize - 0xc000],
            0xd000..0xe000 => self.wram[addr as usize - 0xd000 + 0x1000 * self.wram_bank],
            0xe000..0xf000 => self.wram[addr as usize - 0xe000],
            0xf000..0xfe00 => self.wram[addr as usize - 0xf000 + 0x1000 * self.wram_bank],
            0xfe00..0xfea0 => self.gpu.read_byte(addr), //OAM
            0xfea0..0xff00 => panic!("Not Allow Address: {}", addr),
            0xff00 => self.joypad.read_byte(addr),
            0xff01..=0xff02 => self.serial.read_byte(addr),
            0xff04..=0xff07 => self.timer.read_byte(addr),
            0xff0f => self.intf.borrow().data,
            0xff10..=0xff3f => match &self.apu {
                Some(some) => some.read_byte(addr),
                None => 0x00,
            },
            0xff4d => {
                // let a = if self.speed == Speed::Double { 0x80 } else { 0x00 };
                // let b = if self.shift { 0x01 } else { 0x00 };
                // a | b
                todo!()
            }
            0xff40..=0xff45 | 0xff47..=0xff4b | 0xff4f => self.gpu.read_byte(addr),
            0xff51..=0xff55 => self.hdma.read_byte(addr),
            0xff68..=0xff6b => self.gpu.read_byte(addr),
            0xff70 => self.wram_bank as u8,
            0xff80..0xffff => self.hram[addr as usize - 0xff80], //Hram
            0xffff => self.inte,
            _ => todo!(),
        }
    }
    /// 向指定地址写入一个字节
    fn write_byte(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000..0x8000 => self.cartridge.write_byte(addr, val),
            0x8000..0xa000 => self.gpu.write_byte(addr, val),
            0xa000..0xc000 => self.cartridge.write_byte(addr, val),
            0xc000..0xd000 => self.wram[addr as usize - 0xc000] = val,
            0xd000..0xe000 => self.wram[addr as usize - 0xd000 + 0x1000 * self.wram_bank] = val,
            0xe000..0xf000 => self.wram[addr as usize - 0xe000] = val,
            0xf000..0xfe00 => self.wram[addr as usize - 0xf000 + 0x1000 * self.wram_bank] = val,
            0xfe00..0xfea0 => self.gpu.write_byte(addr, val),
            0xfea0..0xff00 => warn!("Write to unused address: 0x{:04x} - 0x{:02x}", addr, val),
            0xff00 => self.joypad.write_byte(addr, val),
            0xff01..0xff03 => self.serial.write_byte(addr, val),
            0xff04..0xff08 => self.timer.write_byte(addr, val),
            0xff0f => self.intf.borrow_mut().data = val,
            0xff10..0xff40 => self.apu.as_mut().map_or((), |s| s.write_byte(addr, val)),
            0xff46 => {
                // Writing to this register launches a DMA transfer from ROM or RAM to OAM memory (sprite attribute
                // table).
                // See: http://gbdev.gg8.se/wiki/articles/Video_Display#FF46_-_DMA_-_DMA_Transfer_and_Start_Address_.28R.2FW.29
                assert!(val <= 0xf1);
                let base = (val as u16) << 8;
                for i in 0..0xa0 {
                    let b = self.read_byte(base + i);
                    self.write_byte(0xfe00 + i, b);
                }
            }
            0xff4d => todo!(),
            0xff40..=0xff45 | 0xff47..=0xff4b | 0xff4f => self.gpu.write_byte(addr, val),
            0xff51..=0xff55 => self.hdma.write_byte(addr, val),
            0xff68..=0xff6b => self.gpu.write_byte(addr, val),
            0xff70 => {
                self.wram_bank = match val & 0x7 {
                    0 => 1,
                    n => n as usize,
                };
            }
            0xff80..0xffff => self.hram[addr as usize - 0xff80] = val,
            0xffff => self.inte = val,
            _ => {}
        }
    }
}
