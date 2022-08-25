use std::{cell::RefCell, rc::Rc, thread};

use log::debug;
// use log::warn;

use crate::bus::Bus;
use crate::bus::Memory;
use crate::term::Term;

pub const CLOCK_FREQUENCY: u32 = 4_194_304;
pub const STEP_TIME: u32 = 16;
pub const STEP_CYCLES: u32 = (STEP_TIME as f64 / (1000_f64 / CLOCK_FREQUENCY as f64)) as u32;

// Nintendo documents describe the CPU & instructions speed in machine cycles while this document describes them in
// clock cycles. Here is the translation:
//   1 machine cycle = 4 clock cycles
//                   GB CPU Speed    NOP Instruction
// Machine Cycles    1.05MHz         1 cycle
// Clock Cycles      4.19MHz         4 cycles
//
//  0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
const OP_CYCLES: [u32; 256] = [
    1, 3, 2, 2, 1, 1, 2, 1, 5, 2, 2, 2, 1, 1, 2, 1, // 0
    0, 3, 2, 2, 1, 1, 2, 1, 3, 2, 2, 2, 1, 1, 2, 1, // 1
    2, 3, 2, 2, 1, 1, 2, 1, 2, 2, 2, 2, 1, 1, 2, 1, // 2
    2, 3, 2, 2, 3, 3, 3, 1, 2, 2, 2, 2, 1, 1, 2, 1, // 3
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, // 4
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, // 5
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, // 6
    2, 2, 2, 2, 2, 2, 0, 2, 1, 1, 1, 1, 1, 1, 2, 1, // 7
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, // 8
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, // 9
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, // a
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, // b
    2, 3, 3, 4, 3, 4, 2, 4, 2, 4, 3, 0, 3, 6, 2, 4, // c
    2, 3, 3, 0, 3, 4, 2, 4, 2, 4, 3, 0, 3, 0, 2, 4, // d
    3, 3, 2, 0, 0, 4, 2, 4, 4, 1, 4, 0, 0, 0, 2, 4, // e
    3, 3, 2, 1, 0, 4, 2, 4, 3, 2, 4, 1, 0, 0, 2, 4, // f
];

//  0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
const CB_CYCLES: [u32; 256] = [
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 0
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 1
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 2
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 3
    2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, // 4
    2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, // 5
    2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, // 6
    2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, // 7
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 8
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 9
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // a
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // b
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // c
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // d
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // e
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // f
];

/// 时钟：Z80 包含两种类型的时钟（m 和 t）_clock： {m：0， t：0}，
/// 寄存器集 _r： { a：0， b：0， c：0， d：0， e：0， h：0， l：0， f：0， // 8 位寄存器
/// pc：0， sp：0， // 16 位寄存器
/// m：0， t：0 // 时钟用于最后一个输入
///
/// 寄存器F:标志寄存器。在Gameboy中他有4种标志:
/// 1. Zero (0x80): 如果最有一个操作产生了结果零;
/// 2. Operation (0x40): 如果最后一个操作是减法;
/// 3. Half-carry (0x20): 如果最后一个操作的低4位产生了一个溢出即大于15;
/// 4. Carry (0x10): 如果最后一个操作产生了一个超过255（对于加法）或者小于0（对于减法）的结果.
pub struct Z80 {
    //Main rigesters
    pub regs: Registers,
    halted: bool,
    ime: bool,
    bus: Rc<RefCell<Bus>>,
}
#[derive(Default)]
pub struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    sp: u16, //栈顶指针
    pub pc: u16,
}

/// 低4位保留
/// 第8位: Zero Flag
/// 第7位: 加减标志
/// 第6位：半进位FLAG
/// 第5位: 进位标志
pub enum Flag {
    Z = 0x80,
    N = 0x40,
    H = 0x20,
    C = 0x10,
}

impl Registers {
    pub fn get_af(&self) -> u16 {
        (self.a as u16) << 8 | self.f as u16
    }
    pub fn set_af(&mut self, val: u16) {
        self.f = (val & 0x00ff) as u8;
        self.a = ((val >> 8) & 0x00ff) as u8
    }
    pub fn get_bc(&self) -> u16 {
        (self.b as u16) << 8 | self.c as u16
    }
    pub fn set_bc(&mut self, val: u16) {
        self.c = (val & 0x00ff) as u8;
        self.b = ((val >> 8) & 0x00ff) as u8
    }
    pub fn get_de(&self) -> u16 {
        (self.d as u16) << 8 | self.e as u16
    }
    pub fn set_de(&mut self, val: u16) {
        self.e = (val & 0x00ff) as u8;
        self.d = ((val >> 8) & 0x00ff) as u8
    }
    pub fn get_hl(&self) -> u16 {
        (self.h as u16) << 8 | self.l as u16
    }
    pub fn set_hl(&mut self, val: u16) {
        self.l = (val & 0x00ff) as u8;
        self.h = ((val >> 8) & 0x00ff) as u8
    }
    /// GB上电通过bios后的状态
    pub fn reset(&mut self, term: Term) {
        match term {
            Term::GB => {
                self.a = 0x01;
            }
            Term::GBP => {
                self.a = 0xff;
            }
            Term::GBC => {
                self.a = 0x11;
            }
            Term::SGB => {
                self.a = 0x01;
            }
        }
        self.f = 0xb0;
        self.b = 0x00;
        self.c = 0x13;
        self.d = 0x00;
        self.e = 0xd8;
        self.h = 0x01;
        self.l = 0x4d;
        self.sp = 0xfffe;
        self.pc = 0x0100;
    }
    pub fn set_flag(&mut self, flag: Flag, switch: bool) {
        if switch {
            self.f |= flag as u8
        } else {
            self.f &= !(flag as u8)
        }
    }
    pub fn get_flag(&self, flag: Flag) -> bool {
        (self.f & flag as u8) != 0
    }
}

impl Z80 {
    pub fn new(bus: Rc<RefCell<Bus>>) -> Self {
        let mut a = Z80 {
            bus,
            regs: Registers::default(),
            halted: false,
            ime: true,
        };
        a.reset();
        a
    }
    /// 恢复执行完bios后的状态
    pub fn reset(&mut self) {
        self.regs.reset(self.bus.borrow().term);
        self.halted = false;
        self.ime = true;
    }

    pub fn run(&mut self) -> u32 {
        let mac = {
            let c = self.handle_interrupt();
            if c != 0 {
                c
            } else if self.halted {
                OP_CYCLES[0]
            } else {
                self.step()
            }
        };
        mac * 4
    }
    // The IME (interrupt master enable) flag is reset by DI and prohibits all interrupts. It is set by EI and
    // acknowledges the interrupt setting by the IE register.
    // 1. When an interrupt is generated, the IF flag will be set.
    // 2. If the IME flag is set & the corresponding IE flag is set, the following 3 steps are performed.
    // 3. Reset the IME flag and prevent all interrupts.
    // 4. The PC (program counter) is pushed onto the stack.
    // 5. Jump to the starting address of the interrupt.
    fn handle_interrupt(&mut self) -> u32 {
        if !self.halted && !self.ime {
            return 0;
        }
        // FF0F - IF - Interrupt Flag (R/W)
        //   Bit 0: V-Blank  Interrupt Request (INT 40h)  (1=Request)
        //   Bit 1: LCD STAT Interrupt Request (INT 48h)  (1=Request)
        //   Bit 2: Timer    Interrupt Request (INT 50h)  (1=Request)
        //   Bit 3: Serial   Interrupt Request (INT 58h)  (1=Request)
        //   Bit 4: Joypad   Interrupt Request (INT 60h)  (1=Request)
        let intf = self.bus.borrow().read_byte(0xff0f);
        // FFFF - IE - Interrupt Enable (R/W)
        //   Bit 0: V-Blank  Interrupt Enable  (INT 40h)  (1=Enable)
        //   Bit 1: LCD STAT Interrupt Enable  (INT 48h)  (1=Enable)
        //   Bit 2: Timer    Interrupt Enable  (INT 50h)  (1=Enable)
        //   Bit 3: Serial   Interrupt Enable  (INT 58h)  (1=Enable)
        //   Bit 4: Joypad   Interrupt Enable  (INT 60h)  (1=Enable)
        let inte = self.bus.borrow().read_byte(0xffff);
        // 任何在IF中设为1的比特位代表 请求 一个中断
        // 只有IE中相应的比特位设为1同时IME标志为1时才会执行中断
        // 同时触发的终端会按照比特位由低(0-V-Blank)向高(4-Joypad)执行
        // CPU会自动将IME设为0来阻止中断执行过程中的二次中断
        // 中断执行结束后会使用RETI来返回并将IME设为1
        let ii = intf & inte;
        if ii == 0x00 {
            return 0;
        }
        self.halted = false;
        if !self.ime {
            return 0;
        }
        self.ime = false;

        // 关闭将要执行的中断，其它的写回IF
        let n = ii.trailing_zeros();
        let intf = intf & !(1 << n);
        self.bus.borrow_mut().write_byte(0xff0f, intf);

        self.stack_push(self.regs.pc);
        // 开始执行中断
        self.regs.pc = 0x0040 | ((n as u16) << 3);
        4
    }

    /// 执行一条指令并返回用时
    pub fn step(&mut self) -> u32 {
        let op = self.imm();
        // debug!("pc:0x{:4x}, opcode: 0x{:02x}", self.regs.pc - 1, op);
        let mut cb = 0u8;
        match op {
            // mnemonic, opcode bytes, clock cycles, affected flags (ordered as znhc), and explanatation.
            // 以下来自Pan Docs
            // 16位地址，8位值
            // ()代表取地址上的值

            //-----GMB 8bit-Loadcommands-----
            //  ld   r,r         xx         4 ---- r=r
            0x40 => {}
            0x41 => self.regs.b = self.regs.c,
            0x42 => self.regs.b = self.regs.d,
            0x43 => self.regs.b = self.regs.e,
            0x44 => self.regs.b = self.regs.h,
            0x45 => self.regs.b = self.regs.l,
            0x47 => self.regs.b = self.regs.a,
            0x48 => self.regs.c = self.regs.b,
            0x49 => {}
            0x4a => self.regs.c = self.regs.d,
            0x4b => self.regs.c = self.regs.e,
            0x4c => self.regs.c = self.regs.h,
            0x4d => self.regs.c = self.regs.l,
            0x4f => self.regs.c = self.regs.a,
            0x50 => self.regs.d = self.regs.b,
            0x51 => self.regs.d = self.regs.c,
            0x52 => {}
            0x53 => self.regs.d = self.regs.e,
            0x54 => self.regs.d = self.regs.h,
            0x55 => self.regs.d = self.regs.l,
            0x57 => self.regs.d = self.regs.a,
            0x58 => self.regs.e = self.regs.b,
            0x59 => self.regs.e = self.regs.c,
            0x5a => self.regs.e = self.regs.d,
            0x5b => {}
            0x5c => self.regs.e = self.regs.h,
            0x5d => self.regs.e = self.regs.l,
            0x5f => self.regs.e = self.regs.a,
            0x60 => self.regs.h = self.regs.b,
            0x61 => self.regs.h = self.regs.c,
            0x62 => self.regs.h = self.regs.d,
            0x63 => self.regs.h = self.regs.e,
            0x64 => {}
            0x65 => self.regs.h = self.regs.l,
            0x67 => self.regs.h = self.regs.a,
            0x68 => self.regs.l = self.regs.b,
            0x69 => self.regs.l = self.regs.c,
            0x6a => self.regs.l = self.regs.d,
            0x6b => self.regs.l = self.regs.e,
            0x6c => self.regs.l = self.regs.h,
            0x6d => {}
            0x6f => self.regs.l = self.regs.a,
            0x78 => self.regs.a = self.regs.b,
            0x79 => self.regs.a = self.regs.c,
            0x7a => self.regs.a = self.regs.d,
            0x7b => self.regs.a = self.regs.e,
            0x7c => self.regs.a = self.regs.h,
            0x7d => self.regs.a = self.regs.l,
            0x7f => {}
            //  ld   r,n         xx nn      8 ---- r=n
            0x06 => self.regs.b = self.imm(),
            0x0e => self.regs.c = self.imm(),
            0x16 => self.regs.d = self.imm(),
            0x1e => self.regs.e = self.imm(),
            0x26 => self.regs.h = self.imm(),
            0x2e => self.regs.l = self.imm(),
            0x3e => self.regs.a = self.imm(),
            //  ld   r,(HL)      xx         8 ---- r=(HL)
            0x46 => self.regs.b = self.bus.borrow().read_byte(self.regs.get_hl()),
            0x4e => self.regs.c = self.bus.borrow().read_byte(self.regs.get_hl()),
            0x56 => self.regs.d = self.bus.borrow().read_byte(self.regs.get_hl()),
            0x5e => self.regs.e = self.bus.borrow().read_byte(self.regs.get_hl()),
            0x66 => self.regs.h = self.bus.borrow().read_byte(self.regs.get_hl()),
            0x6e => self.regs.l = self.bus.borrow().read_byte(self.regs.get_hl()),
            0x7e => self.regs.a = self.bus.borrow().read_byte(self.regs.get_hl()),
            //  ld   (HL),r      7x         8 ---- (HL)=r
            0x70 => self
                .bus
                .borrow_mut()
                .write_byte(self.regs.get_hl(), self.regs.b),
            0x71 => self
                .bus
                .borrow_mut()
                .write_byte(self.regs.get_hl(), self.regs.c),
            0x72 => self
                .bus
                .borrow_mut()
                .write_byte(self.regs.get_hl(), self.regs.d),
            0x73 => self
                .bus
                .borrow_mut()
                .write_byte(self.regs.get_hl(), self.regs.e),
            0x74 => self
                .bus
                .borrow_mut()
                .write_byte(self.regs.get_hl(), self.regs.h),
            0x75 => self
                .bus
                .borrow_mut()
                .write_byte(self.regs.get_hl(), self.regs.l),
            0x77 => self
                .bus
                .borrow_mut()
                .write_byte(self.regs.get_hl(), self.regs.a),
            //  ld   (HL),n      36 nn     12 ----
            0x36 => {
                let v = self.imm();
                self.bus.borrow_mut().write_byte(self.regs.get_hl(), v);
            }
            //  ld   A,(BC)      0A         8 ----
            0x0a => self.regs.a = self.bus.borrow().read_byte(self.regs.get_bc()),
            //  ld   A,(DE)      1A         8 ----
            0x1a => self.regs.a = self.bus.borrow().read_byte(self.regs.get_de()),
            //  ld   A,(nn)      FA        16 ----
            0xfa => {
                let a = self.imm_word();
                self.regs.a = self.bus.borrow().read_byte(a);
            }
            //  ld   (BC),A      02         8 ----
            0x02 => self
                .bus
                .borrow_mut()
                .write_byte(self.regs.get_bc(), self.regs.a),
            //  ld   (DE),A      02         8 ----
            0x12 => self
                .bus
                .borrow_mut()
                .write_byte(self.regs.get_de(), self.regs.a),
            //  ld   (nn),A      EA        16 ----
            0xea => {
                let a = self.imm_word();
                self.bus.borrow_mut().write_byte(a, self.regs.a);
            }
            //  ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
            0xf0 => {
                let a = 0xff00 | self.imm() as u16;
                self.regs.a = self.bus.borrow().read_byte(a);
            }
            //  ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
            0xe0 => {
                let a = 0xff00 | self.imm() as u16;
                self.bus.borrow_mut().write_byte(a, self.regs.a);
            }
            //  ld   A,(FF00+C)  F2         8 ---- read from io-port C (memory FF00+C)
            0xf2 => self.regs.a = self.bus.borrow().read_byte(0xff00 | self.regs.c as u16),
            //  ld   (FF00+C),A  E2         8 ---- write to io-port C (memory FF00+C)
            0xe2 => self
                .bus
                .borrow_mut()
                .write_byte(0xff00 | self.regs.c as u16, self.regs.a),
            //  ldi  (HL),A      22         8 ---- (HL)=A, HL=HL+1
            0x22 => {
                let v = self.regs.get_hl();
                self.bus.borrow_mut().write_byte(v, self.regs.a);
                self.regs.set_hl(v + 1);
            }
            //  ldi  A,(HL)      2A         8 ---- A=(HL), HL=HL+1
            0x2a => {
                let v = self.regs.get_hl();
                self.regs.a = self.bus.borrow().read_byte(v);
                self.regs.set_hl(v + 1);
            }
            //  ldd  (HL),A      32         8 ---- (HL)=A, HL=HL-1
            0x32 => {
                let a = self.regs.get_hl();
                self.bus.borrow_mut().write_byte(a, self.regs.a);
                self.regs.set_hl(a - 1);
            }
            //  ldd  A,(HL)      3A         8 ---- A=(HL), HL=HL-1
            0x3a => {
                let v = self.regs.get_hl();
                self.regs.a = self.bus.borrow().read_byte(v);
                self.regs.set_hl(v - 1);
            }

            //-----GMB 16bit-Loadcommands-----
            //  ld   rr,nn       x1 nn nn  12 ---- rr=nn (rr may be BC,DE,HL or SP)
            0x01 | 0x11 | 0x21 | 0x31 => {
                let v = self.imm_word();
                match op {
                    0x01 => self.regs.set_bc(v),
                    0x11 => self.regs.set_de(v),
                    0x21 => self.regs.set_hl(v),
                    0x31 => self.regs.sp = v,
                    _ => {}
                }
            }
            // LD (d16), SP
            0x08 => {
                let a = self.imm_word();
                self.bus.borrow_mut().write_word(a, self.regs.sp);
            }
            //  ld   SP,HL       F9         8 ---- SP=HL
            0xf9 => self.regs.sp = self.regs.get_hl(),
            //  push rr          x5        16 ---- SP=SP-2  (SP)=rr   (rr may be BC,DE,HL,AF)
            0xc5 => self.stack_push(self.regs.get_bc()),
            0xd5 => self.stack_push(self.regs.get_de()),
            0xe5 => self.stack_push(self.regs.get_hl()),
            0xf5 => self.stack_push(self.regs.get_af()),
            //  pop  rr          x1        12 (AF) rr=(SP)  SP=SP+2   (rr may be BC,DE,HL,AF)
            0xc1 | 0xf1 | 0xd1 | 0xe1 => {
                let v = self.stack_pop();
                match op {
                    0xc1 => self.regs.set_bc(v),
                    0xd1 => self.regs.set_de(v),
                    0xe1 => self.regs.set_hl(v),
                    0xf1 => self.regs.set_af(v),
                    _ => {}
                }
            }

            //-----GMB 8bit-Arithmetic/logical Commands-----
            //  add  A,r         8x         4 z0hc A=A+r
            0x80 => self.alu_add(self.regs.b),
            0x81 => self.alu_add(self.regs.c),
            0x82 => self.alu_add(self.regs.d),
            0x83 => self.alu_add(self.regs.e),
            0x84 => self.alu_add(self.regs.h),
            0x85 => self.alu_add(self.regs.l),
            0x87 => self.alu_add(self.regs.a),
            //  add  A,n         C6 nn      8 z0hc A=A+n
            0xc6 => {
                let v = self.imm();
                self.alu_add(v);
            }
            //  add  A,(HL)      86         8 z0hc A=A+(HL)
            0x86 => {
                let a = self.bus.borrow().read_byte(self.regs.get_hl());
                self.alu_add(a);
            }
            //  adc  A,r         8x         4 z0hc A=A+r+cy
            0x88 => self.alu_adc(self.regs.b),
            0x89 => self.alu_adc(self.regs.c),
            0x8a => self.alu_adc(self.regs.d),
            0x8b => self.alu_adc(self.regs.e),
            0x8c => self.alu_adc(self.regs.h),
            0x8d => self.alu_adc(self.regs.l),
            0x8f => self.alu_adc(self.regs.a),
            //  adc  A,n         CE nn      8 z0hc A=A+n+cy
            0xce => {
                let v = self.imm();
                self.alu_adc(v);
            }
            //  adc  A,(HL)      8E         8 z0hc A=A+(HL)+cy
            0x8e => {
                let a = self.bus.borrow().read_byte(self.regs.get_hl());
                self.alu_adc(a);
            }
            //  sub  r           9x         4 z1hc A=A-r
            0x90 => self.alu_sub(self.regs.b),
            0x91 => self.alu_sub(self.regs.c),
            0x92 => self.alu_sub(self.regs.d),
            0x93 => self.alu_sub(self.regs.e),
            0x94 => self.alu_sub(self.regs.h),
            0x95 => self.alu_sub(self.regs.l),
            0x97 => self.alu_sub(self.regs.a),
            //  sub  n           D6 nn      8 z1hc A=A-n
            0xd6 => {
                let v = self.imm();
                self.alu_sub(v);
            }
            //  sub  (HL)        96         8 z1hc A=A-(HL)
            0x96 => {
                let a = self.bus.borrow().read_byte(self.regs.get_hl());
                self.alu_sub(a);
            }
            //  sbc  A,r         9x         4 z1hc A=A-r-cy
            0x98 => self.alu_sbc(self.regs.b),
            0x99 => self.alu_sbc(self.regs.c),
            0x9a => self.alu_sbc(self.regs.d),
            0x9b => self.alu_sbc(self.regs.e),
            0x9c => self.alu_sbc(self.regs.h),
            0x9d => self.alu_sbc(self.regs.l),
            0x9f => self.alu_sbc(self.regs.a),
            //  sbc  A,n         DE nn      8 z1hc A=A-n-cy
            0xde => {
                let v = self.imm();
                self.alu_sbc(v);
            }
            //  sbc  A,(HL)      9E         8 z1hc A=A-(HL)-cy
            0x9e => {
                let a = self.bus.borrow().read_byte(self.regs.get_hl());
                self.alu_sbc(a);
            }
            //  and  r           Ax         4 z010 A=A & r
            0xa0 => self.alu_and(self.regs.b),
            0xa1 => self.alu_and(self.regs.c),
            0xa2 => self.alu_and(self.regs.d),
            0xa3 => self.alu_and(self.regs.e),
            0xa4 => self.alu_and(self.regs.h),
            0xa5 => self.alu_and(self.regs.l),
            0xa7 => self.alu_and(self.regs.a),
            //  and  n           E6 nn      8 z010 A=A & n
            0xe6 => {
                let v = self.imm();
                self.alu_and(v);
            }
            //  and  (HL)        A6         8 z010 A=A & (HL)
            0xa6 => {
                let a = self.bus.borrow().read_byte(self.regs.get_hl());
                self.alu_and(a);
            }
            //  xor  r           Ax         4 z000
            0xa8 => self.alu_xor(self.regs.b),
            0xa9 => self.alu_xor(self.regs.c),
            0xaa => self.alu_xor(self.regs.d),
            0xab => self.alu_xor(self.regs.e),
            0xac => self.alu_xor(self.regs.h),
            0xad => self.alu_xor(self.regs.l),
            0xaf => self.alu_xor(self.regs.a),
            //  xor  n           EE nn      8 z000
            0xee => {
                let v = self.imm();
                self.alu_xor(v);
            }
            //  xor  (HL)        AE         8 z000
            0xae => {
                let a = self.bus.borrow().read_byte(self.regs.get_hl());
                self.alu_xor(a);
            }
            //  or   r           Bx         4 z000 A=A | r
            0xb0 => self.alu_or(self.regs.b),
            0xb1 => self.alu_or(self.regs.c),
            0xb2 => self.alu_or(self.regs.d),
            0xb3 => self.alu_or(self.regs.e),
            0xb4 => self.alu_or(self.regs.h),
            0xb5 => self.alu_or(self.regs.l),
            0xb7 => self.alu_or(self.regs.a),
            //  or   n           F6 nn      8 z000 A=A | n
            0xf6 => {
                let v = self.imm();
                self.alu_or(v);
            }
            //  or   (HL)        B6         8 z000 A=A | (HL)
            0xb6 => {
                let a = self.bus.borrow().read_byte(self.regs.get_hl());
                self.alu_or(a);
            }
            //  cp   r           Bx         4 z1hc compare A-r
            0xb8 => self.alu_cp(self.regs.b),
            0xb9 => self.alu_cp(self.regs.c),
            0xba => self.alu_cp(self.regs.d),
            0xbb => self.alu_cp(self.regs.e),
            0xbc => self.alu_cp(self.regs.h),
            0xbd => self.alu_cp(self.regs.l),
            0xbf => self.alu_cp(self.regs.a),
            //  cp   n           FE nn      8 z1hc compare A-n
            0xfe => {
                let v = self.imm();
                self.alu_cp(v);
            }
            //  cp   (HL)        BE         8 z1hc compare A-(HL)
            0xbe => {
                let a = self.bus.borrow().read_byte(self.regs.get_hl());
                self.alu_cp(a);
            }
            //  inc  r           xx         4 z0h- r=r+1
            0x04 => self.regs.b = self.alu_inc(self.regs.b),
            0x0c => self.regs.c = self.alu_inc(self.regs.c),
            0x14 => self.regs.d = self.alu_inc(self.regs.d),
            0x1c => self.regs.e = self.alu_inc(self.regs.e),
            0x24 => self.regs.h = self.alu_inc(self.regs.h),
            0x2c => self.regs.l = self.alu_inc(self.regs.l),
            0x3c => self.regs.a = self.alu_inc(self.regs.a),
            //  inc  (HL)        34        12 z0h- (HL)=(HL)+1
            0x34 => {
                let a = self.regs.get_hl();
                let v = self.bus.borrow().read_byte(a);
                let h = self.alu_inc(v);
                self.bus.borrow_mut().write_byte(a, h);
            }
            //  dec  r           xx         4 z1h- r=r-1
            0x05 => self.regs.b = self.alu_dec(self.regs.b),
            0x0d => self.regs.c = self.alu_dec(self.regs.c),
            0x15 => self.regs.d = self.alu_dec(self.regs.d),
            0x1d => self.regs.e = self.alu_dec(self.regs.e),
            0x25 => self.regs.h = self.alu_dec(self.regs.h),
            0x2d => self.regs.l = self.alu_dec(self.regs.l),
            0x3d => self.regs.a = self.alu_dec(self.regs.a),
            //  dec  (HL)        35        12 z1h- (HL)=(HL)-1
            0x35 => {
                let a = self.regs.get_hl();
                let v = self.bus.borrow().read_byte(a);
                let h = self.alu_dec(v);
                self.bus.borrow_mut().write_byte(a, h);
            }
            //  daa              27         4 z-0x decimal adjust akku
            0x27 => self.alu_daa(),
            //  cpl              2F         4 -11- A = A xor FF
            0x2f => self.alu_cpl(),

            //-----GMB 16bit-Arithmetic/logical Commands-----
            //  add  HL,rr     x9           8 -0hc HL = HL+rr     ;rr may be BC,DE,HL,SP
            0x09 => self.alu_add_hl(self.regs.get_bc()),
            0x19 => self.alu_add_hl(self.regs.get_de()),
            0x29 => self.alu_add_hl(self.regs.get_hl()),
            0x39 => self.alu_add_hl(self.regs.sp),
            //  inc  rr        x3           8 ---- rr = rr+1      ;rr may be BC,DE,HL,SP
            0x03 => self.regs.set_bc(self.regs.get_bc().wrapping_add(1)),
            0x13 => self.regs.set_de(self.regs.get_de().wrapping_add(1)),
            0x23 => self.regs.set_hl(self.regs.get_hl().wrapping_add(1)),
            0x33 => self.regs.sp = self.regs.sp.wrapping_add(1),
            //  dec  rr        xB           8 ---- rr = rr-1      ;rr may be BC,DE,HL,SP
            0x0b => self.regs.set_bc(self.regs.get_bc().wrapping_sub(1)),
            0x1b => self.regs.set_de(self.regs.get_de().wrapping_sub(1)),
            0x2b => self.regs.set_hl(self.regs.get_hl().wrapping_sub(1)),
            0x3b => self.regs.sp = self.regs.sp.wrapping_sub(1),
            //  add  SP,dd     E8          16 00hc SP = SP +/- dd ;dd is 8bit signed number
            0xe8 => self.alu_add_sp(),
            //  ld   HL,SP+dd  F8          12 00hc HL = SP +/- dd ;dd is 8bit signed number
            0xf8 => {
                let a = self.regs.sp;
                let b = i16::from(self.imm() as i8) as u16;
                self.regs
                    .set_flag(Flag::C, (a & 0x00ff) + (b & 0x00ff) > 0x00ff);
                self.regs
                    .set_flag(Flag::H, (a & 0x000f) + (b & 0x000f) > 0x000f);
                self.regs.set_flag(Flag::N, false);
                self.regs.set_flag(Flag::Z, false);
                self.regs.set_hl(a.wrapping_add(b));
            }
            //-----GMB CPU-Controlcommands-----
            //  ccf            3F           4 -00c cy=cy xor 1
            0x3f => self.alu_ccf(),
            //  scf            37           4 -001 cy=1
            0x37 => self.alu_scf(),
            //  nop            00           4 ---- no operation
            0x00 => {}
            //  halt           76         N*4 ---- halt until interrupt occurs (low power)
            0x76 => self.halted = true,
            //  stop           10 00        ? ---- low power standby mode (VERY low power)
            0x10 => {}
            //  di             F3           4 ---- disable interrupts, IME=0
            0xf3 => self.ime = false,
            //  ei             FB           4 ---- enable interrupts, IME=1
            0xfb => self.ime = true,

            //-----GMB Rotate- und Shift-Commands-----
            //  rlca           07           4 000c rotate akku left
            0x07 => {
                self.regs.a = self.alu_rlc(self.regs.a);
                self.regs.set_flag(Flag::Z, false);
            }
            //  rla            17           4 000c rotate akku left through carry
            0x17 => {
                self.regs.a = self.alu_rl(self.regs.a);
                self.regs.set_flag(Flag::Z, false);
            }
            //  rrca           0F           4 000c rotate akku right
            0x0f => {
                self.regs.a = self.alu_rrc(self.regs.a);
                self.regs.set_flag(Flag::Z, false);
            }
            //  rra            1F           4 000c rotate akku right through carry
            0x1f => {
                self.regs.a = self.alu_rr(self.regs.a);
                self.regs.set_flag(Flag::Z, false);
            }
            // Extended Bit Operations
            0xcb => {
                cb = self.imm();
                match cb {
                    //  rlca           07           4 000c rotate akku left
                    0x07 => self.regs.a = self.alu_rlc(self.regs.a),
                    //  rla            17           4 000c rotate akku left through carry
                    0x17 => self.regs.a = self.alu_rl(self.regs.a),
                    //  rrca           0F           4 000c rotate akku right
                    0x0f => self.regs.a = self.alu_rrc(self.regs.a),
                    //  rra            1F           4 000c rotate akku right through carry
                    0x1f => self.regs.a = self.alu_rr(self.regs.a),
                    //  rlc  r         CB 0x        8 z00c rotate left
                    0x00 => self.regs.b = self.alu_rlc(self.regs.b),
                    0x01 => self.regs.c = self.alu_rlc(self.regs.c),
                    0x02 => self.regs.d = self.alu_rlc(self.regs.d),
                    0x03 => self.regs.e = self.alu_rlc(self.regs.e),
                    0x04 => self.regs.h = self.alu_rlc(self.regs.h),
                    0x05 => self.regs.l = self.alu_rlc(self.regs.l),
                    //  rlc  (HL)      CB 06       16 z00c rotate left
                    0x06 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_rlc(v);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    //  rl   r         CB 1x        8 z00c rotate left through carry
                    0x10 => self.regs.b = self.alu_rl(self.regs.b),
                    0x11 => self.regs.c = self.alu_rl(self.regs.c),
                    0x12 => self.regs.d = self.alu_rl(self.regs.d),
                    0x13 => self.regs.e = self.alu_rl(self.regs.e),
                    0x14 => self.regs.h = self.alu_rl(self.regs.h),
                    0x15 => self.regs.l = self.alu_rl(self.regs.l),
                    //  rl   (HL)      CB 16       16 z00c rotate left through carry
                    0x16 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_rl(v);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    //  rrc  r         CB 0x        8 z00c rotate right
                    0x08 => self.regs.b = self.alu_rrc(self.regs.b),
                    0x09 => self.regs.c = self.alu_rrc(self.regs.c),
                    0x0a => self.regs.d = self.alu_rrc(self.regs.d),
                    0x0b => self.regs.e = self.alu_rrc(self.regs.e),
                    0x0c => self.regs.h = self.alu_rrc(self.regs.h),
                    0x0d => self.regs.l = self.alu_rrc(self.regs.l),
                    //  rrc  (HL)      CB 0E       16 z00c rotate right
                    0x0e => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_rrc(v);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    //  rr   r         CB 1x        8 z00c rotate right through carry
                    0x18 => self.regs.b = self.alu_rr(self.regs.b),
                    0x19 => self.regs.c = self.alu_rr(self.regs.c),
                    0x1a => self.regs.d = self.alu_rr(self.regs.d),
                    0x1b => self.regs.e = self.alu_rr(self.regs.e),
                    0x1c => self.regs.h = self.alu_rr(self.regs.h),
                    0x1d => self.regs.l = self.alu_rr(self.regs.l),
                    //  rr   (HL)      CB 1E       16 z00c rotate right through carry
                    0x1e => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_rr(v);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    //  sla  r         CB 2x        8 z00c shift left arithmetic (b0=0)
                    0x20 => self.regs.b = self.alu_sla(self.regs.b),
                    0x21 => self.regs.c = self.alu_sla(self.regs.c),
                    0x22 => self.regs.d = self.alu_sla(self.regs.d),
                    0x23 => self.regs.e = self.alu_sla(self.regs.e),
                    0x24 => self.regs.h = self.alu_sla(self.regs.h),
                    0x25 => self.regs.l = self.alu_sla(self.regs.l),
                    0x27 => self.regs.a = self.alu_sla(self.regs.a),
                    //  sla  (HL)      CB 26       16 z00c shift left arithmetic (b0=0)
                    0x26 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_sla(v);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    //  swap r         CB 3x        8 z000 exchange low/hi-nibble
                    0x30 => self.regs.b = self.alu_swap(self.regs.b),
                    0x31 => self.regs.c = self.alu_swap(self.regs.c),
                    0x32 => self.regs.d = self.alu_swap(self.regs.d),
                    0x33 => self.regs.e = self.alu_swap(self.regs.e),
                    0x34 => self.regs.h = self.alu_swap(self.regs.h),
                    0x35 => self.regs.l = self.alu_swap(self.regs.l),
                    0x37 => self.regs.a = self.alu_swap(self.regs.a),
                    //  swap (HL)      CB 36       16 z000 exchange low/hi-nibble
                    0x36 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_swap(v);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    //  sra  r         CB 2x        8 z00c shift right arithmetic (b7=b7)
                    0x28 => self.regs.b = self.alu_sra(self.regs.b),
                    0x29 => self.regs.c = self.alu_sra(self.regs.c),
                    0x2a => self.regs.d = self.alu_sra(self.regs.d),
                    0x2b => self.regs.e = self.alu_sra(self.regs.e),
                    0x2c => self.regs.h = self.alu_sra(self.regs.h),
                    0x2d => self.regs.l = self.alu_sra(self.regs.l),
                    0x2f => self.regs.a = self.alu_sra(self.regs.a),
                    //  sra  (HL)      CB 2E       16 z00c shift right arithmetic (b7=b7)
                    0x2e => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_sra(v);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    //  srl  r         CB 3x        8 z00c shift right logical (b7=0)
                    0x38 => self.regs.b = self.alu_srl(self.regs.b),
                    0x39 => self.regs.c = self.alu_srl(self.regs.c),
                    0x3a => self.regs.d = self.alu_srl(self.regs.d),
                    0x3b => self.regs.e = self.alu_srl(self.regs.e),
                    0x3c => self.regs.h = self.alu_srl(self.regs.h),
                    0x3d => self.regs.l = self.alu_srl(self.regs.l),
                    0x3f => self.regs.a = self.alu_srl(self.regs.a),
                    //  srl  (HL)      CB 3E       16 z00c shift right logical (b7=0)
                    0x3e => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_srl(v);
                        self.bus.borrow_mut().write_byte(a, h);
                    }

                    //-----GMB Singlebit Operation Commands-----
                    //  bit  n,r       CB xx        8 z01- test bit n
                    //  bit  n,(HL)    CB xx       12 z01- test bit n
                    //  set  n,r       CB xx        8 ---- set bit n
                    //  set  n,(HL)    CB xx       16 ---- set bit n
                    //  res  n,r       CB xx        8 ---- reset bit n
                    //  res  n,(HL)    CB xx       16 ---- reset bit n

                    // BIT b, r8
                    0x40 => self.alu_bit(self.regs.b, 0),
                    0x41 => self.alu_bit(self.regs.c, 0),
                    0x42 => self.alu_bit(self.regs.d, 0),
                    0x43 => self.alu_bit(self.regs.e, 0),
                    0x44 => self.alu_bit(self.regs.h, 0),
                    0x45 => self.alu_bit(self.regs.l, 0),
                    0x46 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        self.alu_bit(v, 0);
                    }
                    0x47 => self.alu_bit(self.regs.a, 0),
                    0x48 => self.alu_bit(self.regs.b, 1),
                    0x49 => self.alu_bit(self.regs.c, 1),
                    0x4a => self.alu_bit(self.regs.d, 1),
                    0x4b => self.alu_bit(self.regs.e, 1),
                    0x4c => self.alu_bit(self.regs.h, 1),
                    0x4d => self.alu_bit(self.regs.l, 1),
                    0x4e => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        self.alu_bit(v, 1);
                    }
                    0x4f => self.alu_bit(self.regs.a, 1),
                    0x50 => self.alu_bit(self.regs.b, 2),
                    0x51 => self.alu_bit(self.regs.c, 2),
                    0x52 => self.alu_bit(self.regs.d, 2),
                    0x53 => self.alu_bit(self.regs.e, 2),
                    0x54 => self.alu_bit(self.regs.h, 2),
                    0x55 => self.alu_bit(self.regs.l, 2),
                    0x56 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        self.alu_bit(v, 2);
                    }
                    0x57 => self.alu_bit(self.regs.a, 2),
                    0x58 => self.alu_bit(self.regs.b, 3),
                    0x59 => self.alu_bit(self.regs.c, 3),
                    0x5a => self.alu_bit(self.regs.d, 3),
                    0x5b => self.alu_bit(self.regs.e, 3),
                    0x5c => self.alu_bit(self.regs.h, 3),
                    0x5d => self.alu_bit(self.regs.l, 3),
                    0x5e => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        self.alu_bit(v, 3);
                    }
                    0x5f => self.alu_bit(self.regs.a, 3),
                    0x60 => self.alu_bit(self.regs.b, 4),
                    0x61 => self.alu_bit(self.regs.c, 4),
                    0x62 => self.alu_bit(self.regs.d, 4),
                    0x63 => self.alu_bit(self.regs.e, 4),
                    0x64 => self.alu_bit(self.regs.h, 4),
                    0x65 => self.alu_bit(self.regs.l, 4),
                    0x66 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        self.alu_bit(v, 4);
                    }
                    0x67 => self.alu_bit(self.regs.a, 4),
                    0x68 => self.alu_bit(self.regs.b, 5),
                    0x69 => self.alu_bit(self.regs.c, 5),
                    0x6a => self.alu_bit(self.regs.d, 5),
                    0x6b => self.alu_bit(self.regs.e, 5),
                    0x6c => self.alu_bit(self.regs.h, 5),
                    0x6d => self.alu_bit(self.regs.l, 5),
                    0x6e => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        self.alu_bit(v, 5);
                    }
                    0x6f => self.alu_bit(self.regs.a, 5),
                    0x70 => self.alu_bit(self.regs.b, 6),
                    0x71 => self.alu_bit(self.regs.c, 6),
                    0x72 => self.alu_bit(self.regs.d, 6),
                    0x73 => self.alu_bit(self.regs.e, 6),
                    0x74 => self.alu_bit(self.regs.h, 6),
                    0x75 => self.alu_bit(self.regs.l, 6),
                    0x76 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        self.alu_bit(v, 6);
                    }
                    0x77 => self.alu_bit(self.regs.a, 6),
                    0x78 => self.alu_bit(self.regs.b, 7),
                    0x79 => self.alu_bit(self.regs.c, 7),
                    0x7a => self.alu_bit(self.regs.d, 7),
                    0x7b => self.alu_bit(self.regs.e, 7),
                    0x7c => self.alu_bit(self.regs.h, 7),
                    0x7d => self.alu_bit(self.regs.l, 7),
                    0x7e => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        self.alu_bit(v, 7);
                    }
                    0x7f => self.alu_bit(self.regs.a, 7),

                    // RES b, r8
                    0x80 => self.regs.b = self.alu_res(self.regs.b, 0),
                    0x81 => self.regs.c = self.alu_res(self.regs.c, 0),
                    0x82 => self.regs.d = self.alu_res(self.regs.d, 0),
                    0x83 => self.regs.e = self.alu_res(self.regs.e, 0),
                    0x84 => self.regs.h = self.alu_res(self.regs.h, 0),
                    0x85 => self.regs.l = self.alu_res(self.regs.l, 0),
                    0x86 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_res(v, 0);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0x87 => self.regs.a = self.alu_res(self.regs.a, 0),
                    0x88 => self.regs.b = self.alu_res(self.regs.b, 1),
                    0x89 => self.regs.c = self.alu_res(self.regs.c, 1),
                    0x8a => self.regs.d = self.alu_res(self.regs.d, 1),
                    0x8b => self.regs.e = self.alu_res(self.regs.e, 1),
                    0x8c => self.regs.h = self.alu_res(self.regs.h, 1),
                    0x8d => self.regs.l = self.alu_res(self.regs.l, 1),
                    0x8e => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_res(v, 1);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0x8f => self.regs.a = self.alu_res(self.regs.a, 1),
                    0x90 => self.regs.b = self.alu_res(self.regs.b, 2),
                    0x91 => self.regs.c = self.alu_res(self.regs.c, 2),
                    0x92 => self.regs.d = self.alu_res(self.regs.d, 2),
                    0x93 => self.regs.e = self.alu_res(self.regs.e, 2),
                    0x94 => self.regs.h = self.alu_res(self.regs.h, 2),
                    0x95 => self.regs.l = self.alu_res(self.regs.l, 2),
                    0x96 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_res(v, 2);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0x97 => self.regs.a = self.alu_res(self.regs.a, 2),
                    0x98 => self.regs.b = self.alu_res(self.regs.b, 3),
                    0x99 => self.regs.c = self.alu_res(self.regs.c, 3),
                    0x9a => self.regs.d = self.alu_res(self.regs.d, 3),
                    0x9b => self.regs.e = self.alu_res(self.regs.e, 3),
                    0x9c => self.regs.h = self.alu_res(self.regs.h, 3),
                    0x9d => self.regs.l = self.alu_res(self.regs.l, 3),
                    0x9e => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_res(v, 3);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0x9f => self.regs.a = self.alu_res(self.regs.a, 3),
                    0xa0 => self.regs.b = self.alu_res(self.regs.b, 4),
                    0xa1 => self.regs.c = self.alu_res(self.regs.c, 4),
                    0xa2 => self.regs.d = self.alu_res(self.regs.d, 4),
                    0xa3 => self.regs.e = self.alu_res(self.regs.e, 4),
                    0xa4 => self.regs.h = self.alu_res(self.regs.h, 4),
                    0xa5 => self.regs.l = self.alu_res(self.regs.l, 4),
                    0xa6 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_res(v, 4);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0xa7 => self.regs.a = self.alu_res(self.regs.a, 4),
                    0xa8 => self.regs.b = self.alu_res(self.regs.b, 5),
                    0xa9 => self.regs.c = self.alu_res(self.regs.c, 5),
                    0xaa => self.regs.d = self.alu_res(self.regs.d, 5),
                    0xab => self.regs.e = self.alu_res(self.regs.e, 5),
                    0xac => self.regs.h = self.alu_res(self.regs.h, 5),
                    0xad => self.regs.l = self.alu_res(self.regs.l, 5),
                    0xae => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_res(v, 5);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0xaf => self.regs.a = self.alu_res(self.regs.a, 5),
                    0xb0 => self.regs.b = self.alu_res(self.regs.b, 6),
                    0xb1 => self.regs.c = self.alu_res(self.regs.c, 6),
                    0xb2 => self.regs.d = self.alu_res(self.regs.d, 6),
                    0xb3 => self.regs.e = self.alu_res(self.regs.e, 6),
                    0xb4 => self.regs.h = self.alu_res(self.regs.h, 6),
                    0xb5 => self.regs.l = self.alu_res(self.regs.l, 6),
                    0xb6 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_res(v, 6);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0xb7 => self.regs.a = self.alu_res(self.regs.a, 6),
                    0xb8 => self.regs.b = self.alu_res(self.regs.b, 7),
                    0xb9 => self.regs.c = self.alu_res(self.regs.c, 7),
                    0xba => self.regs.d = self.alu_res(self.regs.d, 7),
                    0xbb => self.regs.e = self.alu_res(self.regs.e, 7),
                    0xbc => self.regs.h = self.alu_res(self.regs.h, 7),
                    0xbd => self.regs.l = self.alu_res(self.regs.l, 7),
                    0xbe => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_res(v, 7);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0xbf => self.regs.a = self.alu_res(self.regs.a, 7),

                    // SET b, r8
                    0xc0 => self.regs.b = self.alu_set(self.regs.b, 0),
                    0xc1 => self.regs.c = self.alu_set(self.regs.c, 0),
                    0xc2 => self.regs.d = self.alu_set(self.regs.d, 0),
                    0xc3 => self.regs.e = self.alu_set(self.regs.e, 0),
                    0xc4 => self.regs.h = self.alu_set(self.regs.h, 0),
                    0xc5 => self.regs.l = self.alu_set(self.regs.l, 0),
                    0xc6 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_set(v, 0);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0xc7 => self.regs.a = self.alu_set(self.regs.a, 0),
                    0xc8 => self.regs.b = self.alu_set(self.regs.b, 1),
                    0xc9 => self.regs.c = self.alu_set(self.regs.c, 1),
                    0xca => self.regs.d = self.alu_set(self.regs.d, 1),
                    0xcb => self.regs.e = self.alu_set(self.regs.e, 1),
                    0xcc => self.regs.h = self.alu_set(self.regs.h, 1),
                    0xcd => self.regs.l = self.alu_set(self.regs.l, 1),
                    0xce => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_set(v, 1);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0xcf => self.regs.a = self.alu_set(self.regs.a, 1),
                    0xd0 => self.regs.b = self.alu_set(self.regs.b, 2),
                    0xd1 => self.regs.c = self.alu_set(self.regs.c, 2),
                    0xd2 => self.regs.d = self.alu_set(self.regs.d, 2),
                    0xd3 => self.regs.e = self.alu_set(self.regs.e, 2),
                    0xd4 => self.regs.h = self.alu_set(self.regs.h, 2),
                    0xd5 => self.regs.l = self.alu_set(self.regs.l, 2),
                    0xd6 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_set(v, 2);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0xd7 => self.regs.a = self.alu_set(self.regs.a, 2),
                    0xd8 => self.regs.b = self.alu_set(self.regs.b, 3),
                    0xd9 => self.regs.c = self.alu_set(self.regs.c, 3),
                    0xda => self.regs.d = self.alu_set(self.regs.d, 3),
                    0xdb => self.regs.e = self.alu_set(self.regs.e, 3),
                    0xdc => self.regs.h = self.alu_set(self.regs.h, 3),
                    0xdd => self.regs.l = self.alu_set(self.regs.l, 3),
                    0xde => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_set(v, 3);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0xdf => self.regs.a = self.alu_set(self.regs.a, 3),
                    0xe0 => self.regs.b = self.alu_set(self.regs.b, 4),
                    0xe1 => self.regs.c = self.alu_set(self.regs.c, 4),
                    0xe2 => self.regs.d = self.alu_set(self.regs.d, 4),
                    0xe3 => self.regs.e = self.alu_set(self.regs.e, 4),
                    0xe4 => self.regs.h = self.alu_set(self.regs.h, 4),
                    0xe5 => self.regs.l = self.alu_set(self.regs.l, 4),
                    0xe6 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_set(v, 4);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0xe7 => self.regs.a = self.alu_set(self.regs.a, 4),
                    0xe8 => self.regs.b = self.alu_set(self.regs.b, 5),
                    0xe9 => self.regs.c = self.alu_set(self.regs.c, 5),
                    0xea => self.regs.d = self.alu_set(self.regs.d, 5),
                    0xeb => self.regs.e = self.alu_set(self.regs.e, 5),
                    0xec => self.regs.h = self.alu_set(self.regs.h, 5),
                    0xed => self.regs.l = self.alu_set(self.regs.l, 5),
                    0xee => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_set(v, 5);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0xef => self.regs.a = self.alu_set(self.regs.a, 5),
                    0xf0 => self.regs.b = self.alu_set(self.regs.b, 6),
                    0xf1 => self.regs.c = self.alu_set(self.regs.c, 6),
                    0xf2 => self.regs.d = self.alu_set(self.regs.d, 6),
                    0xf3 => self.regs.e = self.alu_set(self.regs.e, 6),
                    0xf4 => self.regs.h = self.alu_set(self.regs.h, 6),
                    0xf5 => self.regs.l = self.alu_set(self.regs.l, 6),
                    0xf6 => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_set(v, 6);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0xf7 => self.regs.a = self.alu_set(self.regs.a, 6),
                    0xf8 => self.regs.b = self.alu_set(self.regs.b, 7),
                    0xf9 => self.regs.c = self.alu_set(self.regs.c, 7),
                    0xfa => self.regs.d = self.alu_set(self.regs.d, 7),
                    0xfb => self.regs.e = self.alu_set(self.regs.e, 7),
                    0xfc => self.regs.h = self.alu_set(self.regs.h, 7),
                    0xfd => self.regs.l = self.alu_set(self.regs.l, 7),
                    0xfe => {
                        let a = self.regs.get_hl();
                        let v = self.bus.borrow().read_byte(a);
                        let h = self.alu_set(v, 7);
                        self.bus.borrow_mut().write_byte(a, h);
                    }
                    0xff => self.regs.a = self.alu_set(self.regs.a, 7),
                }
            }
            //-----GMB Jumpcommands-----
            //  jp   nn        C3 nn nn    16 ---- jump to nn, PC=nn
            0xc3 => self.regs.pc = self.imm_word(),
            //  jp   HL        E9           4 ---- jump to HL, PC=HL
            0xe9 => self.regs.pc = self.regs.get_hl(),
            //  jp   f,nn      xx nn nn 16;12 ---- conditional jump if nz,z,nc,c
            0xc2 => {
                let pc=self.imm_word();
                if !self.regs.get_flag(Flag::Z) {
                    self.regs.pc = pc;
                }
            }
            0xca => {
                let pc=self.imm_word();
                if self.regs.get_flag(Flag::Z) {
                    self.regs.pc = pc;
                }
            }
            0xd2 => {
                let pc=self.imm_word();
                if !self.regs.get_flag(Flag::C) {
                    self.regs.pc = pc;
                }
            }
            0xda => {
                let pc=self.imm_word();
                if self.regs.get_flag(Flag::C) {
                    self.regs.pc = pc;
                }
            }
            //  jr   PC+dd     18 dd       12 ---- relative jump to nn (PC=PC+/-7bit)
            0x18 => {
                let n = self.imm();
                self.alu_jr(n);
            }
            //  jr   f,PC+dd   xx dd     12;8 ---- conditional relative jump if nz,z,nc,c
            0x20 => {
                let a = self.imm();
                if !self.regs.get_flag(Flag::Z) {
                    self.alu_jr(a);
                }
            }
            0x28 => {
                let a = self.imm();
                if self.regs.get_flag(Flag::Z) {
                    self.alu_jr(a);
                }
            }
            0x30 => {
                let a = self.imm();
                if !self.regs.get_flag(Flag::C) {
                    self.alu_jr(a);
                }
            }
            0x38 => {
                let a = self.imm();
                if self.regs.get_flag(Flag::C) {
                    self.alu_jr(a);
                }
            }
            //  call nn        CD nn nn    24 ---- call to nn, SP=SP-2, (SP)=PC, PC=nn
            0xcd => {
                let nn = self.imm_word();
                self.stack_push(self.regs.pc);
                self.regs.pc = nn;
            }
            //  call f,nn      xx nn nn 24;12 ---- conditional call if nz,z,nc,c
            0xc4 => {
                let a = self.imm_word();
                if !self.regs.get_flag(Flag::Z) {
                    self.stack_push(self.regs.pc);
                    self.regs.pc = a;
                }
            }
            0xcc => {
                let a = self.imm_word();
                if self.regs.get_flag(Flag::Z) {
                    self.stack_push(self.regs.pc);
                    self.regs.pc = a;
                }
            }
            0xd4 => {
                let a = self.imm_word();
                if !self.regs.get_flag(Flag::C) {
                    self.stack_push(self.regs.pc);
                    self.regs.pc = a;
                }
            }
            0xdc => {
                let a = self.imm_word();
                if self.regs.get_flag(Flag::C) {
                    self.stack_push(self.regs.pc);
                    self.regs.pc = a;
                }
            }
            //  ret            C9          16 ---- return, PC=(SP), SP=SP+2
            0xc9 => self.regs.pc = self.stack_pop(),
            //  ret  f         xx        20;8 ---- conditional return if nz,z,nc,c
            0xc0 => {
                if !self.regs.get_flag(Flag::Z) {
                    self.regs.pc = self.stack_pop();
                }
            }
            0xc8 => {
                if self.regs.get_flag(Flag::Z) {
                    self.regs.pc = self.stack_pop();
                }
            }
            0xd0 => {
                if !self.regs.get_flag(Flag::C) {
                    self.regs.pc = self.stack_pop();
                }
            }
            0xd8 => {
                if self.regs.get_flag(Flag::C) {
                    self.regs.pc = self.stack_pop();
                }
            }
            //  reti           D9          16 ---- return and enable interrupts (IME=1)
            0xd9 => {
                self.regs.pc = self.stack_pop();
                self.ime = true;
            }
            //  rst  n         xx          16 ---- call to 00,08,10,18,20,28,30,38
            0xc7 => {
                self.stack_push(self.regs.pc);
                self.regs.pc = 0x00;
            }
            0xcf => {
                self.stack_push(self.regs.pc);
                self.regs.pc = 0x08;
            }
            0xd7 => {
                self.stack_push(self.regs.pc);
                self.regs.pc = 0x10;
            }
            0xdf => {
                self.stack_push(self.regs.pc);
                self.regs.pc = 0x18;
            }
            0xe7 => {
                self.stack_push(self.regs.pc);
                self.regs.pc = 0x20;
            }
            0xef => {
                self.stack_push(self.regs.pc);
                self.regs.pc = 0x28;
            }
            0xf7 => {
                self.stack_push(self.regs.pc);
                self.regs.pc = 0x30;
            }
            0xff => {
                self.stack_push(self.regs.pc);
                self.regs.pc = 0x38;
            }

            /*
            Opcode  Z80             GMB
            ---------------------------------------
            08      EX   AF,AF      LD   (nn),SP
            10      DJNZ PC+dd      STOP
            22      LD   (nn),HL    LDI  (HL),A
            2A      LD   HL,(nn)    LDI  A,(HL)
            32      LD   (nn),A     LDD  (HL),A
            3A      LD   A,(nn)     LDD  A,(HL)
            D3      OUT  (n),A      -
            D9      EXX             RETI
            DB      IN   A,(n)      -
            DD      <IX>            -
            E0      RET  PO         LD   (FF00+n),A
            E2      JP   PO,nn      LD   (FF00+C),A
            E3      EX   (SP),HL    -
            E4      CALL P0,nn      -
            E8      RET  PE         ADD  SP,dd
            EA      JP   PE,nn      LD   (nn),A
            EB      EX   DE,HL      -
            EC      CALL PE,nn      -
            ED      <pref>          -
            F0      RET  P          LD   A,(FF00+n)
            F2      JP   P,nn       LD   A,(FF00+C)
            F4      CALL P,nn       -
            F8      RET  M          LD   HL,SP+dd
            FA      JP   M,nn       LD   A,(nn)
            FC      CALL M,nn       -
            FD      <IY>            -
            CB3X    SLL  r/(HL)     SWAP r/(HL)

            注意：未使用的 （-） 操作码将在使用时锁定 gameboy CPU。
             */
            0xd3 => panic!("Opcode 0xd3 is not implemented"),
            0xdb => panic!("Opcode 0xdb is not implemented"),
            0xdd => panic!("Opcode 0xdd is not implemented"),
            0xe3 => panic!("Opcode 0xe3 is not implemented"),
            0xe4 => panic!("Opcode 0xd4 is not implemented"),
            0xeb => panic!("Opcode 0xeb is not implemented"),
            0xec => panic!("Opcode 0xec is not implemented"),
            0xed => panic!("Opcode 0xed is not implemented"),
            0xf4 => panic!("Opcode 0xf4 is not implemented"),
            0xfc => panic!("Opcode 0xfc is not implemented"),
            0xfd => panic!("Opcode 0xfd is not implemented"),
            // _ => warn!("Operation {} Not Found!", op),
        }
        //判断跳转指令在跳转时会多一个Machine cycle
        let ecycle = match op {
            0x20 | 0x30 => {
                if self.regs.get_flag(Flag::Z) {
                    0x00
                } else {
                    0x01
                }
            }
            0x28 | 0x38 => {
                if self.regs.get_flag(Flag::Z) {
                    0x01
                } else {
                    0x00
                }
            }
            0xc0 | 0xd0 => {
                if self.regs.get_flag(Flag::Z) {
                    0x00
                } else {
                    0x03
                }
            }
            0xc8 | 0xcc | 0xd8 | 0xdc => {
                if self.regs.get_flag(Flag::Z) {
                    0x03
                } else {
                    0x00
                }
            }
            0xc2 | 0xd2 => {
                if self.regs.get_flag(Flag::Z) {
                    0x00
                } else {
                    0x01
                }
            }
            0xca | 0xda => {
                if self.regs.get_flag(Flag::Z) {
                    0x01
                } else {
                    0x00
                }
            }
            0xc4 | 0xd4 => {
                if self.regs.get_flag(Flag::Z) {
                    0x00
                } else {
                    0x03
                }
            }
            _ => 0x00,
        };
        if op == 0xcb {
            CB_CYCLES[cb as usize]
        } else {
            OP_CYCLES[op as usize] + ecycle
        }
    }

    /// 读取当前pc位置的一个字节，并将pc+1
    fn imm(&mut self) -> u8 {
        let i = self.bus.borrow().read_byte(self.regs.pc);
        self.regs.pc += 1;
        i
    }
    /// 读取当前pc位置的2个字节，并将pc+2
    fn imm_word(&mut self) -> u16 {
        let i = self.bus.borrow().read_word(self.regs.pc);
        self.regs.pc += 2;
        i
    }

    fn stack_push(&mut self, v: u16) {
        self.regs.sp -= 2;
        self.bus.borrow_mut().write_word(self.regs.sp, v);
    }

    fn stack_pop(&mut self) -> u16 {
        let i = self.bus.borrow().read_word(self.regs.sp);
        self.regs.sp += 2;
        i
    }
    /// Add n to A.
    /// n = A,B,C,D,E,H,L,(HL),#
    fn alu_add(&mut self, n: u8) {
        let a = self.regs.a;
        let r = a.wrapping_add(n);
        self.regs
            .set_flag(Flag::C, u16::from(a) + u16::from(n) > 0xff);
        self.regs.set_flag(Flag::H, (a & 0x0f) + (n & 0x0f) > 0x0f);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, r == 0x00);
        self.regs.a = r;
    }
    /// Add n + Carry flag to A.
    /// n = A,B,C,D,E,H,L,(HL),#
    fn alu_adc(&mut self, n: u8) {
        let a = self.regs.a;
        let c = self.regs.get_flag(Flag::C) as u8;
        let r = a.wrapping_add(n).wrapping_add(c);
        self.regs
            .set_flag(Flag::C, u16::from(a) + u16::from(n) + u16::from(c) > 0xff);
        self.regs
            .set_flag(Flag::H, (a & 0x0f) + (n & 0x0f) + (c & 0x0f) > 0x0f);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, r == 0x00);
        self.regs.a = r;
    }

    /// Subtract n from A.
    /// n = A,B,C,D,E,H,L,(HL),#
    fn alu_sub(&mut self, n: u8) {
        let a = self.regs.a;
        let r = a.wrapping_sub(n);
        self.regs.set_flag(Flag::C, a < n);
        self.regs.set_flag(Flag::H, (a & 0x0f) < (n & 0x0f));
        self.regs.set_flag(Flag::N, true);
        self.regs.set_flag(Flag::Z, r == 0x00);
        self.regs.a = r;
    }
    /// Subtract n + Carry flag from A.
    /// n = A,B,C,D,E,H,L,(HL),#
    fn alu_sbc(&mut self, n: u8) {
        let a = self.regs.a;
        let c = self.regs.get_flag(Flag::C) as u8;
        let r = a.wrapping_sub(n).wrapping_sub(c);
        self.regs
            .set_flag(Flag::C, u16::from(a) < u16::from(n) + u16::from(c));
        self.regs.set_flag(Flag::H, (a & 0x0f) < (n & 0x0f) + c);
        self.regs.set_flag(Flag::N, true);
        self.regs.set_flag(Flag::Z, r == 0x00);
        self.regs.a = r;
    }
    /// 将n与寄存器a的值逻辑与，结果存放在寄存器a中
    /// n = A,B,C,D,E,H,L,(HL),#
    fn alu_and(&mut self, n: u8) {
        let r = self.regs.a & n;
        self.regs.set_flag(Flag::C, false);
        self.regs.set_flag(Flag::H, true);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, r == 0x00);
        self.regs.a = r;
    }
    /// 将n与寄存器a的值逻辑异或，结果存放在寄存器a中
    /// n = A,B,C,D,E,H,L,(HL),#
    fn alu_xor(&mut self, n: u8) {
        let r = self.regs.a ^ n;
        self.regs.set_flag(Flag::C, false);
        self.regs.set_flag(Flag::H, false);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, r == 0x00);
        self.regs.a = r;
    }
    /// 将n与寄存器a的值逻辑或，结果存放在寄存器a中
    /// n = A,B,C,D,E,H,L,(HL),#
    fn alu_or(&mut self, n: u8) {
        let r = self.regs.a | n;
        self.regs.set_flag(Flag::C, false);
        self.regs.set_flag(Flag::H, false);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, r == 0x00);
        self.regs.a = r;
    }
    /// 比较n与寄存器a中的值 实际上是a-n 结果被丢弃
    /// n = A,B,C,D,E,H,L,(HL),#
    fn alu_cp(&mut self, n: u8) {
        let r = self.regs.a;
        self.alu_sub(n);
        self.regs.a = r;
    }

    /// 递增寄存器 n.
    /// n = A,B,C,D,E,H,L,(HL)
    fn alu_inc(&mut self, a: u8) -> u8 {
        let r = a.wrapping_add(1);
        self.regs.set_flag(Flag::H, (a & 0x0f) + 0x01 > 0x0f);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, r == 0x00);
        r
    }
    /// 递减寄存器 n.
    /// n = A,B,C,D,E,H,L,(HL)
    fn alu_dec(&mut self, a: u8) -> u8 {
        let r = a.wrapping_sub(1);
        self.regs.set_flag(Flag::H, a.trailing_zeros() >= 4);
        self.regs.set_flag(Flag::N, true);
        self.regs.set_flag(Flag::Z, r == 0);
        r
    }

    /// Add n to HL
    /// n = BC,DE,HL,SP
    fn alu_add_hl(&mut self, n: u16) {
        let a = self.regs.get_hl();
        let r = a.wrapping_add(n);
        self.regs.set_flag(Flag::C, a > 0xffff - n);
        self.regs
            .set_flag(Flag::H, (a & 0x0fff) + (n & 0x0fff) > 0x0fff);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_hl(r);
    }

    /// SP = SP +/- n.
    /// n是一个字节的有符号整数i8.
    fn alu_add_sp(&mut self) {
        let a = self.regs.sp;
        let b = i16::from(self.imm() as i8) as u16;
        self.regs
            .set_flag(Flag::C, (a & 0x00ff) + (b & 0x00ff) > 0x00ff);
        self.regs
            .set_flag(Flag::H, (a & 0x000f) + (b & 0x000f) > 0x000f);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, false);
        self.regs.sp = a.wrapping_add(b);
    }

    /// Swap upper & lower nibles of n.
    /// n = A,B,C,D,E,H,L,(HL)
    fn alu_swap(&mut self, a: u8) -> u8 {
        self.regs.set_flag(Flag::C, false);
        self.regs.set_flag(Flag::H, false);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, a == 0x00);
        (a >> 4) | (a << 4)
    }

    /// Decimal adjust registerister A. This instruction adjusts registerister A so that the correct representation of Binary
    /// Coded Decimal (BCD) is obtained.
    fn alu_daa(&mut self) {
        let mut a = self.regs.a;
        let mut adjust = if self.regs.get_flag(Flag::C) {
            0x60
        } else {
            0x00
        };
        if self.regs.get_flag(Flag::H) {
            adjust |= 0x06;
        };
        if !self.regs.get_flag(Flag::N) {
            if a & 0x0f > 0x09 {
                adjust |= 0x06;
            };
            if a > 0x99 {
                adjust |= 0x60;
            };
            a = a.wrapping_add(adjust);
        } else {
            a = a.wrapping_sub(adjust);
        }
        self.regs.set_flag(Flag::C, adjust >= 0x60);
        self.regs.set_flag(Flag::H, false);
        self.regs.set_flag(Flag::Z, a == 0x00);
        self.regs.a = a;
    }

    /// Complement A registerister. (Flip all bits.)
    fn alu_cpl(&mut self) {
        self.regs.a = !self.regs.a;
        self.regs.set_flag(Flag::H, true);
        self.regs.set_flag(Flag::N, true);
    }

    /// Complement carry flag. If C flag is set, then reset it. If C flag is reset, then set it.
    /// Flags affected:
    fn alu_ccf(&mut self) {
        let v = !self.regs.get_flag(Flag::C);
        self.regs.set_flag(Flag::C, v);
        self.regs.set_flag(Flag::H, false);
        self.regs.set_flag(Flag::N, false);
    }

    /// Set Carry flag.
    fn alu_scf(&mut self) {
        self.regs.set_flag(Flag::C, true);
        self.regs.set_flag(Flag::H, false);
        self.regs.set_flag(Flag::N, false);
    }

    /// Rotate A left. Old bit 7 to Carry flag.
    fn alu_rlc(&mut self, a: u8) -> u8 {
        let c = (a & 0x80) >> 7 == 0x01;
        let r = (a << 1) | u8::from(c);
        self.regs.set_flag(Flag::C, c);
        self.regs.set_flag(Flag::H, false);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, r == 0x00);
        r
    }

    /// Rotate A left through Carry flag.
    fn alu_rl(&mut self, a: u8) -> u8 {
        let c = (a & 0x80) >> 7 == 0x01;
        let r = (a << 1) + u8::from(self.regs.get_flag(Flag::C));
        self.regs.set_flag(Flag::C, c);
        self.regs.set_flag(Flag::H, false);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, r == 0x00);
        r
    }

    /// Rotate A right. Old bit 0 to Carry flag.
    fn alu_rrc(&mut self, a: u8) -> u8 {
        let c = a & 0x01 == 0x01;
        let r = if c { 0x80 | (a >> 1) } else { a >> 1 };
        self.regs.set_flag(Flag::C, c);
        self.regs.set_flag(Flag::H, false);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, r == 0x00);
        r
    }

    /// Rotate A right through Carry flag.
    fn alu_rr(&mut self, a: u8) -> u8 {
        let c = a & 0x01 == 0x01;
        let r = if self.regs.get_flag(Flag::C) {
            0x80 | (a >> 1)
        } else {
            a >> 1
        };
        self.regs.set_flag(Flag::C, c);
        self.regs.set_flag(Flag::H, false);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, r == 0x00);
        r
    }

    /// Shift n left into Carry. LSB of n set to 0.
    /// n = A,B,C,D,E,H,L,(HL)
    fn alu_sla(&mut self, a: u8) -> u8 {
        let c = (a & 0x80) >> 7 == 0x01;
        let r = a << 1;
        self.regs.set_flag(Flag::C, c);
        self.regs.set_flag(Flag::H, false);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, r == 0x00);
        r
    }

    /// Shift n right into Carry. MSB doesn't change.
    /// n = A,B,C,D,E,H,L,(HL)
    fn alu_sra(&mut self, a: u8) -> u8 {
        let c = a & 0x01 == 0x01;
        let r = (a >> 1) | (a & 0x80);
        self.regs.set_flag(Flag::C, c);
        self.regs.set_flag(Flag::H, false);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, r == 0x00);
        r
    }

    /// Shift n right into Carry. MSB set to 0.
    /// n = A,B,C,D,E,H,L,(HL)
    fn alu_srl(&mut self, a: u8) -> u8 {
        let c = a & 0x01 == 0x01;
        let r = a >> 1;
        self.regs.set_flag(Flag::C, c);
        self.regs.set_flag(Flag::H, false);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, r == 0x00);
        r
    }

    /// Test bit b in registerister r.
    /// b = 0 - 7, r = A,B,C,D,E,H,L,(HL)
    fn alu_bit(&mut self, a: u8, b: u8) {
        let r = a & (1 << b) == 0x00;
        self.regs.set_flag(Flag::H, true);
        self.regs.set_flag(Flag::N, false);
        self.regs.set_flag(Flag::Z, r);
    }

    /// Set bit b in registerister r.
    /// b = 0 - 7, r = A,B,C,D,E,H,L,(HL)
    fn alu_set(&mut self, a: u8, b: u8) -> u8 {
        a | (1 << b)
    }

    /// Reset bit b in registerister r.
    /// b = 0 - 7, r = A,B,C,D,E,H,L,(HL)
    fn alu_res(&mut self, a: u8, b: u8) -> u8 {
        a & !(1 << b)
    }

    /// Add n to current address and jump to it.
    /// n = one byte signed immediate value
    fn alu_jr(&mut self, n: u8) {
        self.regs.pc = ((self.regs.pc as u32 as i32) + n as i8 as i32) as u16;
    }
}
// Real time cpu provided to simulate real hardware speed.
pub struct RTC {
    pub cpu: Z80,
    step_cycles: u32,
    step_zero: std::time::Instant,
    step_flip: bool,
}

impl RTC {
    pub fn power_up(bus: Rc<RefCell<Bus>>) -> Self {
        let cpu = Z80::new(bus);
        Self {
            cpu,
            step_cycles: 0,
            step_zero: std::time::Instant::now(),
            step_flip: false,
        }
    }

    // Function next simulates real hardware execution speed, by limiting the frequency of the function cpu.next().
    pub fn next(&mut self) -> u32 {
        if self.step_cycles > STEP_CYCLES {
            self.step_flip = true;
            self.step_cycles -= STEP_CYCLES;
            let now = std::time::Instant::now();
            let d = now.duration_since(self.step_zero);
            let s = u64::from(STEP_TIME.saturating_sub(d.as_millis() as u32));
            debug!("CPU: sleep {} millis", s);
            thread::sleep(std::time::Duration::from_millis(s));
            self.step_zero = self
                .step_zero
                .checked_add(std::time::Duration::from_millis(u64::from(STEP_TIME)))
                .unwrap();
            // If now is after the just updated target frame time, reset to
            // avoid drift.
            if now.checked_duration_since(self.step_zero).is_some() {
                self.step_zero = now;
            }
        }
        let cycles = self.cpu.run();
        self.step_cycles += cycles;
        cycles
    }

    pub fn flip(&mut self) -> bool {
        let r = self.step_flip;
        if r {
            self.step_flip = false;
        }
        r
    }
}
