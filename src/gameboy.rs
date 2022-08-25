use std::{cell::RefCell, path::Path, rc::Rc};

use crate::{bus::{Bus, Memory}, cpu::RTC};

pub struct GameBoy {
    pub bus: Rc<RefCell<Bus>>,
    pub cpu: RTC,
}

impl GameBoy {
    pub fn power_up(path: impl AsRef<Path>) -> Self {
        let bus = Rc::new(RefCell::new(Bus::power_up(path)));
        let cpu = RTC::power_up(bus.clone());
        Self { bus, cpu }
    }

    pub fn next(&mut self) -> u32 {
        if self.bus.borrow().read_byte(self.cpu.cpu.regs.pc) == 0x10 {
            // self.mmu.borrow_mut().switch_speed();
            todo!()
        }
        let cycles = self.cpu.next();
        self.bus.borrow_mut().step(cycles);

        cycles
    }

    pub fn check_and_reset_gpu_updated(&mut self) -> bool {
        let result = self.bus.borrow().gpu.v_blank;
        self.bus.borrow_mut().gpu.v_blank = false;
        result
    }

}
