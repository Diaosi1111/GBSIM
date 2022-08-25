pub struct Intf {
    pub data: u8,
}
impl Intf {
    pub fn new() -> Self {
        Self { data: 0x00 }
    }

    pub fn hi(&mut self, flag: Flag) {
        self.data |= 1 << flag as u8;
    }
}

#[derive(Clone)]
pub enum Flag {
    VBlank = 0,
    LCDCStatus = 1,
    Timer = 2,
    Serial = 3,
    Joypad = 4,
}
