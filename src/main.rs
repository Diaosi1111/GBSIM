use z80sim::mmu::load_rom;


fn main() {
    env_logger::init();
    println!("Hello, world!");
    load_rom("res/boxes.gb".into());
}
