// fn main() {
//     env_logger::init();
//     GameBoy::init("res/boxes.gb");
//     println!("Hello, world!");
// }
#[cfg(not(feature = "audio"))]
fn initialize_audio(_: &gbs::gameboy::GameBoy) {
    panic!("audio is not supported");
}
use clap::Parser;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    /// auto inc minor
    #[clap(long, default_value_t = 2)]
    scale: u8,

    /// open audio output
    #[clap(short, long)]
    audio: bool,

    /// some regular input
    #[clap(group = "input")]
    rom: String,
    // /// some special input argument
    // #[clap(long, group = "input")]
    // spec_in: Option<String>,

    // #[clap(short, requires = "input")]
    // config: Option<String>,
}

// #[cfg(feature = "gui")]
fn main() {
    use gbs::gameboy::GameBoy;
    use gbs::gpu::{SCREEN_H, SCREEN_W};

    env_logger::init(); //初始化log

    let cli = Cli::parse();

    let mut gameboy = GameBoy::power_up(cli.rom.clone()); //启动gameboy

    // Initialize audio related
    if cli.audio {
        initialize_audio(&gameboy);
    }

    let mut option = minifb::WindowOptions::default();
    option.resize = true;
    option.scale = match cli.scale {
        1 => minifb::Scale::X1,
        2 => minifb::Scale::X2,
        4 => minifb::Scale::X4,
        8 => minifb::Scale::X8,
        _ => panic!("支持的缩放倍率: 1, 2, 4 or 8"),
    };
    let rom_name = gameboy.bus.borrow().cartridge.title();
    let mut window = minifb::Window::new(
        format!("Gameboy - {}", rom_name).as_str(),
        SCREEN_W,
        SCREEN_H,
        option,
    )
    .unwrap();
    let mut window_buffer = vec![0x00; SCREEN_W * SCREEN_H];
    window
        .update_with_buffer(window_buffer.as_slice(), SCREEN_W, SCREEN_H)
        .unwrap();
    loop {
        // Stop the program, if the GUI is closed by the user
        if !window.is_open() {
            break;
        }

        // Execute an instruction
        gameboy.next();

        // Update the window
        if gameboy.check_and_reset_gpu_updated() {
            let mut i: usize = 0;
            for l in gameboy.bus.borrow().gpu.data.iter() {
                for w in l.iter() {
                    let b = u32::from(w[0]) << 16;
                    let g = u32::from(w[1]) << 8;
                    let r = u32::from(w[2]);
                    let a = 0xff00_0000;

                    window_buffer[i] = a | b | g | r;
                    i += 1;
                }
            }
            window
                .update_with_buffer(window_buffer.as_slice(), SCREEN_W, SCREEN_H)
                .unwrap();
        }

        // if !gameboy.cpu.flip() {
        //     continue;
        // }

        // Handling keyboard events
        if window.is_key_down(minifb::Key::Escape) {
            break;
        }
        let keys = vec![
            (minifb::Key::Right, gbs::joypad::JoypadKey::Right),
            (minifb::Key::Up, gbs::joypad::JoypadKey::Up),
            (minifb::Key::Left, gbs::joypad::JoypadKey::Left),
            (minifb::Key::Down, gbs::joypad::JoypadKey::Down),
            (minifb::Key::Z, gbs::joypad::JoypadKey::A),
            (minifb::Key::X, gbs::joypad::JoypadKey::B),
            (minifb::Key::Space, gbs::joypad::JoypadKey::Select),
            (minifb::Key::Enter, gbs::joypad::JoypadKey::Start),
        ];
        for (rk, vk) in &keys {
            if window.is_key_down(*rk) {
                gameboy.bus.borrow_mut().joypad.keydown(vk.clone());
            } else {
                gameboy.bus.borrow_mut().joypad.keyup(vk.clone());
            }
        }
    }
    gameboy.bus.borrow_mut().cartridge.sav();
}
