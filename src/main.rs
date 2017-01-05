use std::env;
use std::fs;
use std::io::Read;
use std::path::Path;

#[allow(dead_code)]
struct Cpu {
    a: u8,
    f: u8,

    b: u8,
    c: u8,

    d: u8,
    e: u8,

    sp: u16,
    pc: u16,
}

impl Cpu {
    fn new() -> Cpu {
        Cpu {
            // 1 byte 0xdeadbeef ?
            a: 0x42,
            f: 0x42,
            b: 0x42,
            c: 0x42,
            d: 0x42,
            e: 0x42,
            sp: 0x4221,

            pc: 0x0100,
        }
    }
}

fn main() {
    let boot_rom_filename = "boot.bin";
    let cart_rom_filename = env::args().nth(1).unwrap();

    //Read boot ROM data
    let boot_rom_buffer = read_rom(boot_rom_filename);
    let cart_rom_buffer = read_rom(cart_rom_filename);

    println!("Boot ROM size: {} bytes", boot_rom_buffer.len());
    println!("Cart ROM size: {} bytes", cart_rom_buffer.len());

    let cpu = Cpu::new();
}

fn read_rom<P: AsRef<Path>>(path: P) -> Vec<u8> {
    let mut file = fs::File::open(path).unwrap();
    let mut buffer = Vec::<u8>::new();

    file.read_to_end(&mut buffer).unwrap();

    return buffer;
}
