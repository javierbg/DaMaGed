mod gb;

use std::env;
use std::fs;
use std::io::Read;
use std::path::Path;

fn main() {
    let boot_rom_filename = "boot.bin";
    let cart_rom_filename = env::args().nth(1).unwrap();

    //Read boot ROM data
    let boot_rom_buffer = read_rom(boot_rom_filename);
    let cart_rom_buffer = read_rom(cart_rom_filename);

    println!("Boot ROM size: {} bytes", boot_rom_buffer.len());
    println!("Cart ROM size: {} bytes", cart_rom_buffer.len());

    let gb = gb::GB::default();
    println!("{:#?}", gb);
}

fn read_rom<P: AsRef<Path>>(path: P) -> Vec<u8> {
    let mut file = fs::File::open(path).unwrap();
    let mut buffer = Vec::<u8>::new();

    file.read_to_end(&mut buffer).unwrap();

    return buffer;
}
