#![allow(dead_code)]

extern crate argparse;
extern crate minifb;

mod emulator;
mod gb;
mod cpu;
mod instruction;
mod interconnect;
mod io;
mod mem_map;
mod rom;
mod video;

use std::fs;
use std::io::Read;
use std::path::Path;

fn main() {
    let mut cart_rom_filename: String = "".into();
    let mut debug = false;

    {
        let mut parser = argparse::ArgumentParser::new();

        parser.refer(&mut cart_rom_filename)
            .add_argument("cart_rom", argparse::Store,
                          "Path to a Cartridge ROM")
            .required();

        parser.refer(&mut debug)
            .add_option(&["-g"], argparse::StoreTrue,
                        "Activate debug mode");

        parser.parse_args_or_exit();
    }


    let boot_rom_filename = "boot.bin";
    let cart_rom_filename = cart_rom_filename;

    //Read boot ROM data
    let boot_rom_buffer = read_rom(boot_rom_filename);
    let cart_rom_buffer = read_rom(cart_rom_filename);

    println!("Boot ROM size: {} bytes", boot_rom_buffer.len());
    println!("Cart ROM size: {} Kbytes", cart_rom_buffer.len());

    let mut emul = emulator::Emulator::new(boot_rom_buffer, cart_rom_buffer, debug);

    emul.run();
}

fn read_rom<P: AsRef<Path>>(path: P) -> Box<[u8]> {
    let mut file = fs::File::open(path).unwrap();
    let mut buffer = Vec::<u8>::new();

    file.read_to_end(&mut buffer).unwrap();

    buffer.into_boxed_slice()
}
