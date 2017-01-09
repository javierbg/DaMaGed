use std::fmt;
use super::{mem_map, rom, gbio};
use super::mem_map::Addr;

#[allow(dead_code)]
pub struct Interconnect{
	rom: rom::ROM,

	vram: [u8 ; mem_map::VRAM_LENGTH as usize],

	internal_ram: [u8 ; mem_map::INTERNAL_RAM_LENGTH as usize],

	sprite_ram: [u8 ; mem_map::SPRITE_RAM_LENGTH as usize],

	io: gbio::GBIO,

	high_ram: [u8 ; mem_map::HIGH_RAM_LENGTH as usize]
}

impl Interconnect {
	pub fn new(boot_rom: Box<[u8]>, cart_rom: Box<[u8]>) -> Interconnect {
		Interconnect {
			rom: rom::ROM::new(boot_rom, cart_rom),

			vram: [0u8 ; mem_map::VRAM_LENGTH as usize],

			internal_ram: [0u8 ; mem_map::INTERNAL_RAM_LENGTH as usize],

			sprite_ram: [0u8 ; mem_map::SPRITE_RAM_LENGTH as usize],

			io: gbio::GBIO::new(),

			high_ram: [0u8 ; mem_map::HIGH_RAM_LENGTH as usize]
		}
	}

	pub fn read_byte(&self, addr: u16) -> u8 {
		let real_addr = mem_map::map_addr(addr);

		match real_addr {
			Addr::Bank0(_) | Addr::BankN(_) => {
				self.rom.read_rom(real_addr, self.io.boot_sequence())
			},
			_ => 0u8
		}
	}

	pub fn write_byte(&mut self, addr: u16, val: u8) {
		let real_addr = mem_map::map_addr(addr);

		match real_addr {
			Addr::Bank0(_) => self.rom.write_rom(real_addr, val),
			Addr::BankN(_) => self.rom.write_rom(real_addr, val),
			Addr::VRam(a) => {
				println!("Writing {:02X} into VRAM address {:04X}", val, addr);
				self.vram[a as usize] = val;
			},
			Addr::HardwareIO(_) => self.io.write_byte(real_addr, val),

			_ => {
				panic!("Writing to {:04X} not implemented", addr);
			}
		};
	}
}

impl fmt::Debug for Interconnect {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
		write!(f, "HERE BE RAM")
	}
}
