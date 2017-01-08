use std::fmt;
use super::mem_map;

const BOOT_ROM_SIZE: usize = 256;

#[allow(dead_code)]
pub struct Interconnect{
	boot_rom: Box<[u8]>,
	cart_rom: Box<[u8]>,

	current_rom_bank: u8,

	vram: [u8 ; mem_map::VRAM_LENGTH as usize],

	internal_ram: [u8 ; mem_map::INTERNAL_RAM_LENGTH as usize],

	sprite_ram: [u8 ; mem_map::SPRITE_RAM_LENGTH as usize],

	high_ram: [u8 ; mem_map::HIGH_RAM_LENGTH as usize],

	interrupt_enable_register: u8
}

impl Interconnect {
	pub fn new(boot_rom: Box<[u8]>, cart_rom: Box<[u8]>) -> Interconnect {
		assert!(boot_rom.len() == BOOT_ROM_SIZE, "Invalid boot rom size");

		Interconnect {
			boot_rom: boot_rom,
			cart_rom: cart_rom,

			current_rom_bank: 1u8,

			vram: [0u8 ; mem_map::VRAM_LENGTH as usize],

			internal_ram: [0u8 ; mem_map::INTERNAL_RAM_LENGTH as usize],

			sprite_ram: [0u8 ; mem_map::SPRITE_RAM_LENGTH as usize],

			high_ram: [0u8 ; mem_map::HIGH_RAM_LENGTH as usize],

			interrupt_enable_register: 0u8
		}
	}
}

impl fmt::Debug for Interconnect {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
		write!(f, "HERE BE RAM")
	}
}
