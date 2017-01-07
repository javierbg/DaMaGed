use std::fmt;

const BOOT_ROM_SIZE: usize = 256;
const RAM_SIZE: usize = 8 * 1024; // 8K
const VRAM_SIZE: usize = 8 * 1024; // 8K

#[allow(dead_code)]
pub struct Interconnect{
	boot_rom: Box<[u8]>,
	cart_rom: Box<[u8]>,

	ram: [u8 ; RAM_SIZE],
	vram: [u8 ; VRAM_SIZE]
}

impl Interconnect {
	pub fn new(boot_rom: Box<[u8]>, cart_rom: Box<[u8]>) -> Interconnect {
		assert!(boot_rom.len() == BOOT_ROM_SIZE, "Invalid boot rom size");

		Interconnect {
			boot_rom: boot_rom,
			cart_rom: cart_rom,

			ram: [0u8 ; RAM_SIZE],
			vram: [0u8 ; VRAM_SIZE]
		}
	}
}

impl fmt::Debug for Interconnect {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
		write!(f, "HERE BE RAM")
	}
}
