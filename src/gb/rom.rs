use super::mem_map;

const BOOT_ROM_SIZE: u16 = 256;

pub struct ROM {
	boot_rom: Box<[u8]>,
	cart_rom: Box<[u8]>,

	// Current ROM bank should really be a u8, and that's the way it should be "interfaced"
	// but storing it as a u16 makes it so that you only have to cast when switching banks, but
	// not every time you access BankN
	current_rom_bank: u16,
}

impl ROM {
	pub fn new(boot_rom: Box<[u8]>, cart_rom: Box<[u8]>) -> ROM {
		assert!(boot_rom.len() == (BOOT_ROM_SIZE as usize), "Invalid boot rom size");

		ROM {
			boot_rom: boot_rom,
			cart_rom: cart_rom,

			current_rom_bank: 1u16,
		}
	}

	pub fn read_rom(&self, addr: u16, boot_sequence: bool) -> u8{
		match addr {
			mem_map::CARTRIDGE_BANK0_START ... mem_map::CARTRIDGE_BANK0_END => {
				if boot_sequence && (addr < BOOT_ROM_SIZE){
					self.boot_rom[addr as usize]
				}
				else {
					self.cart_rom[addr as usize]
				}
			},

			mem_map::CARTRIDGE_BANKN_START ... mem_map::CARTRIDGE_BANKN_END => {
				let real_rom_addr: u16 = (self.current_rom_bank * mem_map::CARTRIDGE_BANK_LENGTH) + addr;
				self.cart_rom[real_rom_addr as usize]
			},

			_ => {
				panic!("Invalid ROM address")
			}
		}
	}

	// Kind of a contradiction, but this should model the act of "writing" to the ROM, which
	// in a real cartridge would communicate with the Memory Bank Controller (MBC)
	pub fn write_rom(&mut self, addr: u16, val: u8) {
		match addr {
			0x2000u16 => { //TODO: extract to constant?
				self.current_rom_bank = val as u16;
			}

			_ => {
				panic!("Trying to write to ROM!");
			}
		}
	}
}
