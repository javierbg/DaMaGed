use mem_map;

const BOOT_ROM_SIZE: u16 = 256;

pub struct ROM {
	boot_rom: Box<[u8]>,
	cart_rom: Box<[u8]>,

	// Current ROM bank should really be a u8, and that's the way it should be "interfaced"
	// but storing it as a u16 makes it so that you only have to cast when switching banks, but
	// not every time you access BankN
	current_rom_bank: u8,
}

impl ROM {
	pub fn new(boot_rom: Box<[u8]>, cart_rom: Box<[u8]>) -> ROM {
		assert!(boot_rom.len() == (BOOT_ROM_SIZE as usize), "Invalid boot rom size");

		ROM {
			boot_rom: boot_rom,
			cart_rom: cart_rom,

			current_rom_bank: 1u8,
		}
	}

	pub fn read_rom(&self, addr: mem_map::Addr, boot_sequence: bool) -> u8{
		match addr {
			mem_map::Addr::Bank0(i) => {
				if boot_sequence && (i < BOOT_ROM_SIZE){
					self.boot_rom[i as usize]
				}
				else {
					self.cart_rom[i as usize]
				}
			},

			mem_map::Addr::BankN(i) => {
				let real_rom_addr: usize = ((self.current_rom_bank as usize) * (mem_map::CARTRIDGE_BANK_LENGTH as usize)) + (i as usize);
				self.cart_rom[real_rom_addr]
			},

			_ => {
				panic!("Invalid ROM address")
			}
		}
	}

	// Kind of a contradiction, but this should model the act of "writing" to the ROM, which
	// in a real cartridge would communicate with the Memory Bank Controller (MBC)
	pub fn write_rom(&mut self, addr: mem_map::Addr, val: u8) {
		match addr {
			mem_map::Addr::Bank0(0x2000u16) => { //TODO: Extract to constant?
				self.current_rom_bank = val;
			}

			_ => {
				// Should it panic? Boot ROM actually tries to write once to rom before finishing
				// zeroing the vram
				//panic!("Trying to write to ROM!");
			}
		}
	}
}
