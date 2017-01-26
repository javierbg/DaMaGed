use mem_map;

const BOOT_ROM_SIZE: u16 = 256;

pub struct ROM {
	boot_rom: Box<[u8]>,
	cart_rom: Box<[u8]>,

	pub game_title: String,

	cart_type: CartridgeType,
	has_battery: bool,
	has_ram: bool,

	// Current ROM bank should really be a u8, and that's the way it should be "interfaced"
	// but storing it as a u16 makes it so that you only have to cast when switching banks, but
	// not every time you access BankN
	n_rom_banks: u8,
	current_rom_bank_start: usize,

	n_ram_banks: u8,
}

enum CartridgeType {
	RomOnly,
	MBC1, MBC2, MBC3
}

impl ROM {
	pub fn new(boot_rom: Box<[u8]>, cart_rom: Box<[u8]>) -> ROM {
		assert!(boot_rom.len() == (BOOT_ROM_SIZE as usize), "Invalid boot rom size");
		//assert!(cart_rom[0x143] != 0x80, "Game Boy Color games are not valid");

		let title = String::from_utf8_lossy(&cart_rom[0x134..0x143]).into_owned();

		let (cart_type, battery, ram) = match cart_rom[0x147] {
			0x00 => (CartridgeType::RomOnly, false, false),
			0x13 => (CartridgeType::MBC3, true, true),
			_ => panic!("Unimplemented cartridge type"),
		};

		let n_rom_banks = match cart_rom[0x148] {
			0 => 2,
			1 => 4,
			2 => 8,
			3 => 16,
			4 => 32,
			5 => 64,
			6 => 128,
			0x52 => 72,
			0x53 => 80,
			0x54 => 96,
			_ => panic!("Unkown ROM size"),
		};

		let n_ram_banks = match cart_rom[0x149] {
			0 => 0,
			1 => 1,
			2 => 1,
			3 => 4,
			4 => 16,
			_ => panic!("Unknown cartridge RAM size"),
		};

		ROM {
			boot_rom: boot_rom,
			cart_rom: cart_rom,

			game_title: title,

			cart_type: cart_type,
			has_battery: battery,
			has_ram: ram,

			n_rom_banks: n_rom_banks,
			current_rom_bank_start: mem_map::CARTRIDGE_BANKN_START as usize,

			n_ram_banks: n_ram_banks,
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
				let bank_addr = (addr - mem_map::CARTRIDGE_BANKN_START) as usize;
				self.cart_rom[self.current_rom_bank_start + bank_addr]
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
			0x2000 ... 0x3FFF => { //TODO: Extract to constant?
				match self.cart_type {
					CartridgeType::RomOnly => {},
					CartridgeType::MBC3 =>  {
						let selection = (val & 0b0111_111) as usize;
						let n_bank = if selection == 0 { 1 } else { selection };

						self.current_rom_bank_start = n_bank * 0x4000;
					},

					_ => panic!("Unimplemented cartridge type"),
				}
			}

			_ => {
				// Should it panic? Boot ROM actually tries to write once to rom before finishing
				// zeroing the vram
				//panic!("Trying to write to ROM!");
			}
		}
	}
}
