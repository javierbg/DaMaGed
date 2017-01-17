use std::fmt;
use mem_map;
use mem_map::Addr;
use rom;
use io;

#[allow(dead_code)]
pub struct Interconnect{
	rom: rom::ROM,

	internal_ram: [u8 ; mem_map::INTERNAL_RAM_LENGTH as usize],

	io: io::GBIO,

	high_ram: [u8 ; mem_map::HIGH_RAM_LENGTH as usize]
}

impl Interconnect {
	pub fn new(boot_rom: Box<[u8]>, cart_rom: Box<[u8]>) -> Interconnect {
		Interconnect {
			rom: rom::ROM::new(boot_rom, cart_rom),

			internal_ram: [0u8 ; mem_map::INTERNAL_RAM_LENGTH as usize],

			io: io::GBIO::new(),

			high_ram: [0u8 ; mem_map::HIGH_RAM_LENGTH as usize]
		}
	}

	pub fn read_byte(&self, addr: u16) -> u8 {
		let real_addr = mem_map::map_addr(addr);

		match real_addr {
			Addr::Bank0(_) | Addr::BankN(_) => {
				self.rom.read_rom(real_addr, self.io.boot_sequence())
			},

			Addr::HardwareIO(a) => self.io.read_byte(a),

			Addr::HighRam(a) => self.high_ram[a as usize],

			_ => {
				panic!("Reading from {:04X} not implemented", addr);
			}
		}
	}

	pub fn write_byte(&mut self, addr: u16, val: u8) {
		let real_addr = mem_map::map_addr(addr);

		match real_addr {
			Addr::Bank0(_) => self.rom.write_rom(real_addr, val),
			Addr::BankN(_) => self.rom.write_rom(real_addr, val),
			Addr::VRam(a) => {
				self.io.ppu.vram[a as usize] = val;
			},
			Addr::SpriteRam(a) => {
				self.io.ppu.sprite_ram[a as usize] = val;
			},
			Addr::HardwareIO(a) => self.io.write_byte(a, val),
			Addr::HighRam(a) => {
				self.high_ram[a as usize] = val;
			},

			_ => {
				panic!("Writing to {:04X} not implemented", addr);
			}
		};
	}

	pub fn read_2bytes(&self, addr: u16) -> (u8, u8) {
		let lsb = self.read_byte(addr);
		let msb = self.read_byte(addr+1);
		(lsb, msb)
	}
}

impl fmt::Debug for Interconnect {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
		write!(f, "HERE BE RAM")
	}
}
