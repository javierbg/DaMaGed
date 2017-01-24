use std::fmt;
use mem_map;
use mem_map::Addr;
use rom;
use io;

use cpu::Interrupt;
use video::VideoBuffer;

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
			Addr::Bank0(_) | Addr::BankN(_) => self.rom.read_rom(real_addr, self.io.boot_sequence()),

			Addr::InternalRam(a) => self.internal_ram[a as usize],

			Addr::SpriteRam(a) => self.io.ppu.read_sprite_entry(a),
			Addr::HardwareIO(a) => self.io.read_byte(a),

			Addr::HighRam(a) => self.high_ram[a as usize],

			Addr::InterruptEnable => self.io.read_byte(0xFFu8),

			_ => panic!("Reading from {:04X} not implemented", addr),
		}
	}

	pub fn write_byte(&mut self, addr: u16, val: u8) {
		let real_addr = mem_map::map_addr(addr);

		match real_addr {
			Addr::Bank0(_) | Addr::BankN(_) => self.rom.write_rom(real_addr, val),
			Addr::InternalRam(a) => self.internal_ram[a as usize] = val,
			Addr::VRam(a) => self.io.ppu.vram[a as usize] = val,
			Addr::SpriteRam(a) => self.io.ppu.write_sprite_entry(a, val),
			Addr::ExternalRam(_) => println!("Write to external RAM {:04X}", addr),
			Addr::Unused => {},
			Addr::HardwareIO(a) => {
				if a == 0x46 {
					self.dma_transfer(val);
				} else {
					self.io.write_byte(a, val);
				}
			},
			Addr::HighRam(a) => self.high_ram[a as usize] = val,
			Addr::InterruptEnable => self.io.write_byte(0xFFu8, val),

			_ => panic!("Writing to {:04X} not implemented", addr),
		};
	}

	fn dma_transfer(&mut self, addr_hi: u8) {
		let source_start: u16 = (addr_hi as u16) << 8;
		let source_end  : u16 = source_start + 0x00A0;

		let mut dest_addr  : u16 = 0xFE00;

		for source_addr in source_start..source_end {
			let copied_byte = self.read_byte(source_addr);
			self.write_byte(dest_addr, copied_byte);
			dest_addr += 1;
		}
	}

	pub fn read_2bytes(&self, addr: u16) -> (u8, u8) {
		let lsb = self.read_byte(addr);
		let msb = self.read_byte(addr+1);
		(lsb, msb)
	}

	pub fn advance_cycles(&mut self, n_cycles: u64, vbuff: &mut VideoBuffer) -> Option<Interrupt> {
		self.io.advance_cycles(n_cycles, vbuff)
	}
}

impl fmt::Debug for Interconnect {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
		write!(f, "HERE BE RAM")
	}
}
