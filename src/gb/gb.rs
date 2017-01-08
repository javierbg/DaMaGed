use super::Interconnect;
use super::Cpu;

#[derive(Debug)]
pub struct GB {
	cpu: Cpu,
	interconnect: Interconnect
}

#[allow(dead_code)]
impl GB {
	pub fn new(boot_rom: Box<[u8]>, cart_rom: Box<[u8]>) -> GB {
		GB {
			cpu: Cpu::default(),
			interconnect: Interconnect::new(boot_rom, cart_rom)
		}
	}

	pub fn run(&mut self) {
		self.cpu.run(&mut self.interconnect);
	}

	/*fn read_byte(&self, addr: u16) -> u8 {

	}*/
}
