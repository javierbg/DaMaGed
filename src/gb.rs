use interconnect::Interconnect;
use cpu;
use cpu::{Reg8, Reg16};
use video::VideoBuffer;
use instruction;
use instruction::Instruction;

#[derive(Debug)]
pub struct GB {
	pub cpu: cpu::Cpu,
	interconnect: Interconnect
}

#[allow(dead_code)]
impl GB {
	pub fn new(boot_rom: Box<[u8]>, cart_rom: Box<[u8]>) -> GB {
		GB {
			cpu: cpu::Cpu::default(),
			interconnect: Interconnect::new(boot_rom, cart_rom)
		}
	}

	pub fn game_title(&self) -> String {
		self.interconnect.game_title()
	}

	pub fn step(&mut self, vbuff: &mut VideoBuffer) -> (Instruction, u64) {
		self.cpu.step(&mut self.interconnect, vbuff)
	}

	pub fn run(&mut self, vbuff: &mut VideoBuffer) {
		self.cpu.run(&mut self.interconnect, vbuff);
	}

	pub fn read_8bit_register(&self, r: Reg8) -> u8 {
		self.cpu.read_8bit_register(&self.interconnect, r)
	}

	pub fn read_16bit_register(&self, r: Reg16) -> u16 {
		self.cpu.read_16bit_register(r)
	}

	fn get_next_instruction(&self) -> (u16, Instruction) {
		(self.cpu.pc, instruction::get_next_instruction(&self.interconnect, self.cpu.pc))
	}

	pub fn get_next_instructions(&self, n_inst: u16) -> Vec<(u16, Instruction)> {
		instruction::get_next_instructions(&self.interconnect, self.cpu.pc, n_inst)
	}
}
