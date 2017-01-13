use super::Interconnect;
use super::cpu;
use super::cpu::instruction::{Reg8, Reg16, Register};
use super::cpu::instruction;


use std::io;
use std::io::Write;
use std::collections::HashSet;

#[derive(Debug)]
pub struct GB {
	cpu: cpu::Cpu,
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

	pub fn step(&mut self) {
		self.cpu.step(&mut self.interconnect);
	}

	pub fn run(&mut self, debug: bool) {
		if debug {
			// TODO: print program info
			self.run_debug_mode();
		}
		else {
			self.cpu.run(&mut self.interconnect);
		}
	}

	fn run_debug_mode(&mut self) {
		let mut stdout = io::stdout();
		let stdin = io::stdin();
		let mut stdin_buffer = String::new();

		let mut breakpoints = HashSet::<u16>::new();

		loop{
			print!("> ");
			stdout.flush();

			// parse it
			stdin_buffer.clear();
			if let Err(err) = stdin.read_line(&mut stdin_buffer) {
				println!("Input error: {}", err);
				continue;
			}

			if let Some(op) = Self::parse_debug_operation(&stdin_buffer) {
				match op {
					DebugOp::SetBreakpoint(addr) => {
						if breakpoints.contains(&addr) {
							breakpoints.remove(&addr);
							println!("Breakpoint at 0x{:04X} removed", addr);
						}
						else {
							breakpoints.insert(addr);
							println!("Breakpoint set at address 0x{:04X}", addr);
						}
					}

					DebugOp::Continue => {
						while !breakpoints.contains(&self.cpu.pc) {
							let pc_of_inst = self.cpu.pc; // Needs to be retreived before step
							let inst = self.cpu.step(&mut self.interconnect);
							println!("  {:04X} : {}", pc_of_inst, inst);
						}
					}

					DebugOp::Step => {
						let pc_of_inst = self.cpu.pc; // Needs to be retreived before step
						let inst = self.cpu.step(&mut self.interconnect);
						println!("  {:04X} : {}", pc_of_inst, inst);
					}

					DebugOp::PrintRegister(r) => {
						match r {
							Register::Register8(r8) => {
								let val = self.cpu.read_8bit_register(&self.interconnect, r8);
								println!("{}: {:02X}", r8, val);
							}
							Register::Register16(r16) => {
								let val = self.cpu.read_16bit_register(r16);
								println!("{}: {:04X}", r16, val);
							}
						}
					}

					DebugOp::PrintCpuRegs => {
						println!("{}", self.cpu);
					}

					DebugOp::Quit => break,

					DebugOp::Disassemble(n) => {
						for (addr,inst) in instruction::get_next_instructions(&self.interconnect, self.cpu.pc, n) {
							println!("  {:04X} : {}", addr, inst);
						}
					}
				}
			}

			else {
				println!("Unrecognized operation");
			}
		}
	}

	fn parse_debug_operation(input: &String) -> Option<DebugOp> {
		let chunks = input.split_whitespace().collect::<Vec<&str>>();

		if chunks.len() == 0 {
			return None;
		}

		match chunks[0] {
			"c" => Some(DebugOp::Continue),

			"b" => {
				if chunks.len() != 2 {
					println!("`b' syntax: b <hex_address>");
					None
				}
				else {
					if let Ok(addr) = u16::from_str_radix(chunks[1], 16) {
						Some(DebugOp::SetBreakpoint(addr))
					}
					else {
						println!("Invalid address");
						None
					}
				}
			},

			"pa" => Some(DebugOp::PrintCpuRegs),

			"p" => {
				if chunks.len() != 2 {
					println!("`p' syntax: p <register>");
					None
				}
				else {
					if let Some(reg) = match chunks[1].to_uppercase().as_ref() {
						"A" => Some(Register::Register8(Reg8::A)),
						"F" => Some(Register::Register8(Reg8::F)),
						"B" => Some(Register::Register8(Reg8::B)),
						"C" => Some(Register::Register8(Reg8::C)),
						"D" => Some(Register::Register8(Reg8::D)),
						"E" => Some(Register::Register8(Reg8::E)),
						"H" => Some(Register::Register8(Reg8::H)),
						"L" => Some(Register::Register8(Reg8::L)),
						"SP" => Some(Register::Register16(Reg16::SP)),
						"PC" => Some(Register::Register16(Reg16::PC)),
						"BC" => Some(Register::Register16(Reg16::BC)),
						"DE" => Some(Register::Register16(Reg16::DE)),
						"HL" => Some(Register::Register16(Reg16::HL)),
						_   => {
							println!("Unrecognized register \"{}\"", chunks[1]);
							None
						}
					} {
						Some(DebugOp::PrintRegister(reg))
					}
					else{
						None
					}

				}
			}

			"q" => Some(DebugOp::Quit),

			"d" => {
				if chunks.len() != 2 {
					println!("`d' syntax: d <number_of_instructions>");
					None
				}
				else {
					if let Ok(n) = u16::from_str_radix(chunks[1], 10) {
						Some(DebugOp::Disassemble(n))
					}
					else {
						println!("Invalid number of instructions \"{}\"", chunks[1]);
						None
					}
				}
			}

			"s" => Some(DebugOp::Step),

			_ => None
		}
	}
}

enum DebugOp {
	Continue, // Continue until next breakpoint
	SetBreakpoint(u16), // Set breakpoint at said memory address (it should be the start of an instruction)
	PrintRegister(Register), // Print the contents of a register
	Quit,
	Disassemble(u16), // Disassemble the next n instructions
	PrintCpuRegs, // Print all CPU registers
	Step, // Execute just one CPU instruction
}
