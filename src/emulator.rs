use std::collections::HashSet;
use std::io;
use std::io::Write;
use std::time::{Instant, Duration};
use std::thread::sleep;

use cpu::{Register, Reg8, Reg16};
use instruction::Instruction;
use gb;
use video::VideoBuffer;

use minifb::{Window, WindowOptions, Scale, Key};

const CYCLES_PER_FRAME:  u64 = 70_256;

enum ExecutionMode {
	Running,
	Debugging,
}

#[derive(Clone, Copy)]
enum DebugCommand {
	Continue, // Continue until next breakpoint
	SetBreakpoint(u16), // Set breakpoint at said memory address (it should be the start of an instruction)
	PrintRegister(Register), // Print the contents of a register
	Quit,
	Disassemble(u16), // Disassemble the next n instructions
	PrintCpuRegs, // Print all CPU registers
	Step, // Execute just one CPU instruction
	LastCommand, // Repeat last command
}

pub struct Emulator {
	gb: gb::GB,
	mode: ExecutionMode,

	//Debugging
	breakpoints: HashSet<u16>,
	last_command: Option<DebugCommand>,
}

impl Emulator {
	pub fn new(boot_rom: Box<[u8]>, cart_rom: Box<[u8]>, debug: bool) -> Emulator {
		Emulator {
			gb: gb::GB::new(boot_rom, cart_rom),
			mode: if debug {ExecutionMode::Debugging} else {ExecutionMode::Running},

			breakpoints: HashSet::new(),
			last_command: None,
		}
	}

	pub fn run(&mut self) {
		match self.mode {
			ExecutionMode::Running => {
				let w_options = WindowOptions {
					borderless: false,
					title: true,
					resize: false,
					scale: Scale::X2,
				};

				match  Window::new("DaMaGed", 160, 144, w_options) {
					Ok(mut window) => self.run_window(&mut window),
					Err(err) => println!("Error creating window: {}", err),
				}
			}

			ExecutionMode::Debugging => {
				self.run_debug_mode();
			}
		}
	}

	fn run_window(&mut self, window: &mut Window) {
		let frame_duration: Duration = Duration::new(0,16750419);
		let mut frame_start = Instant::now();

		while window.is_open() && !window.is_key_down(Key::Escape) {
			let mut vbuff = VideoBuffer::default();

			self.gb.step(&mut vbuff);

			if let Some(buff) = vbuff.next_frame {
				window.update_with_buffer(&buff);

				let elapsed = frame_start.elapsed();
				if elapsed < frame_duration {
					let remaining = frame_duration - elapsed;
					sleep(remaining);
				}

				frame_start = Instant::now();
			}
		}
	}

	fn execute_debug_command(&mut self, command: DebugCommand) {
		let mut vbuff = VideoBuffer::default();

		match command {
			DebugCommand::SetBreakpoint(addr) => {
				if self.breakpoints.contains(&addr) {
					self.breakpoints.remove(&addr);
					println!("Breakpoint at 0x{:04X} removed", addr);
				}
				else {
					self.breakpoints.insert(addr);
					println!("Breakpoint set at address 0x{:04X}", addr);
				}
			}

			DebugCommand::Continue => {
				let pc_of_inst = self.gb.cpu.pc; // Needs to be retreived before step
				let (inst, _) = self.gb.step(&mut vbuff);
				self.print_instruction(pc_of_inst, inst);

				while !self.breakpoints.contains(&self.gb.cpu.pc) {
					let pc_of_inst = self.gb.cpu.pc; // Needs to be retreived before step
					let (inst, _) = self.gb.step(&mut vbuff);
					self.print_instruction(pc_of_inst, inst);
				}
			}

			DebugCommand::Step => {
				let pc_of_inst = self.gb.cpu.pc; // Needs to be retreived before step
				let (inst, _) = self.gb.step(&mut vbuff);
				self.print_instruction(pc_of_inst, inst);
			}

			DebugCommand::PrintRegister(r) => {
				match r {
					Register::Register8(r8) => {
						let val = self.gb.read_8bit_register(r8);
						println!("{}: {:02X}", r8, val);
					}
					Register::Register16(r16) => {
						let val = self.gb.read_16bit_register(r16);
						println!("{}: {:04X}", r16, val);
					}
				}
			}

			DebugCommand::PrintCpuRegs => {
				println!("{}", self.gb.cpu);
			}

			DebugCommand::Disassemble(n) => {
				for (addr,inst) in self.gb.get_next_instructions(n) {
					self.print_instruction(addr, inst);
				}
			}

			_ => {}
		}
	}

	fn print_instruction(&self, addr: u16, inst: Instruction) {
		println!("{} {:04X} : {}", if self.breakpoints.contains(&addr) {"*"} else {" "},
								   addr, inst);
	}

	fn run_debug_mode(&mut self) {
		let mut stdout = io::stdout();
		let stdin = io::stdin();
		let mut stdin_buffer = String::new();

		loop{
			print!("> ");
			stdout.flush();

			// parse it
			stdin_buffer.clear();
			if let Err(err) = stdin.read_line(&mut stdin_buffer) {
				println!("Input error: {}", err);
				continue;
			}

			if let Some(comm) = Self::parse_debug_operation(&stdin_buffer) {
				match comm {
					DebugCommand::Quit => break,
					DebugCommand::LastCommand => {
						if let Some(last_comm) = self.last_command {
							self.execute_debug_command(last_comm);
						}
					},
					_ => {
						self.last_command = Some(comm);
						self.execute_debug_command(comm);
					}
				}
			}

			else {
				println!("Unrecognized operation");
			}
		}
	}

	fn parse_debug_operation(input: &String) -> Option<DebugCommand> {
		let chunks = input.split_whitespace().collect::<Vec<&str>>();

		if chunks.len() == 0 {
			return Some(DebugCommand::LastCommand);
		}

		match chunks[0] {
			"c" => Some(DebugCommand::Continue),

			"b" => {
				if chunks.len() != 2 {
					println!("`b' syntax: b <hex_address>");
					None
				}
				else {
					if let Ok(addr) = u16::from_str_radix(chunks[1], 16) {
						Some(DebugCommand::SetBreakpoint(addr))
					}
					else {
						println!("Invalid address");
						None
					}
				}
			},

			"pa" => Some(DebugCommand::PrintCpuRegs),

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
						Some(DebugCommand::PrintRegister(reg))
					}
					else{
						None
					}

				}
			}

			"q" => Some(DebugCommand::Quit),

			"d" => {
				if chunks.len() != 2 {
					println!("`d' syntax: d <number_of_instructions>");
					None
				}
				else {
					if let Ok(n) = u16::from_str_radix(chunks[1], 10) {
						Some(DebugCommand::Disassemble(n))
					}
					else {
						println!("Invalid number of instructions \"{}\"", chunks[1]);
						None
					}
				}
			}

			"s" => Some(DebugCommand::Step),

			_ => None
		}
	}
}
