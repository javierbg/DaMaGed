use interconnect::Interconnect;
use std::fmt;
use std::iter;

use cpu::{Reg8, Reg16};

// Whole data of an instruction. Because it holds the original bytes, it can be
// dissassembled.
pub struct Instruction {
	pub ex: ExInstruction,
	pub bytes: Vec<u8>,
	pub cycles: u64,
}

impl Instruction {
	fn mnemonic(&self) -> String {
		let ex = &self.ex;
		match ex {
			&ExInstruction::Nop => "nop".into(),
			&ExInstruction::Load8(Reg8::MemH(a), Reg8::A) => format!("ldh ($ff00+{:02x}),a", a),
			&ExInstruction::Load8(Reg8::A, Reg8::MemH(a)) => format!("ldh a,($ff00+{:02x})", a),
			&ExInstruction::Load8(dst, src) => format!("ld {},{}",dst,src),
			&ExInstruction::Load8Imm(dst, val) => format!("ld {},${:02x}",dst,val),
			&ExInstruction::Load16(dst, src) => format!("ld {},{}",dst,src),
			&ExInstruction::Load16Imm(dst, val) => format!("ld {},${:04x}",dst,val),
			&ExInstruction::Compare(r) => format!("cp {}", r),
			&ExInstruction::CompareImm(val) => format!("cp ${:02x}", val),
			&ExInstruction::JrC(jr, cond) => format!("jr {},${}",cond,jr),
			&ExInstruction::LoadHLPredec => "ld (-hl),a".into(),
			&ExInstruction::LoadHLPostinc => "ld (hl+),a".into(),
			&ExInstruction::AddA(r) => format!("add a,{}", r),
			&ExInstruction::SubA(r) => format!("sub {}", r),
			&ExInstruction::Xor(r) => format!("xor {}", r),
			&ExInstruction::Increment8(r) => format!("inc {}",r),
			&ExInstruction::Decrement8(r) => format!("dec {}",r),
			&ExInstruction::Increment16(r) => format!("inc {}",r),
			&ExInstruction::Decrement16(r) => format!("dec {}",r),
			&ExInstruction::Call(a) => format!("call ${:04x}", a),
			&ExInstruction::Return => format!("ret"),
			&ExInstruction::Push(r) => format!("push {}", r),
			&ExInstruction::Pop(r)  => format!("pop {}", r),
			&ExInstruction::Bit(r,b) => format!("bit {},{}",b,r),
			&ExInstruction::Rotate(reg,dir,carry) => {
				if self.bytes[0] == 0xCB { // CB prefixed op
					format!("r{}{} {}", if dir {"l"} else {"r"},
				                        if carry {"c"} else {""},
								        reg)
				} else { // Rotate on A register
					format!("r{}{}a", if dir {"l"} else {"r"},
				                      if carry {"c"} else {""})
				}
			}

			_ => format!("{:?}", self.ex)
		}
	}
}

impl fmt::Display for Instruction {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let mn = self.mnemonic();
		let pad: String = iter::repeat(" ").take(20-mn.len()).collect();

		// Get bytes representation
		let mut bs: String = "".into();
		for b in &self.bytes {
			bs = format!("{} {:02X}", bs, b);
		}

		write!(f, "{}{}; {}", mn, pad, bs)
	}
}

// Executable instruction. This is what the CPU will use to execute the instruction
// with less efort
#[derive(Debug)]
pub enum ExInstruction {
	Load8(Reg8, Reg8),
	Load8Imm(Reg8, u8),

	Load16(Reg16, Reg16),
	Load16Imm(Reg16, u16),

	// Special Game Boy instructions
	LoadHLPostinc, // (HL) <- A, HL <- HL + 1
	LoadAPostinc,  // A <- (HL), HL <- HL + 1
	LoadHLPredec,  // HL <- HL - 1, (HL) <- A
	LoadAPredec,   // HL <- HL - 1, A <- (HL)

	Push(Reg16),
	Pop(Reg16),

	// Add to Accumulator
	AddA(Reg8), AddAImm(u8),

	// Add to Accumulator with carry
	AddC(Reg8), AddCImm(u8),

	// Subtract to Accumulator
	SubA(Reg8), SubAImm(u8),

	// Subtract to Accumulator with carry
	SubC(Reg8), SubCImm(u8),

	And(Reg8), AndImm(u8),

	Or(Reg8), OrImm(u8),

	Xor(Reg8), XorImm(u8),

	Compare(Reg8), CompareImm(u8),

	Increment8(Reg8), Decrement8(Reg8),

	Daa, // Decimal Adjust A (I think?). In short, adjust so number is BCD
	Cpl, // Invert A (one's complement)
	Neg, // Negate A (A <- 0 - A, aka two's complement)
	Ccf, // Toggle Carry flag
	Scf, // Set Carry flag
	Nop, // No operation
	Halt, // Halt the system (power saving). Wake up on interrupt
	Di, // Disable interrupt flip-flops (IFF1, IFF2)
	Ei, // Enable the above

	// 16-bit arithmetic
	Add16(Reg16), // Add to HL
	Increment16(Reg16),
	Decrement16(Reg16),

	// Rotate and Shift

	// This encapsulates all rotations. Params, in order:
	// register, direction (true=left), carry
	Rotate(Reg8, bool, bool),

	// Same with shifts
	// register, direciton (true=left), type (true=arithmetic)
	Shift(Reg8, bool, bool),

	Swap(Reg8),

	Bit(Reg8, u8), // Test the n-th bit of a register
	Set(Reg8, u8), // Set the n-th bit of a register
	Res(Reg8, u8), // Reset the n-th bit of a register

	// Jump instructions
	Jp(u16), // Jump execution to address
	JpC(u16, Condition), // Jump only if condition is met
	Jr(i8), // Relative jump
	JrC(i8, Condition), // Relative jump only if condition is met
	JpHL, // Jump to the contents of HL (PC <- HL)

	Stop, // Halt CPU

	Call(u16), // Call subroutine (push PC to stack and jump)
	CallC(u16, Condition), // Same but only if condition is met
	Return, // Return from subroutine
	ReturnC(Condition), // Return only if condition is met
	// No need for RST, can use Call

	PrefixCB, // Used to denote that a CB prefixed instruction is next, not a full instruction (used internally only)

	Unimplemented // For debug purposes only
}

// Jump conditions
#[derive(Debug, Copy, Clone)]
pub enum Condition {
	C, NC, Z, NZ
}

impl fmt::Display for Condition {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s: String = format!("{:?}", * self).to_lowercase();
		write!(f, "{}", s)
	}
}

/* If you think the code below is ugly:
     1. You are sane
	 2. Please, take a look at issue #1

   Returns the Executable Instruction along with the number of CPU cycles */
fn decode_opcode(opcode: u8) -> (ExInstruction, u64) {
	match opcode {
		0xCB => (ExInstruction::PrefixCB, 0),
		0x00 => (ExInstruction::Nop, 4),

		0xE2 => (ExInstruction::Load8(Reg8::MemC, Reg8::A), 8),
		0x1A => (ExInstruction::Load8(Reg8::A, Reg8::MemDE), 8),
		0xEA => (ExInstruction::Load8(Reg8::Mem(0u16), Reg8::A), 16),

		0x40 => (ExInstruction::Load8(Reg8::B, Reg8::B), 4),
		0x41 => (ExInstruction::Load8(Reg8::B, Reg8::C), 4),
		0x42 => (ExInstruction::Load8(Reg8::B, Reg8::D), 4),
		0x43 => (ExInstruction::Load8(Reg8::B, Reg8::E), 4),
		0x44 => (ExInstruction::Load8(Reg8::B, Reg8::H), 4),
		0x45 => (ExInstruction::Load8(Reg8::B, Reg8::L), 4),
		0x46 => (ExInstruction::Load8(Reg8::B, Reg8::MemHL), 8),
		0x47 => (ExInstruction::Load8(Reg8::B, Reg8::A), 4),

		0x48 => (ExInstruction::Load8(Reg8::C, Reg8::B), 4),
		0x49 => (ExInstruction::Load8(Reg8::C, Reg8::C), 4),
		0x4A => (ExInstruction::Load8(Reg8::C, Reg8::D), 4),
		0x4B => (ExInstruction::Load8(Reg8::C, Reg8::E), 4),
		0x4C => (ExInstruction::Load8(Reg8::C, Reg8::H), 4),
		0x4D => (ExInstruction::Load8(Reg8::C, Reg8::L), 4),
		0x4E => (ExInstruction::Load8(Reg8::C, Reg8::MemHL), 8),
		0x4F => (ExInstruction::Load8(Reg8::C, Reg8::A), 4),

		0x50 => (ExInstruction::Load8(Reg8::D, Reg8::B), 4),
		0x51 => (ExInstruction::Load8(Reg8::D, Reg8::C), 4),
		0x52 => (ExInstruction::Load8(Reg8::D, Reg8::D), 4),
		0x53 => (ExInstruction::Load8(Reg8::D, Reg8::E), 4),
		0x54 => (ExInstruction::Load8(Reg8::D, Reg8::H), 4),
		0x55 => (ExInstruction::Load8(Reg8::D, Reg8::L), 4),
		0x56 => (ExInstruction::Load8(Reg8::D, Reg8::MemHL), 8),
		0x57 => (ExInstruction::Load8(Reg8::D, Reg8::A), 4),

		0x58 => (ExInstruction::Load8(Reg8::E, Reg8::B), 4),
		0x59 => (ExInstruction::Load8(Reg8::E, Reg8::C), 4),
		0x5A => (ExInstruction::Load8(Reg8::E, Reg8::D), 4),
		0x5B => (ExInstruction::Load8(Reg8::E, Reg8::E), 4),
		0x5C => (ExInstruction::Load8(Reg8::E, Reg8::H), 4),
		0x5D => (ExInstruction::Load8(Reg8::E, Reg8::L), 4),
		0x5E => (ExInstruction::Load8(Reg8::E, Reg8::MemHL), 8),
		0x5F => (ExInstruction::Load8(Reg8::E, Reg8::A), 4),

		0x60 => (ExInstruction::Load8(Reg8::H, Reg8::B), 4),
		0x61 => (ExInstruction::Load8(Reg8::H, Reg8::C), 4),
		0x62 => (ExInstruction::Load8(Reg8::H, Reg8::D), 4),
		0x63 => (ExInstruction::Load8(Reg8::H, Reg8::E), 4),
		0x64 => (ExInstruction::Load8(Reg8::H, Reg8::H), 4),
		0x65 => (ExInstruction::Load8(Reg8::H, Reg8::L), 4),
		0x66 => (ExInstruction::Load8(Reg8::H, Reg8::MemHL), 8),
		0x67 => (ExInstruction::Load8(Reg8::H, Reg8::A), 4),

		0x68 => (ExInstruction::Load8(Reg8::L, Reg8::B), 4),
		0x69 => (ExInstruction::Load8(Reg8::L, Reg8::C), 4),
		0x6A => (ExInstruction::Load8(Reg8::L, Reg8::D), 4),
		0x6B => (ExInstruction::Load8(Reg8::L, Reg8::E), 4),
		0x6C => (ExInstruction::Load8(Reg8::L, Reg8::H), 4),
		0x6D => (ExInstruction::Load8(Reg8::L, Reg8::L), 4),
		0x6E => (ExInstruction::Load8(Reg8::L, Reg8::MemHL), 8),
		0x6F => (ExInstruction::Load8(Reg8::L, Reg8::A), 4),

		0x70 => (ExInstruction::Load8(Reg8::MemHL, Reg8::B), 8),
		0x71 => (ExInstruction::Load8(Reg8::MemHL, Reg8::C), 8),
		0x72 => (ExInstruction::Load8(Reg8::MemHL, Reg8::D), 8),
		0x73 => (ExInstruction::Load8(Reg8::MemHL, Reg8::E), 8),
		0x74 => (ExInstruction::Load8(Reg8::MemHL, Reg8::H), 8),
		0x75 => (ExInstruction::Load8(Reg8::MemHL, Reg8::L), 8),
		0x76 => (ExInstruction::Halt, 4),
		0x77 => (ExInstruction::Load8(Reg8::MemHL, Reg8::A), 8),

		0x78 => (ExInstruction::Load8(Reg8::A, Reg8::B), 4),
		0x79 => (ExInstruction::Load8(Reg8::A, Reg8::C), 4),
		0x7A => (ExInstruction::Load8(Reg8::A, Reg8::D), 4),
		0x7B => (ExInstruction::Load8(Reg8::A, Reg8::E), 4),
		0x7C => (ExInstruction::Load8(Reg8::A, Reg8::H), 4),
		0x7D => (ExInstruction::Load8(Reg8::A, Reg8::L), 4),
		0x7E => (ExInstruction::Load8(Reg8::A, Reg8::MemHL), 8),
		0x7F => (ExInstruction::Load8(Reg8::A, Reg8::A), 4),

		0x80 => (ExInstruction::AddA(Reg8::B), 4),
		0x81 => (ExInstruction::AddA(Reg8::C), 4),
		0x82 => (ExInstruction::AddA(Reg8::D), 4),
		0x83 => (ExInstruction::AddA(Reg8::E), 4),
		0x84 => (ExInstruction::AddA(Reg8::H), 4),
		0x85 => (ExInstruction::AddA(Reg8::L), 4),
		0x86 => (ExInstruction::AddA(Reg8::MemHL), 8),
		0x87 => (ExInstruction::AddA(Reg8::A), 4),

		0x06 => (ExInstruction::Load8Imm(Reg8::B, 0u8), 8),
		0x0E => (ExInstruction::Load8Imm(Reg8::C, 0u8), 8),
		0x16 => (ExInstruction::Load8Imm(Reg8::D, 0u8), 8),
		0x1E => (ExInstruction::Load8Imm(Reg8::E, 0u8), 8),
		0x26 => (ExInstruction::Load8Imm(Reg8::H, 0u8), 8),
		0x2E => (ExInstruction::Load8Imm(Reg8::L, 0u8), 8),
		0x36 => (ExInstruction::Load8Imm(Reg8::MemHL, 0u8), 12),
		0x3E => (ExInstruction::Load8Imm(Reg8::A, 0u8), 8),

		0xE0 => (ExInstruction::Load8(Reg8::MemH(0u8), Reg8::A), 12),
		0xF0 => (ExInstruction::Load8(Reg8::A, Reg8::MemH(0u8)), 12),

		0x01 => (ExInstruction::Load16Imm(Reg16::BC, 0u16), 12),
		0x11 => (ExInstruction::Load16Imm(Reg16::DE, 0u16), 12),
		0x21 => (ExInstruction::Load16Imm(Reg16::HL, 0u16), 12),
		0x31 => (ExInstruction::Load16Imm(Reg16::SP, 0u16), 12),

		// Bear in mind that conditional jump instructions will take
		// more CPU cycles if the jump is made
		0x18 => (ExInstruction::Jr(0i8), 8),
		0x28 => (ExInstruction::JrC(0i8, Condition::Z), 8),
		0x20 => (ExInstruction::JrC(0i8, Condition::NZ), 8),

		0x32 => (ExInstruction::LoadHLPredec, 8),
		0x22 => (ExInstruction::LoadHLPostinc, 8),

		0x90 => (ExInstruction::SubA(Reg8::B), 4),
		0x91 => (ExInstruction::SubA(Reg8::C), 4),
		0x92 => (ExInstruction::SubA(Reg8::D), 4),
		0x93 => (ExInstruction::SubA(Reg8::E), 4),
		0x94 => (ExInstruction::SubA(Reg8::H), 4),
		0x95 => (ExInstruction::SubA(Reg8::L), 4),
		0x96 => (ExInstruction::SubA(Reg8::MemHL), 8),
		0x97 => (ExInstruction::SubA(Reg8::A), 4),

		0xB8 => (ExInstruction::Compare(Reg8::B), 4),
		0xB9 => (ExInstruction::Compare(Reg8::C), 4),
		0xBA => (ExInstruction::Compare(Reg8::D), 4),
		0xBB => (ExInstruction::Compare(Reg8::E), 4),
		0xBC => (ExInstruction::Compare(Reg8::H), 4),
		0xBD => (ExInstruction::Compare(Reg8::L), 4),
		0xBE => (ExInstruction::Compare(Reg8::MemHL), 8),
		0xBF => (ExInstruction::Compare(Reg8::A), 4),

		0xAF => (ExInstruction::Xor(Reg8::A), 4),

		0x04 => (ExInstruction::Increment8(Reg8::B), 4),
		0x0C => (ExInstruction::Increment8(Reg8::C), 4),
		0x14 => (ExInstruction::Increment8(Reg8::D), 4),
		0x1C => (ExInstruction::Increment8(Reg8::E), 4),
		0x24 => (ExInstruction::Increment8(Reg8::H), 4),
		0x2C => (ExInstruction::Increment8(Reg8::L), 4),
		0x34 => (ExInstruction::Increment8(Reg8::MemHL), 12),
		0x3C => (ExInstruction::Increment8(Reg8::A), 4),

		0x05 => (ExInstruction::Decrement8(Reg8::B), 4),
		0x0D => (ExInstruction::Decrement8(Reg8::C), 4),
		0x15 => (ExInstruction::Decrement8(Reg8::D), 4),
		0x1D => (ExInstruction::Decrement8(Reg8::E), 4),
		0x25 => (ExInstruction::Decrement8(Reg8::H), 4),
		0x2D => (ExInstruction::Decrement8(Reg8::L), 4),
		0x35 => (ExInstruction::Decrement8(Reg8::MemHL), 12),
		0x3D => (ExInstruction::Decrement8(Reg8::A), 4),

		0x23 => (ExInstruction::Increment16(Reg16::HL), 8),
		0x13 => (ExInstruction::Increment16(Reg16::DE), 8),

		0x17 => (ExInstruction::Rotate(Reg8::A, true, false), 4),

		0xC5 => (ExInstruction::Push(Reg16::BC), 16),
		0xC1 => (ExInstruction::Pop(Reg16::BC), 12),

		0xCD => (ExInstruction::Call(0u16), 24),
		0xC9 => (ExInstruction::Return, 16),

		0xFE => (ExInstruction::CompareImm(0u8), 8),

		_ => (ExInstruction::Unimplemented, 0),
	}
}

/* Decodes the register argument in a CP prefixed operation
   Also returns the number of cycles of the operation, which only depends on this argument
   (wether or not it needs to access memory) */
fn decode_cb_op_reg(opcode: u8) -> (Reg8, u64) {
	match opcode & 0x07 {
		0b000 => (Reg8::B, 8),
		0b001 => (Reg8::C, 8),
		0b010 => (Reg8::D, 8),
		0b011 => (Reg8::E, 8),
		0b100 => (Reg8::H, 8),
		0b101 => (Reg8::L, 8),
		0b110 => (Reg8::MemHL, 16),
		0b111 => (Reg8::A, 8),
		_ => (Reg8::A, 8), //Never should happen because of the AND operation, but pattern matching is strict
	}
}

fn decode_cb_op_bit(opcode: u8) -> u8 {
	(opcode & 0b00111000) >> 3
}

fn decode_cb_opcode(opcode: u8) -> (ExInstruction, u64) {
	let (reg, n_cycles) = decode_cb_op_reg(opcode);

	let first_two_bits = opcode & 0xC0;
	let inst = if first_two_bits != 0 {
		let bit = decode_cb_op_bit(opcode);
		match first_two_bits {
			0x40 => ExInstruction::Bit(reg, bit),
			0x80 => ExInstruction::Res(reg, bit),
			0xC0 => ExInstruction::Set(reg, bit),
			_ => ExInstruction::Unimplemented // Should never happen
		}
	}
	else {
		match opcode & 0x38 {
			0x00 => ExInstruction::Rotate(reg,true,true),   // RLC
			0x08 => ExInstruction::Rotate(reg,false,true),  // RRC
			0x10 => ExInstruction::Rotate(reg,true,false),  // RL
			0x18 => ExInstruction::Rotate(reg,false,false), // RR
			0x20 => ExInstruction::Shift(reg,true,true),    // SLA
			0x28 => ExInstruction::Shift(reg,false,true),   // SRA
			0x30 => ExInstruction::Swap(reg),               // SwAP
			0x38 => ExInstruction::Shift(reg,false,false),  // SLL
			_ => ExInstruction::Unimplemented // Should never happen
		}
	};

	(inst, n_cycles)
}

// Retrieves the n next instructions
pub fn get_next_instructions(itct: &Interconnect, addr: u16, n_inst: u16) -> Vec<(u16, Instruction)> {
	let mut insts = Vec::<(u16, Instruction)>::new();
	let mut current_addr = addr;

	for _ in 0..n_inst {
		let next_inst = get_next_instruction(itct, current_addr);
		let next_addr = current_addr + next_inst.bytes.len() as u16;
		insts.push((current_addr, next_inst));
		current_addr = next_addr;
	}

	insts
}

// Returns an instruction along with the length of it, in order to update the PC afterwards
pub fn get_next_instruction(interconnect: &Interconnect, pc: u16) -> Instruction {
	let opcode = interconnect.read_byte(pc);
	let mut bytes: Vec<u8> = Vec::new();
	let mut n_cycles: u64 = 0;
	bytes.push(opcode);

	let (decoded, op_n_cycles) = decode_opcode(opcode);
	n_cycles += op_n_cycles;

	let inst = match decoded {
		ExInstruction::PrefixCB => {
			let second_byte = interconnect.read_byte(pc+1);
			bytes.push(second_byte);
			let (decoded_cb, cb_n_cycles) = decode_cb_opcode(second_byte);
			n_cycles += cb_n_cycles;
			decoded_cb
		},

		ExInstruction::Jr(_) => {
			let second_byte = interconnect.read_byte(pc+1);
			bytes.push(second_byte);
			let jump = second_byte as i8;
			ExInstruction::Jr(jump)
		},

		ExInstruction::JrC(_, c) => {
			let second_byte = interconnect.read_byte(pc+1);
			bytes.push(second_byte);
			let jump = second_byte as i8;
			ExInstruction::JrC(jump, c)
		},

		ExInstruction::Load8(Reg8::Mem(_), src) => {
			let (lsb, msb) = interconnect.read_2bytes(pc+1);
			bytes.push(lsb);
			bytes.push(msb);

			let b: u16 = bytes_to_u16(lsb, msb);
			ExInstruction::Load8(Reg8::Mem(b), src)
		},

		ExInstruction::Load8(dst, Reg8::Mem(_)) => {
			let (lsb, msb) = interconnect.read_2bytes(pc+1);
			bytes.push(lsb);
			bytes.push(msb);

			let b: u16 = bytes_to_u16(lsb, msb);
			ExInstruction::Load8(dst, Reg8::Mem(b))
		},

		ExInstruction::Load8(dst, Reg8::MemH(_)) => {
			let b = interconnect.read_byte(pc+1);
			bytes.push(b);

			ExInstruction::Load8(dst, Reg8::MemH(b))
		},

		ExInstruction::Load8(Reg8::MemH(_), src) => {
			let b = interconnect.read_byte(pc+1);
			bytes.push(b);

			ExInstruction::Load8(Reg8::MemH(b), src)
		},

		ExInstruction::Load8Imm(r, _) => {
			let v = interconnect.read_byte(pc+1);
			bytes.push(v);
			ExInstruction::Load8Imm(r, v)
		},

		ExInstruction::Load16Imm(r, _) => {
			let (lsb, msb) = interconnect.read_2bytes(pc+1);
			bytes.push(lsb);
			bytes.push(msb);
			let b: u16 = bytes_to_u16(lsb, msb);
			ExInstruction::Load16Imm(r, b)
		},

		ExInstruction::Call(_) => {
			let (lsb, msb) = interconnect.read_2bytes(pc+1);
			bytes.push(lsb);
			bytes.push(msb);
			let b: u16 = bytes_to_u16(lsb, msb);
			ExInstruction::Call(b)
		},

		ExInstruction::CompareImm(_) => {
			let val = interconnect.read_byte(pc+1);
			bytes.push(val);
			ExInstruction::CompareImm(val)
		},

		_ => decoded
	};

	Instruction {
		ex: inst,
		bytes: bytes,
		cycles: n_cycles
	}
}

fn bytes_to_u16(lsb: u8, msb: u8) -> u16 {
	((msb as u16) << 8) + (lsb as u16)
}
