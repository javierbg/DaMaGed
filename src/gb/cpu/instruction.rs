use super::super::Interconnect;
use std::fmt;
use std::iter;

// Whole data of an instruction. Because it holds the original bytes, it can be
// dissassembled.
pub struct Instruction {
	pub ex: ExInstruction,
	pub bytes: Vec<u8>,
}

impl Instruction {
	fn mnemonic(&self) -> String {
		let ex = &self.ex;
		match ex {
			&ExInstruction::Nop => "nop".into(),
			&ExInstruction::Load8(dst, src) => format!("ld {},{}",dst,src),
			&ExInstruction::Load8Imm(dst, val) => format!("ld {},0{:02x}h",dst,val),
			&ExInstruction::Load16(dst, src) => format!("ld {},{}",dst,src),
			&ExInstruction::Load16Imm(dst, val) => format!("ld {},0{:04x}h",dst,val),
			&ExInstruction::JrC(jr, cond) => format!("jr {},${}",cond,jr),
			&ExInstruction::LoadHLPredec => "ld (-hl),a".into(),
			&ExInstruction::Xor(r) => format!("xor {}", r),
			&ExInstruction::Increment(r) => format!("inc {}",r),

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

	Increment(Reg8), Decrement(Reg8),

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
	Inc16(Reg16),
	Dec16(Reg16),

	// Rotate and Shift
	Rlca, // Rotate Left A and Carry
	Rla,  // Rotate Left A
	Rrca, Rra, // More of the same

	Rlc(Reg8), // Rotate Left an 8-bit register
	Rl(Reg8),

	Rrc(Reg8), Rr(Reg8),

	Sla(Reg8), Sra(Reg8), // Arithmetic shift
	Srl(Reg8), // Logical shift (only right bc SLL would do the same as SLA)

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
	Ret, // Return from subroutine
	RetC(Condition), // Return only if condition is met
	// No need for RST, can use Call

	PrefixCB, // Used to denote that a CB prefixed instruction is next, not a full instruction (used internally only)

	Unimplemented // For debug purposes only
}


// 8-bit register
#[derive(Debug, Copy, Clone)]
pub enum Reg8 {
	A, B, C, D, E, H, L,
	//Memory cell pointed by...
	MemBC, MemDE, MemHL, MemSP,
	//Memory cell pointed by 0xFF00 + ...
	MemC,
	//Memory cell pointed by literal value...
	Mem(u16),
	//Memory cell at zero page (high ram) pointed by literal value of...
	MemH(u8)
}

impl fmt::Display for Reg8 {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s: String = match *self {
			Reg8::MemBC => "(bc)".into(),
			Reg8::MemDE => "(de)".into(),
			Reg8::MemHL => "(hl)".into(),
			Reg8::MemSP => "(sp)".into(),
			Reg8::MemC  => "(0ff00h+c)".into(),
			Reg8::Mem(a) => format!("(0{:04x}h)", a),
			Reg8::MemH(a) => format!("(0ff00h+0{:02x}h)", a),
			_ => format!("{:?}", *self)
		};
		write!(f, "{}", s.to_lowercase())
	}
}

// 16-bit register
#[derive(Debug, Copy, Clone)]
pub enum Reg16 {
	BC, DE, HL, SP
}

impl fmt::Display for Reg16 {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s: String = format!("{:?}", * self).to_lowercase();
		write!(f, "{}", s)
	}
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

fn decode_opcode(opcode: u8) -> ExInstruction {
	match opcode {
		0x00 => ExInstruction::Nop,
		0xE2 => ExInstruction::Load8(Reg8::MemC, Reg8::A),
		0x47 => ExInstruction::Load8(Reg8::B, Reg8::A),
		0x77 => ExInstruction::Load8(Reg8::MemHL, Reg8::A),
		0xE0 => ExInstruction::Load8(Reg8::MemH(0u8), Reg8::A),
		0x0E => ExInstruction::Load8Imm(Reg8::C, 0u8),
		0x3E => ExInstruction::Load8Imm(Reg8::A, 0u8),
		0x11 => ExInstruction::Load16Imm(Reg16::DE, 0u16),
		0x21 => ExInstruction::Load16Imm(Reg16::HL, 0u16),
		0x31 => ExInstruction::Load16Imm(Reg16::SP, 0u16),
		0x20 => ExInstruction::JrC(0i8, Condition::NZ),
		0x32 => ExInstruction::LoadHLPredec,
		0xAF => ExInstruction::Xor(Reg8::A),
		0x0C => ExInstruction::Increment(Reg8::C),
		0xCB => ExInstruction::PrefixCB,
		_ => ExInstruction::Unimplemented
	}
}

fn decode_cb_opcode(opcode: u8) -> ExInstruction {
	match opcode {
		0x7c => ExInstruction::Bit(Reg8::H, 7),
		_ => ExInstruction::Unimplemented
	}
}

// Returns an instruction along with the length of it, in order to update the PC afterwards
pub fn get_next_instruction(interconnect: &Interconnect, pc: u16) -> Instruction {
	let opcode = interconnect.read_byte(pc);
	let mut bytes: Vec<u8> = Vec::new();
	bytes.push(opcode);

	let decoded = decode_opcode(opcode);

	let inst = match decoded {
		ExInstruction::PrefixCB => {
			let second_byte = interconnect.read_byte(pc+1);
			bytes.push(second_byte);
			decode_cb_opcode(second_byte)
		},

		ExInstruction::JrC(_, c) => {
			let second_byte = interconnect.read_byte(pc+1);
			bytes.push(second_byte);
			let jump = second_byte as i8;
			ExInstruction::JrC(jump, c)
		},

		ExInstruction::Load8(dst, Reg8::Mem(_)) => {
			let lsb = interconnect.read_byte(pc+1);
			let msb = interconnect.read_byte(pc+2);
			bytes.push(lsb);
			bytes.push(msb);

			let b: u16 = ((msb as u16) << 8) + (lsb as u16);
			ExInstruction::Load8(dst, Reg8::Mem(b))
		},

		ExInstruction::Load8(dst, Reg8::MemH(_)) => {
			let b = interconnect.read_byte(pc+1);
			bytes.push(b);

			ExInstruction::Load8(dst, Reg8::MemH(b))
		}

		ExInstruction::Load8(Reg8::MemH(_), src) => {
			let b = interconnect.read_byte(pc+1);
			bytes.push(b);

			ExInstruction::Load8(Reg8::MemH(b), src)
		}

		ExInstruction::Load8Imm(r, _) => {
			let v = interconnect.read_byte(pc+1);
			bytes.push(v);
			ExInstruction::Load8Imm(r, v)
		},

		ExInstruction::Load16Imm(r, _) => {
			let lsb = interconnect.read_byte(pc+1);
			let msb = interconnect.read_byte(pc+2);
			bytes.push(lsb);
			bytes.push(msb);
			let b: u16 = ((msb as u16) << 8) + (lsb as u16);
			ExInstruction::Load16Imm(r, b)
		},

		_ => decoded
	};

	Instruction {
		ex: inst,
		bytes: bytes
	}
}
