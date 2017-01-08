use super::super::Interconnect;

#[derive(Debug)]
pub enum Instruction {
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
	Jr(u8), // Relative jump
	JrC(u8, Condition), // Relative jump only if condition is met
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
#[derive(Debug)]
pub enum Reg8 {
	A, B, C, D, E, H, L,
	//Memory cell pointed by...
	MemBC, MemDE, MemHL, MemSP,
	//Memory cell pointed by literal value...
	Mem(u16)
}

// 16-bit register
#[derive(Debug)]
pub enum Reg16 {
	BC, DE, HL, SP
}

// Jump conditions
#[derive(Debug)]
pub enum Condition {
	C, NC, Z, NZ
}

fn decode_opcode(opcode: u8) -> Instruction {
	match opcode {
		0x00 => Instruction::Nop,
		0x21 => Instruction::Load16Imm(Reg16::HL, 0u16),
		0x31 => Instruction::Load16Imm(Reg16::SP, 0u16),
		0x32 => Instruction::LoadHLPredec,
		0xAF => Instruction::Xor(Reg8::A),
		0xCB => Instruction::PrefixCB,
		_ => Instruction::Unimplemented
	}
}

fn decode_cb_opcode(opcode: u8) -> Instruction {
	match opcode {
		0x7c => Instruction::Bit(Reg8::H, 7),
		_ => Instruction::Unimplemented
	}
}

// Returns an instruction along with the length of it, in order to update the PC afterwards
pub fn get_next_instruction(interconnect: &Interconnect, pc: u16) -> (Instruction, u16) {
	let opcode = interconnect.read_byte(pc);
	let mut inst_length: u16 = 1;
	let decoded = decode_opcode(opcode);

	let inst = match decoded {
		Instruction::PrefixCB => {
			inst_length += 1;
			get_next_cb_instruction(interconnect, pc)
		},

		Instruction::Load16Imm(r, _) => {
			inst_length += 2;
			let lsb = interconnect.read_byte(pc+1);
			let msb = interconnect.read_byte(pc+2);
			let b: u16 = ((msb as u16) << 8) + (lsb as u16);
			Instruction::Load16Imm(r, b)
		},

		_ => decoded
	};

	(inst, inst_length)
}

fn get_next_cb_instruction(interconnect: &Interconnect, pc: u16) -> Instruction {
	let cb_opcode = interconnect.read_byte(pc+1);
	decode_cb_opcode(cb_opcode)
}
