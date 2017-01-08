pub enum Instruction {
	Load8(Reg8, Reg8),
	Load8Imm(Reg8, u8),

	Load16(Reg16, Reg16),
	Load16Imm(Reg16, u16),

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
}

// 8-bit register
pub enum Reg8 {
	A, B, C, D, E, H, L,
	//Memory cell pointed by...
	MemBC, MemDE, MemHL, MemSP,
	//Memory cell pointed by literal value...
	Mem(u16)
}

// 16-bit register
pub enum Reg16 {
	BC, DE, HL, SP
}

// Jump conditions
pub enum Condition {
	C, NC, Z, NZ
}
