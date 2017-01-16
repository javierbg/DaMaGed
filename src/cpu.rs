use interconnect::Interconnect;
use instruction;
use instruction::ExInstruction;

use std::fmt;

#[allow(dead_code)]
#[derive(Debug)]
pub struct Cpu {
    a: u8,
    f: u8,

    b: u8,
    c: u8,

    d: u8,
    e: u8,

    h: u8,
    l: u8,

    sp: u16,
    pub pc: u16,
}

// F register masks
const Z_MASK: u8 = 0x80;
const N_MASK: u8 = 0x40;
const H_MASK: u8 = 0x20;
const C_MASK: u8 = 0x10;
const Z_MASK_NEG: u8 = !Z_MASK;
const N_MASK_NEG: u8 = !N_MASK;
const H_MASK_NEG: u8 = !H_MASK;
const C_MASK_NEG: u8 = !C_MASK;

enum Flag {
    Z, N, H, C
}

const INIT_ADDRESS: u16 = 0x0000;

impl Default for Cpu {
	fn default() -> Cpu {
		Cpu {
            // 1 byte 0xdeadbeef ?
            a: 0x42,
            f: 0x42,

            b: 0x42,
            c: 0x42,

            d: 0x42,
            e: 0x42,

            h: 0x42,
            l: 0x42,

            sp: 0x4221,

            pc: INIT_ADDRESS,
        }
	}
}

impl fmt::Display for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // This must be the ugliest write ever created
        // There must be a nicer way to do this, but I've been looking for it
        // for too long and I just want to advance with this.
        // If you are reading this and know a better way to this (which must exist)
        // then please, submit a pull request.
        write!(f, "A: {:02X} |\nB: {:02X} | C: {:02X}\nD: {:02X} | E: {:02X}\nH: {:02X} | L: {:02X}\n-----------------\nSP: {:04X}\nPC: {:04X}\n\nF: {} {} {} {} {:04b}\n   Z N H C ----",
               self.a, self.b, self.c, self.d, self.e, self.h, self.l, self.sp, self.pc,
               if self.f & Z_MASK == 0 {"0"} else {"1"},
               if self.f & N_MASK == 0 {"0"} else {"1"},
               if self.f & H_MASK == 0 {"0"} else {"1"},
               if self.f & C_MASK == 0 {"0"} else {"1"},
               self.f & 0x0F)

        /* It should look something like this:
        A: 0xFF |
        B: 0xFF | C: 0xFF
        D: 0xFF | E: 0xFF
        H: 0xFF | L: 0xFF
        -----------------
        SP: 0xFFFF
        PC: 0xFFFF

        F: 1 0 1 0 1111
           Z N H C ----
        */
    }
}

#[allow(dead_code)]
impl Cpu {

    pub fn run(&mut self, itct: &mut Interconnect) {
        loop {
            self.step(itct);
        }
    }

    pub fn step(&mut self, itct: &mut Interconnect) -> instruction::Instruction{
        // get instruction and instruction length from memory with pc
        let inst_addr = self.pc;
        let next_instruction = instruction::get_next_instruction(itct, inst_addr);
        let mut additional_cycles: u32 = 0; // For conditinal jumps/calls/rets, how many extra
                                            // cpu cycles it will take

        // increment pc with instruction length
        self.pc = self.pc.wrapping_add(next_instruction.bytes.len() as u16);

        // execute instruction
        match next_instruction.ex {
            ExInstruction::Nop => {},

            ExInstruction::Load8(dst, src) => {
                let val = self.read_8bit_register(itct, src);
                self.load_8bit_register(itct, dst, val);
            }

            ExInstruction::Load8Imm(r, v) => {
                self.load_8bit_register(itct, r, v);
            },

            ExInstruction::Load16Imm(r, v) => {
                self.load_16bit_register(r, v);
            },

            ExInstruction::LoadHLPredec => {
                let a: u8 = self.a;
                let hl = self.read_16bit_register(Reg16::HL).wrapping_sub(1u16);
                self.load_16bit_register(Reg16::HL, hl);
                self.load_8bit_register(itct, Reg8::MemHL, a);
            },

            ExInstruction::LoadHLPostinc => {
                let a: u8 = self.a;
                self.load_8bit_register(itct, Reg8::MemHL, a);
                let hl = self.read_16bit_register(Reg16::HL).wrapping_add(1u16);
                self.load_16bit_register(Reg16::HL, hl);
            },

            ExInstruction::Xor(r) => {
                let newval = self.a ^ self.read_8bit_register(itct, r);
                self.f = if newval == 0x00 { 0x80 } else { 0x00 };
                self.a = newval;
            },

            ExInstruction::Increment8(r) => {
                let val = self.read_8bit_register(itct, r);
                let newval = self.add_update_flags(val, 1);
                self.load_8bit_register(itct, r, newval);
            },

            ExInstruction::Decrement8(r) => {
                let val = self.read_8bit_register(itct, r);
                let newval = self.sub_update_flags(val, 1);
                self.load_8bit_register(itct, r, newval);
            },

            ExInstruction::Increment16(r) => {
                let val = self.read_16bit_register(r);
                // Do NOT update flags
                let newval = val.wrapping_add(1);
                self.load_16bit_register(r, newval);
            },

            ExInstruction::Rotate(reg, dir, carry) => {
                let old_val = self.read_8bit_register(itct, reg);
                let mut new_val;

                if carry { // with carry
                    if dir { // left (rlc)
                        if (old_val & 0x80) == 0 {
                            self.reset_flag(Flag::C);
                        } else {
                            self.set_flag(Flag::C);
                        };

                        new_val = old_val.rotate_right(1);
                    } else { // right (rrc)
                        if (old_val & 0x01) == 0 {
                            self.reset_flag(Flag::C);
                        } else {
                            self.set_flag(Flag::C);
                        };

                        new_val = old_val.rotate_left(1);
                    }
                } else { // through carry
                    let carry_set = self.get_flag(Flag::C);

                    if dir { // left (rl)
                        new_val = old_val << 1;
                        if carry_set {
                            new_val += 0x01;
                        }

                        if (old_val & 0x80) == 0 {
                            self.reset_flag(Flag::C);
                        } else {
                            self.set_flag(Flag::C);
                        };
                    } else { // right (rr)
                        new_val = old_val >> 1;
                        if carry_set {
                            new_val += 0x80;
                        }

                        if (old_val & 0x01) == 0 {
                            self.reset_flag(Flag::C);
                        } else {
                            self.set_flag(Flag::C);
                        };
                    }
                }

                if new_val == 0 {
                    self.set_flag(Flag::Z);
                } else {
                    self.reset_flag(Flag::Z);
                }

                self.load_8bit_register(itct, reg, new_val);
            },

            ExInstruction::Bit(r, b) => {
                let v = self.read_8bit_register(itct, r);
                let bit = (v >> b) & 0x01;

                if bit == 0x00 {
                    self.set_flag(Flag::Z);
                } else {
                    self.reset_flag(Flag::Z);
                };
                self.reset_flag(Flag::N);
                self.set_flag(Flag::H);
            },

            ExInstruction::Jr(j) => {
                self.pc = self.pc.wrapping_add(j as u16);
            },

            ExInstruction::JrC(j, c) => {
                if self.cond(c) {
                    self.pc = self.pc.wrapping_add(j as u16);
                    additional_cycles = 4;
                }
            },

            ExInstruction::Call(a) => {
                self.push(itct, Reg16::PC);
                self.pc = a;
            },

            ExInstruction::Return => {
                self.pop(itct, Reg16::PC);
            },

            ExInstruction::Pop(r) => {
                self.pop(itct, r);
            },

            ExInstruction::Push(r) => {
                self.push(itct, r);
            },

            ExInstruction::CompareImm(val) => {
                let acc = self.a;
                self.sub_update_flags(acc, val);
            },

            ExInstruction::Unimplemented => {
                panic!("Uninmplemented instruction {:02X} at address {:04X}", next_instruction.bytes[0], inst_addr);
            },

            _ => {
                panic!("Instruction `{:?}' not implemented yet", next_instruction.ex);
            }
        };

        instruction::Instruction {
            ex: next_instruction.ex,
            bytes: next_instruction.bytes,
            cycles: next_instruction.cycles + additional_cycles
        }
    }

    pub fn read_8bit_register(&self, interconnect: &Interconnect, reg: Reg8) -> u8 {
        match reg {
            Reg8::A => self.a,
            Reg8::B => self.b,
            Reg8::C => self.c,
            Reg8::D => self.d,
            Reg8::E => self.e,
            Reg8::H => self.h,
            Reg8::L => self.l,
            Reg8::F => self.f,

            Reg8::MemBC => interconnect.read_byte(self.read_16bit_register(Reg16::BC)),
            Reg8::MemDE => interconnect.read_byte(self.read_16bit_register(Reg16::DE)),
            Reg8::MemHL => interconnect.read_byte(self.read_16bit_register(Reg16::HL)),
            Reg8::MemSP => interconnect.read_byte(self.sp),

            Reg8::MemC  => interconnect.read_byte(0xFF00u16 + (self.c as u16)),

            Reg8::Mem(addr) => interconnect.read_byte(addr),
            Reg8::MemH(addr) => interconnect.read_byte(0xFF00u16 + (addr as u16)),
        }
    }

    fn load_8bit_register(&mut self, interconnect: &mut Interconnect, reg: Reg8, val: u8) {
        match reg {
            Reg8::A => self.a = val,
            Reg8::B => self.b = val,
            Reg8::C => self.c = val,
            Reg8::D => self.d = val,
            Reg8::E => self.e = val,
            Reg8::H => self.h = val,
            Reg8::L => self.l = val,
            Reg8::F => self.f = val,

            Reg8::MemBC => interconnect.write_byte(self.read_16bit_register(Reg16::BC), val),
            Reg8::MemDE => interconnect.write_byte(self.read_16bit_register(Reg16::DE), val),
            Reg8::MemHL => interconnect.write_byte(self.read_16bit_register(Reg16::HL), val),
            Reg8::MemSP => interconnect.write_byte(self.sp, val),

            Reg8::MemC  => interconnect.write_byte(0xFF00u16 + (self.c as u16), val),

            Reg8::Mem(addr) => interconnect.write_byte(addr, val),
            Reg8::MemH(addr) => interconnect.write_byte(0xFF00u16 + (addr as u16), val),
        };
    }

    pub fn read_16bit_register(&self, reg: Reg16) -> u16 {
        match reg {
            Reg16::BC => ((self.b as u16) << 8) + (self.c as u16),
            Reg16::DE => ((self.d as u16) << 8) + (self.e as u16),
            Reg16::HL => ((self.h as u16) << 8) + (self.l as u16),
            Reg16::SP => self.sp,
            Reg16::PC => self.pc,
        }
    }

    fn load_16bit_register(&mut self, reg: Reg16, val: u16) {
        let msb = (val >> 8) as u8;
        let lsb = val as u8;

        match reg {
            Reg16::BC => {
                self.b = msb;
                self.c = lsb;
            },
            Reg16::DE => {
                self.d = msb;
                self.e = lsb;
            },
            Reg16::HL => {
                self.h = msb;
                self.l = lsb;
            },
            Reg16::SP => {
                self.sp = val;
            },
            Reg16::PC => {
                self.pc = val;
            }
        };
    }

    fn set_flag(&mut self, flg: Flag) {
        match flg {
            Flag::Z => self.f = self.f | Z_MASK,
            Flag::N => self.f = self.f | N_MASK,
            Flag::H => self.f = self.f | H_MASK,
            Flag::C => self.f = self.f | C_MASK,
        }
    }

    fn reset_flag(&mut self, flg: Flag) {
        match flg {
            Flag::Z => self.f = self.f & Z_MASK_NEG,
            Flag::N => self.f = self.f & N_MASK_NEG,
            Flag::H => self.f = self.f & H_MASK_NEG,
            Flag::C => self.f = self.f & C_MASK_NEG,
        }
    }

    fn get_flag(&self, flg: Flag) -> bool {
        match flg {
            Flag::Z => self.f & Z_MASK != 0,
            Flag::N => self.f & N_MASK != 0,
            Flag::H => self.f & H_MASK != 0,
            Flag::C => self.f & C_MASK != 0,
        }
    }

    // Determines if a specific condition is true
    fn cond(&self, condition: instruction::Condition) -> bool {
        match condition {
            instruction::Condition::C  => (self.f & C_MASK) != 0x00,
            instruction::Condition::NC => (self.f & C_MASK) == 0x00,
            instruction::Condition::Z  => (self.f & Z_MASK) != 0x00,
            instruction::Condition::NZ => (self.f & Z_MASK) == 0x00,
        }
    }

    // Push a 16 bit value to the stack
    fn push(&mut self, itct: &mut Interconnect, reg: Reg16) {
        let hi_addr = self.sp.wrapping_sub(1);
        let lo_addr = self.sp.wrapping_sub(2);

        match reg {
            Reg16::BC => {
                itct.write_byte(hi_addr, self.b);
                itct.write_byte(lo_addr, self.c);
            },
            Reg16::DE => {
                itct.write_byte(hi_addr, self.d);
                itct.write_byte(lo_addr, self.e);
            },
            Reg16::HL => {
                itct.write_byte(hi_addr, self.h);
                itct.write_byte(lo_addr, self.l);
            },
            Reg16::SP => {
                let hi_byte = (self.sp >> 8) as u8;
                let lo_byte = self.sp as u8;
                itct.write_byte(hi_addr, hi_byte);
                itct.write_byte(lo_addr, lo_byte);
            },
            Reg16::PC => { // For the sake of completion? But maybe I sould just use _
                let hi_byte = (self.pc >> 8) as u8;
                let lo_byte = self.pc as u8;
                itct.write_byte(hi_addr, hi_byte);
                itct.write_byte(lo_addr, lo_byte);
            },
        }

        self.sp = lo_addr;
    }

    // Pop a 16 bit value from the stack
    fn pop(&mut self, itct: &mut Interconnect, reg: Reg16) {
        let lo_addr = self.sp;
        let hi_addr = self.sp.wrapping_add(1);

        let lo_byte = itct.read_byte(lo_addr);
        let hi_byte = itct.read_byte(hi_addr);

        match reg {
            Reg16::BC => {
                self.b = hi_byte;
                self.c = lo_byte;
            },
            Reg16::DE => {
                self.d = hi_byte;
                self.e = lo_byte;
            },
            Reg16::HL => {
                self.h = hi_byte;
                self.l = lo_byte;
            },
            Reg16::PC => {
                self.pc = ((hi_byte as u16) << 8) + (lo_byte as u16);
            },
            _ => {},
        }

        self.sp = self.sp.wrapping_add(2);
    }

    // Adds two 8-bit numbers and marks the flags acordingly
    fn add_update_flags(&mut self, a: u8, b: u8) -> u8 {
        let (result,c_flag) = a.overflowing_add(b);
        let h_flag = (((a & 0x0F) + (b & 0x0F)) & 0xF0) != 0;
        let z_flag = result == 0;

        self.reset_flag(Flag::N);

        if c_flag {
            self.set_flag(Flag::C);
        } else {
            self.reset_flag(Flag::C);
        }

        if h_flag {
            self.set_flag(Flag::H);
        } else {
            self.reset_flag(Flag::H);
        }

        if z_flag {
            self.set_flag(Flag::Z);
        } else {
            self.reset_flag(Flag::Z);
        }

        result
    }

    // Subracts two 8-bit numbers (a-b) and marks the flags acordingly
    // TODO: Check if this is correct
    fn sub_update_flags(&mut self, a: u8, b: u8) -> u8 {
        // Two's complement
        let negative_b = (!b).wrapping_add(1);
        self.set_flag(Flag::N);

        self.add_update_flags(a, negative_b)
    }
}

// 8-bit register
#[derive(Debug, Copy, Clone)]
pub enum Reg8 {
	A, B, C, D, E, F, H, L,
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
	BC, DE, HL, SP, PC
}

impl fmt::Display for Reg16 {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s: String = format!("{:?}", * self).to_lowercase();
		write!(f, "{}", s)
	}
}

#[derive(Clone, Copy)]
pub enum Register {
	Register8(Reg8),
	Register16(Reg16),
}