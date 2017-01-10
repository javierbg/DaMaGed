use super::super::Interconnect;
use super::instruction;
use super::instruction::ExInstruction;

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
    pc: u16,
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

#[allow(dead_code)]
impl Cpu {
    pub fn run(&mut self, itct: &mut Interconnect) {

        loop {
            //println!("{:?}", self);
            // get instruction and instruction length from memory with pc
            let instruction = instruction::get_next_instruction(itct, self.pc);
            println!("{:04X} : {}", self.pc, instruction);

            // increment pc with instruction length
            self.pc = self.pc.wrapping_add(instruction.bytes.len() as u16);

            // execute instruction
            match instruction.ex {
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
                    let hl = self.read_16bit_register(instruction::Reg16::HL).wrapping_sub(1u16);
                    self.load_16bit_register(instruction::Reg16::HL, hl);
                    self.load_8bit_register(itct, instruction::Reg8::MemHL, a);
                },

                ExInstruction::Xor(r) => {
                    let newval = self.a ^ self.read_8bit_register(itct, r);
                    self.f = if newval == 0x00 { 0x80 } else { 0x00 };
                    self.a = newval;
                },

                ExInstruction::Increment(r) => {
                    let val = self.read_8bit_register(itct, r);
                    let newval = val.wrapping_add(1u8);
                    self.load_8bit_register(itct, r, newval);

                    // Zero flag
                    if newval == 0  {
                        self.set_flag(Flag::Z);
                    }  else {
                        self.reset_flag(Flag::Z);
                    };
                    // Half carry flag (is there a better way?)
                    if (((val & 0x0F) + 1u8) & 0xF0) != 0 {
                        self.set_flag(Flag::H);
                    } else {
                        self.reset_flag(Flag::H);
                    };
                },

                ExInstruction::Rotate(reg, dir, carry) => {
                    println!("{:?}", self);
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

                    println!("{:?}", self);
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

                ExInstruction::JrC(j, c) => {
                    if self.cond(c) {
                        self.pc = self.pc.wrapping_add(j as u16);
                    }
                },

                ExInstruction::Call(a) => {
                    let hb = (self.pc >> 8) as u8;
                    let lb = self.pc as u8;

                    let ha = self.sp.wrapping_sub(1);
                    let la = self.sp.wrapping_sub(2);
                    itct.write_byte(ha, hb);
                    itct.write_byte(la, lb);

                    self.sp = la;
                    self.pc = a;
                }

                ExInstruction::Push(r) => {
                    self.push(itct, r);
                }

                ExInstruction::Unimplemented => {
                    panic!("Uninmplemented instruction");
                },

                _ => {
                    panic!("Instruction `{:?}' not implemented yet", instruction.ex);
                }
            };
        }
    }

    fn read_8bit_register(&self, interconnect: &Interconnect, reg: instruction::Reg8) -> u8 {
        match reg {
            instruction::Reg8::A => self.a,
            instruction::Reg8::B => self.b,
            instruction::Reg8::C => self.c,
            instruction::Reg8::D => self.d,
            instruction::Reg8::E => self.e,
            instruction::Reg8::H => self.h,
            instruction::Reg8::L => self.l,

            instruction::Reg8::MemBC => interconnect.read_byte(self.read_16bit_register(instruction::Reg16::BC)),
            instruction::Reg8::MemDE => interconnect.read_byte(self.read_16bit_register(instruction::Reg16::DE)),
            instruction::Reg8::MemHL => interconnect.read_byte(self.read_16bit_register(instruction::Reg16::HL)),
            instruction::Reg8::MemSP => interconnect.read_byte(self.sp),

            instruction::Reg8::MemC  => interconnect.read_byte(0xFF00u16 + (self.c as u16)),

            instruction::Reg8::Mem(addr) => interconnect.read_byte(addr),
            instruction::Reg8::MemH(addr) => interconnect.read_byte(0xFF00u16 + (addr as u16)),
        }
    }

    fn load_8bit_register(&mut self, interconnect: &mut Interconnect, reg: instruction::Reg8, val: u8) {
        match reg {
            instruction::Reg8::A => self.a = val,
            instruction::Reg8::B => self.b = val,
            instruction::Reg8::C => self.c = val,
            instruction::Reg8::D => self.d = val,
            instruction::Reg8::E => self.e = val,
            instruction::Reg8::H => self.h = val,
            instruction::Reg8::L => self.l = val,

            instruction::Reg8::MemBC => interconnect.write_byte(self.read_16bit_register(instruction::Reg16::BC), val),
            instruction::Reg8::MemDE => interconnect.write_byte(self.read_16bit_register(instruction::Reg16::DE), val),
            instruction::Reg8::MemHL => interconnect.write_byte(self.read_16bit_register(instruction::Reg16::HL), val),
            instruction::Reg8::MemSP => interconnect.write_byte(self.sp, val),

            instruction::Reg8::MemC  => interconnect.write_byte(0xFF00u16 + (self.c as u16), val),

            instruction::Reg8::Mem(addr) => interconnect.write_byte(addr, val),
            instruction::Reg8::MemH(addr) => interconnect.write_byte(0xFF00u16 + (addr as u16), val),
        };
    }

    fn read_16bit_register(&self, reg: instruction::Reg16) -> u16 {
        match reg {
            instruction::Reg16::BC => ((self.b as u16) << 8) + (self.c as u16),
            instruction::Reg16::DE => ((self.d as u16) << 8) + (self.e as u16),
            instruction::Reg16::HL => ((self.h as u16) << 8) + (self.l as u16),
            instruction::Reg16::SP => self.sp,
        }
    }

    fn load_16bit_register(&mut self, reg: instruction::Reg16, val: u16) {
        let msb = (val >> 8) as u8;
        let lsb = val as u8;

        match reg {
            instruction::Reg16::BC => {
                self.b = msb;
                self.c = lsb;
            },
            instruction::Reg16::DE => {
                self.d = msb;
                self.e = lsb;
            },
            instruction::Reg16::HL => {
                self.h = msb;
                self.l = lsb;
            },
            instruction::Reg16::SP => {
                self.sp = val;
            },
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
            instruction::Condition::C  => (self.f & 0x10) != 0x00,
            instruction::Condition::NC => (self.f & 0x10) == 0x00,
            instruction::Condition::Z  => (self.f & 0x80) != 0x00,
            instruction::Condition::NZ => (self.f & 0x80) == 0x00,
        }
    }

    // Push a 16 bit value to the stack
    fn push(&mut self, itct: &mut Interconnect, reg: instruction::Reg16) {
        let ha = self.sp.wrapping_sub(1);
        let la = self.sp.wrapping_sub(2);

        match reg {
            instruction::Reg16::BC => {
                itct.write_byte(ha, self.b);
                itct.write_byte(la, self.c);
            },
            instruction::Reg16::DE => {
                itct.write_byte(ha, self.d);
                itct.write_byte(la, self.e);
            },
            instruction::Reg16::HL => {
                itct.write_byte(ha, self.h);
                itct.write_byte(la, self.l);
            },
            instruction::Reg16::SP => {
                let hb = (self.sp >> 8) as u8;
                let lb = self.sp as u8;
                itct.write_byte(ha, hb);
                itct.write_byte(la, lb);
            },
        }

        self.sp = la;
    }
}
