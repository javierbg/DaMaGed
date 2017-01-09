use super::super::Interconnect;
use super::instruction;
use super::instruction::Instruction;

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
        let mut new_flags: u8 = self.f;

        loop {
            println!("{:?}", self);
            // get instruction and instruction length from memory with pc
            let (instruction, inst_len) = instruction::get_next_instruction(itct, self.pc);
            println!("{:04X} : {:?}", self.pc, instruction);

            // increment pc with instruction length
            self.pc = self.pc.wrapping_add(inst_len);

            // execute instruction
            match instruction {
                Instruction::Nop => {},

                Instruction::Load8(dst, src) => {
                    let val = self.read_8bit_register(itct, src);
                    self.load_8bit_register(itct, dst, val);
                }

                Instruction::Load8Imm(r, v) => {
                    self.load_8bit_register(itct, r, v);
                },

                Instruction::Load16Imm(r, v) => {
                    self.load_16bit_register(r, v);
                },

                Instruction::LoadHLPredec => {
                    let a: u8 = self.a;
                    let hl = self.read_16bit_register(instruction::Reg16::HL).wrapping_sub(1u16);
                    self.load_16bit_register(instruction::Reg16::HL, hl);
                    self.load_8bit_register(itct, instruction::Reg8::MemHL, a);
                },

                Instruction::Xor(r) => {
                    let newval = self.a ^ self.read_8bit_register(itct, r);
                    new_flags = if newval == 0x00 { 0x80 } else { 0x00 };
                    self.a = newval;
                },

                Instruction::Increment(r) => {
                    let val = self.read_8bit_register(itct, r);
                    let newval = val.wrapping_add(1u8);
                    self.load_8bit_register(itct, r, newval);

                    // Zero flag
                    new_flags = if newval == 0  {new_flags | 0x80}  else {new_flags & 0x70};
                    // Half carry flag (is there a better way?)
                    new_flags = if (((val & 0x0F) + 1u8) & 0xF0) != 0 {new_flags | 0x20} else {new_flags & 0xD0};
                },

                Instruction::Bit(r, b) => {
                    let v = self.read_8bit_register(itct, r);
                    let bit = (v >> b) & 0x01;
                    let old_flags = new_flags & 0x10; // Set ZNH to 0
                    new_flags = if bit == 0x00 { old_flags | (0xA0) } else { old_flags | (0x20) };
                },

                Instruction::JrC(j, c) => {
                    if self.cond(c) {
                        self.pc = self.pc.wrapping_add(j as u16);
                    }
                },

                Instruction::Unimplemented => {
                    panic!("Uninmplemented instruction");
                },

                _ => {
                    panic!("Instruction `{:?}' not implemented yet", instruction);
                }
            };

            //Update flags
            self.f = new_flags;
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

    // Determines if a specific condition is true
    fn cond(&self, condition: instruction::Condition) -> bool {
        match condition {
            instruction::Condition::C  => (self.f & 0x10) != 0x00,
            instruction::Condition::NC => (self.f & 0x10) == 0x00,
            instruction::Condition::Z  => (self.f & 0x80) != 0x00,
            instruction::Condition::NZ => (self.f & 0x80) == 0x00,
        }
    }
}
