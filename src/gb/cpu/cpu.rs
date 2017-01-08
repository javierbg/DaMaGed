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
    pub fn run(&mut self, interconnect: &mut Interconnect) {
        let mut new_flags: u8 = self.f;

        loop {
            println!("{:?}", self);
            // get instruction and instruction length from memory with pc
            let (instruction, inst_len) = instruction::get_next_instruction(interconnect, self.pc);
            println!("{:04X} : {:?}", self.pc, instruction);

            // increment pc with instruction length
            self.pc += inst_len;

            // execute instruction
            match instruction {
                Instruction::Nop => {},
                Instruction::Load16Imm(r, v) => {
                    self.load_16bit_register(r, v);
                },

                Instruction::Xor(r) => {
                    let newval = self.a ^ self.read_8bit_register(interconnect, r);
                    new_flags = if newval == 0x00 { 0x80 } else { 0x00 };
                    self.a = newval;
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

            instruction::Reg8::Mem(addr) => interconnect.read_byte(addr),
        }
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
                self.l = msb;
            },
            instruction::Reg16::SP => {
                self.sp = val;
            },
        };
    }
}
