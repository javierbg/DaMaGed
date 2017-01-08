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
        loop {
            // get instruction and instruction length from memory with pc
            let (instruction, inst_len) = instruction::get_next_instruction(interconnect, self.pc);
            // increment pc with instruction length
            // execute instruction
            match instruction {
                Instruction::Nop => {},
                Instruction::Load16Imm(r, v) => {
                    self.load_16bit_register(r, v);
                },

                Instruction::Unimplemented => {
                    panic!("Uninmplemented instruction");
                },

                _ => {
                    panic!("Instruction `{:?}' not implemented yet", instruction);
                }
            };
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
