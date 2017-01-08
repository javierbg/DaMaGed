use super::super::Interconnect;

#[allow(dead_code)]
#[derive(Debug)]
pub struct Cpu {
    a: u8,
    f: u8,

    b: u8,
    c: u8,

    d: u8,
    e: u8,

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
            // increment pc with instruction length
            // execute instruction
        }
    }
}
