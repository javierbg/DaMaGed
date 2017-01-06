use super::Interconnect;

#[derive(Debug)]
pub struct GB {
	cpu: Cpu,
	interconnect: Interconnect
}

impl GB {
	pub fn new(boot_rom: Box<[u8]>, cart_rom: Box<[u8]>) -> GB {
		GB {
			cpu: Cpu::default(),
			interconnect: Interconnect::new(boot_rom, cart_rom)
		}
	}
}

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

const INIT_ADDRESS: u16 = 0x0100;

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
