use std::fmt;

const RAM_SIZE: usize = 8 * 1024; // 8K
const VRAM_SIZE: usize = 8 * 1024; // 8K

#[allow(dead_code)]
pub struct Interconnect{
	ram: [u8 ; RAM_SIZE],
	vram: [u8 ; VRAM_SIZE]
}

impl Default for Interconnect {
	fn default() -> Interconnect {
		Interconnect {
			ram: [3u8 ; RAM_SIZE],
			vram: [3u8 ; VRAM_SIZE]
		}
	}
}

impl fmt::Debug for Interconnect {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
		write!(f, "HERE BE RAM")
	}
}
