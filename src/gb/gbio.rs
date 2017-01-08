pub struct GBIO {
	interrupt: Interrupt,
	sound: Sound,
	joypad: Joypad,
	serial: SerialData,
	ppu: PPU,

	boot: bool
}

impl GBIO {
	pub fn new() -> GBIO {
		GBIO {
			interrupt: Interrupt::new(),
			sound: Sound{},
			joypad: Joypad{},
			serial: SerialData{},
			ppu: PPU{},

			boot: true
		}
	}

	// Determines if the system is in its boot sequence
	pub fn boot_sequence(&self) -> bool {
		self.boot
	}
}

struct Interrupt {
	interrupt_flag: u8,
	interrupt_enable: u8
}

impl Interrupt {
	pub fn new() -> Interrupt {
		Interrupt {
			interrupt_flag: 0u8,
			interrupt_enable: 0u8
		}
	}
}

struct Sound {
	// Lots of stuff
}

struct Joypad {

}

// Link cable!
struct SerialData {

}

struct PPU {

}
