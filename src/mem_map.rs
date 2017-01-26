// Turns out that this is just Bank0 of the cartridge, I'll leave it here for now anyway
/*const RESTART_INTERRUPT_START: u16 = 0x0000;
const RESTART_INTERRUPT_LENGTH: u16 = 0x0100;
const RESTAR_INTERRUPT_END: u16 = RESTART_INTERRUPT_START + RESTART_INTERRUPT_LENGTH - 1;

const CARTRIDGE_HEADER_START: u16 = 0x0100;
const CARTRIDGE_HEADER_LENGTH: u16 = 0x0050;
const CARTRIDGE_HEADER_END: u16 = CARTRIDGE_HEADER_START + CARTRIDGE_HEADER_LENGTH - 1;*/

pub const CARTRIDGE_BANK_LENGTH: u16 = 0x4000;

pub const CARTRIDGE_ROM_START: u16 = 0x0000;
pub const CARTRIDGE_ROM_END  : u16 = CARTRIDGE_ROM_START + (CARTRIDGE_BANK_LENGTH * 2) - 1;

pub const CARTRIDGE_BANK0_START: u16 = CARTRIDGE_ROM_START;
pub const CARTRIDGE_BANK0_END  : u16 = CARTRIDGE_BANK0_START + CARTRIDGE_BANK_LENGTH - 1;

pub const CARTRIDGE_BANKN_START: u16 = CARTRIDGE_BANK0_END + 1;
pub const CARTRIDGE_BANKN_END  : u16 = CARTRIDGE_ROM_END;

// This down here is Video RAM or VRAM for short
// Turns out that the mapping can get really funky, so as far as memory mapping goes, we should
// only deal with one big chunk of RAM.
/*// Tile RAM, also known as Character RAM
const TILE_RAM_START: u16 = 0x8000;
pub const TILE_RAM_LENGTH: u16 = 0x0800;
const TILE_RAM_END: u16 = TILE_RAM_START + TILE_RAM_LENGTH - 1;

// Background data (2 of them)
pub const BACKGROUND_MAP_LENGTH: u16 = 0x0400;

const BACKGROUND_MAP1_START: u16 = 0x9800;
const BACKGROUND_MAP1_END: u16 = BACKGROUND_MAP1_START + BACKGROUND_MAP_LENGTH - 1;

const BACKGROUND_MAP2_START: u16 = 0x9C00;
const BACKGROUND_MAP2_END: u16 = BACKGROUND_MAP2_START + BACKGROUND_MAP_LENGTH - 1;*/

const VRAM_START: u16 = 0x8000;
pub const VRAM_LENGTH: u16 = 0x2000;
const VRAM_END: u16 = VRAM_START + VRAM_LENGTH - 1;

// Cartridge RAM, aka "battery" data (saved data)
const EXTERNAL_RAM_START: u16 = 0xA000;
const EXTERNAL_RAM_LENGTH: u16 = 0x2000;
const EXTERNAL_RAM_END: u16 = EXTERNAL_RAM_START + EXTERNAL_RAM_LENGTH - 1;

// Internal or "Work" RAM
const INTERNAL_RAM_START: u16 = 0xC000;
pub const INTERNAL_RAM_LENGTH: u16 = 0x2000;
const INTERNAL_RAM_END: u16 = INTERNAL_RAM_START + INTERNAL_RAM_LENGTH - 1;

// Echo of the internal RAM. Should not be used, but some games migth use it?
const ECHO_INTERNAL_RAM_START: u16 = 0xE000;
const ECHO_INTERNAL_RAM_LENGTH: u16 = 0x1E00;
const ECHO_INTERNAL_RAM_END: u16 = ECHO_INTERNAL_RAM_START + ECHO_INTERNAL_RAM_LENGTH - 1;

// Sprite RAM, aka Object Attribute Memory (OAM)
const SPRITE_RAM_START: u16 = 0xFE00;
pub const SPRITE_RAM_LENGTH: u16 = 0x00A0;
const SPRITE_RAM_END: u16 = SPRITE_RAM_START + SPRITE_RAM_LENGTH - 1;

// There are 96 (0x60) bytes unused here (what a waste!)
const UNUSED_START: u16 = 0xFEA0;
pub const UNUSED_LENGTH: u16 = 0x0060;
const UNUSED_END: u16 = UNUSED_START + UNUSED_LENGTH - 1;

// Hardware I/0 Registers
const HARDWARE_IO_START: u16 = 0xFF00;
const HARDWARE_IO_LENGTH: u16 = 0x0080;
const HARDWARE_IO_END: u16 = HARDWARE_IO_START + HARDWARE_IO_LENGTH - 1;

// High RAM area (used as a quick access RAM)
const HIGH_RAM_START: u16 = 0xFF80;
pub const HIGH_RAM_LENGTH: u16 = 0x007F;
const HIGH_RAM_END: u16 = HIGH_RAM_START + HIGH_RAM_LENGTH - 1;

// Interrupt enable register
const INTERRUPT_ENABLE_REGISTER: u16 = 0xFFFF;

#[derive(Debug)]
pub enum Addr{
	CartridgeRom(u16),
	VRam(u16),
	ExternalRam(u16),
	InternalRam(u16),
	SpriteRam(u8),
	Unused,
	HardwareIO(u8),
	HighRam(u16),
	InterruptEnable,

	Invalid
}

pub fn map_addr(addr: u16) -> Addr {
	match addr {
		CARTRIDGE_ROM_START ... CARTRIDGE_ROM_END
			=> Addr::CartridgeRom(addr - CARTRIDGE_ROM_START),

		VRAM_START ... VRAM_END
			=> Addr::VRam(addr - VRAM_START),

		EXTERNAL_RAM_START ... EXTERNAL_RAM_END
			=> Addr::ExternalRam(addr - EXTERNAL_RAM_START),

		INTERNAL_RAM_START ... INTERNAL_RAM_END
			=> Addr::InternalRam(addr - INTERNAL_RAM_START),

		ECHO_INTERNAL_RAM_START ... ECHO_INTERNAL_RAM_END
			=> Addr::InternalRam(addr - ECHO_INTERNAL_RAM_START),

		SPRITE_RAM_START ... SPRITE_RAM_END
			=> Addr::SpriteRam((addr - SPRITE_RAM_START) as u8),

		UNUSED_START ... UNUSED_END
			=> Addr::Unused,

		HARDWARE_IO_START ... HARDWARE_IO_END
			=> Addr::HardwareIO((addr - HARDWARE_IO_START) as u8),

		HIGH_RAM_START ... HIGH_RAM_END
			=> Addr::HighRam(addr - HIGH_RAM_START),

		INTERRUPT_ENABLE_REGISTER
			=> Addr::InterruptEnable,

		_ => {
			println!("Invalid memory address");
			Addr::Invalid
		}
	}
}
