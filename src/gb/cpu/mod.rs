#![allow(dead_code)]

mod cpu;
pub mod instruction;

pub use self::cpu::Cpu;
pub use self::instruction::Instruction;
pub use self::instruction::Reg8;
pub use self::instruction::Reg16;
