use crate::{cartridge::Cartridge, cpu::memory::Memory, ppu::Ppu};

pub struct NesMemory {
    pub ram: Box<[u8; 0x800]>,
    pub ppu: Ppu,
}

impl NesMemory {
    pub fn new(cartridge: Cartridge) -> Self {
        return Self {
            ram: Box::new([0; 0x800]),
            ppu: Ppu::new(cartridge),
        };
    }
}

impl Memory for NesMemory {
    type Error = MemoryError;

    fn read_u8(&mut self, addr: u16) -> Result<u8, Self::Error> {
        return Ok(match addr {
            0x0000..=0x1fff => self.ram[(addr % 0x0800) as usize],
            0x2002 => self.ppu.read_status(),
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 => return Err(MemoryError::WriteOnly(addr)),
            _ => todo!(),
        });
    }

    fn write_u8(&mut self, addr: u16, val: u8) -> Result<(), Self::Error> {
        todo!()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum MemoryError {
    #[error("tried to write to read-only memory at 0x{0:04X}")]
    ReadOnly(u16),
    #[error("tried to read from write-only memory at 0x{0:04X}")]
    WriteOnly(u16),
}
