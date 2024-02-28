use crate::{cartridge::Cartridge, cpu::memory::Memory, ppu::Ppu};

pub struct NesMemory {
    pub ram: Box<[u8; 0x800]>,
    pub prg_rom: Box<[u8]>,
    pub ppu: Ppu,
    pub nmi_interrupt: bool,
}

impl NesMemory {
    pub fn new(cartridge: Cartridge) -> Self {
        return Self {
            ram: Box::new([0; 0x800]),
            prg_rom: cartridge.prg_rom.into_boxed_slice(),
            ppu: Ppu::new(cartridge.chr_rom, cartridge.screen_mirroring),
            nmi_interrupt: false,
        };
    }

    pub fn write_oam_dma(&mut self, val: u8) -> Result<(), <Self as Memory>::Error> {
        let start = (val as u16) << 8;
        let end = start | 0xff;

        for addr in start..=end {
            let val = self.read_u8(addr)?;
            self.ppu.write_oam_data(val);
        }

        // TODO apply dma ticks
        return Ok(());
    }
}

impl Memory for NesMemory {
    type Error = MemoryError;

    fn read_u8(&mut self, addr: u16) -> Result<u8, Self::Error> {
        return Ok(match addr {
            0x0000..=0x1fff => self.ram[(addr % 0x0800) as usize],
            0x2002 => self.ppu.read_status(),
            0x2004 => self.ppu.read_oam_data(),
            0x2007 => self.ppu.read_data(),
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 => return Err(MemoryError::WriteOnly(addr)),
            0x2008..=0x3fff => return self.read_u8(0x2000 + ((addr - 0x2008) % 8)),
            // TODO APU registers
            0x4000..=0x4017 => 0,
            0x8000..=0xFFFF => self.prg_rom[(addr % 0x8000) as usize],
            _ => return Err(MemoryError::UnknownAddress(addr)),
        });
    }

    fn write_u8(&mut self, addr: u16, val: u8) -> Result<(), Self::Error> {
        match addr {
            0x0000..=0x1fff => self.ram[(addr % 0x0800) as usize] = val,
            0x2000 => self.nmi_interrupt |= self.ppu.write_controller(val),
            0x2001 => self.ppu.write_mask(val),
            0x2003 => self.ppu.write_oam_address(val),
            0x2004 => self.ppu.write_oam_data(val),
            0x2005 => self.ppu.write_scroll(val),
            0x2006 => self.ppu.write_address(val),
            0x2007 => self.ppu.write_data(val),
            0x4014 => return self.write_oam_dma(val),
            0x2008..=0x3fff => return self.write_u8(0x2000 + ((addr - 0x2008) % 8), val),
            // TODO APU registers
            0x4000..=0x4017 => {}
            0x2002 | 0x8000..=0xFFFF => return Err(MemoryError::ReadOnly(addr)),
            _ => return Err(MemoryError::UnknownAddress(addr)),
        };

        return Ok(());
    }
}

#[derive(Debug, thiserror::Error)]
pub enum MemoryError {
    #[error("tried to write to read-only memory at 0x{0:04X}")]
    ReadOnly(u16),
    #[error("tried to read from write-only memory at 0x{0:04X}")]
    WriteOnly(u16),
    #[error("unknown memory address at 0x{0:04X}")]
    UnknownAddress(u16),
}
