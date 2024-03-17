use crate::{
    cartridge::Cartridge,
    cpu::memory::Memory,
    mapper::{m0::Nrom, Mapper},
    ppu::Ppu,
    video::{Error, Video},
};
use bytemuck::zeroed_slice_box;
use std::sync::{
    atomic::{AtomicU8, Ordering},
    Arc,
};
use winit::event_loop::EventLoop;

pub struct NesMemory {
    pub ram: Box<[u8; 0x800]>,
    pub mapper: Box<dyn Mapper>,
    pub ppu: Ppu,
    pub nmi_interrupt: bool,
    pub video: Video,
    pub joypad1: Joypad,
    pub joypad2: Joypad,
}

impl NesMemory {
    pub async fn new(cartridge: Cartridge) -> Result<(Self, EventLoop<()>), Error> {
        let (video, event_loop) = Video::new().await?;
        return Ok((
            Self {
                ram: Box::new([0; 0x800]),
                mapper: Box::new(match cartridge.mapper {
                    0 => Nrom {
                        prg_ram: zeroed_slice_box(512).try_into().unwrap(),
                        prg_rom: cartridge.prg_rom.into_boxed_slice(),
                    },
                    other => return Err(Error::Mapper(other)),
                }),
                ppu: Ppu::new(cartridge.chr_rom, cartridge.screen_mirroring),
                nmi_interrupt: false,
                joypad1: Joypad::default(),
                joypad2: Joypad::default(),
                video,
            },
            event_loop,
        ));
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
        if let Some(val) = self.mapper.cpu_read(addr) {
            return Ok(val);
        }

        return Ok(match addr {
            0x0000..=0x1fff => self.ram[(addr % 0x0800) as usize],
            0x2002 => self.ppu.read_status(),
            0x2004 => self.ppu.read_oam_data(),
            0x2007 => self.ppu.read_data(),
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 => return Err(MemoryError::WriteOnly(addr)),
            0x2008..=0x3fff => return self.read_u8(0x2000 + ((addr - 0x2008) % 8)),
            0x4016 => self.joypad1.read(),
            0x4017 => self.joypad2.read(),
            // TODO APU registers
            0x4000..=0x4017 => 0,
            _ => return Err(MemoryError::UnknownAddress(addr)),
        });
    }

    fn write_u8(&mut self, addr: u16, val: u8) -> Result<(), Self::Error> {
        if self.mapper.cpu_write(addr, val) {
            return Ok(());
        }

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
            0x4016 => self.joypad1.write(val),
            0x4017 => self.joypad2.write(val),
            0x2008..=0x3fff => return self.write_u8(0x2000 + ((addr - 0x2008) % 8), val),
            // TODO APU registers
            0x4000..=0x4017 => {}
            0x2002 | 0x8000..=0xFFFF => return Err(MemoryError::ReadOnly(addr)),
            _ => return Err(MemoryError::UnknownAddress(addr)),
        };

        return Ok(());
    }
}

#[derive(Debug, Default)]
pub struct Joypad {
    strobe: bool,
    button_index: u8,
    pub status: Arc<AtomicU8>,
}

impl Joypad {
    //...
    pub fn write(&mut self, data: u8) {
        self.strobe = data & 1 == 1;
        if self.strobe {
            self.button_index = 0
        }
    }

    pub fn read(&mut self) -> u8 {
        if self.button_index > 7 {
            return 1;
        }
        let response =
            (self.status.load(Ordering::Acquire) & (1 << self.button_index)) >> self.button_index;
        if !self.strobe && self.button_index <= 7 {
            self.button_index += 1;
        }
        response
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
