use super::Mapper;

pub struct Nrom {
    pub prg_ram: Box<[u8; 0x200]>,
    pub prg_rom: Box<[u8]>,
}

impl Mapper for Nrom {
    #[inline]
    fn cpu_read(&mut self, addr: u16) -> Option<u8> {
        return Some(match addr {
            0x6000..=0x7fff => return self.prg_ram.get(addr as usize & 0x1fff).copied(),
            0x8000..=0xFFFF => {
                let addr = (addr % 0x8000) as usize;
                self.prg_rom[addr % self.prg_rom.len()]
            }
            _ => return None,
        });
    }

    #[inline]
    fn cpu_write(&mut self, addr: u16, val: u8) -> bool {
        match addr {
            0x6000..=0x7fff => {
                self.prg_ram[addr as usize & 0x1fff] = val;
            }
            _ => return false,
        };
        return true;
    }
}
