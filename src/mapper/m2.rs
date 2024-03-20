use super::Mapper;
use crate::cartridge::PRG_ROM_PAGE_SIZE;

pub struct UxRom {
    pub prg_rom: Box<[u8]>,
    pub bank_select: u8,
}

impl UxRom {
    #[inline]
    pub fn banks(&self) -> usize {
        self.prg_rom.len() / PRG_ROM_PAGE_SIZE
    }
}

impl Mapper for UxRom {
    // https://github.com/junnys6018/NES-Emulator/blob/master/core/src/Mappers/Mapper002.c#L3
    // TODO fix
    fn cpu_read(&mut self, addr: u16) -> Option<u8> {
        let idx = match addr {
            0x8000..=0xbfff => ((self.bank_select as usize) << 14) | ((addr as usize) & 0x3fff),
            0xc000..=0xffff => {
                let last_bank = self.banks() - 1;
                (last_bank << 14) | ((addr as usize) & 0x3fff)
            }
            _ => return None,
        };

        return self.prg_rom.get(idx).copied();
    }

    fn cpu_write(&mut self, addr: u16, val: u8) -> bool {
        match addr {
            0x8000..=0xffff => self.bank_select = val % (self.banks() as u8),
            _ => return false,
        }
        return true;
    }
}
