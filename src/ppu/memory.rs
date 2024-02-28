use super::{
    nametable::NameTable,
    palette::PaletteMemory,
    tiles::{u2, PatternTable},
};

#[derive(Debug, Clone)]
pub struct Memory {
    pub left_table: PatternTable,
    pub right_table: Option<PatternTable>,
    pub name_tables: NameTables,
    pub palette: PaletteMemory,
}

// https://www.nesdev.org/wiki/PPU_memory_map
impl Memory {
    pub fn new(chr_rom: Vec<u8>, mirror: crate::cartridge::Mirroring) -> Self {
        return Self {
            left_table: PatternTable::new(chr_rom).unwrap(),
            right_table: None,
            name_tables: match mirror {
                crate::cartridge::Mirroring::Vertical => NameTables::Vertical(Box::default()),
                crate::cartridge::Mirroring::Horizontal => NameTables::Horizontal(Box::default()),
                crate::cartridge::Mirroring::FourScreen => NameTables::FourScreen(Box::default()),
            },
            palette: PaletteMemory::default(),
        };
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x0fff => self.left_table.as_bytes()[addr as usize],
            0x1000..=0x1fff => {
                let addr = addr - 0x1000;
                (match self.right_table {
                    Some(ref table) => table.as_bytes(),
                    None => self.left_table.as_bytes(),
                })[addr as usize]
            }
            0x2000..=0x2fff => self.name_tables.read(addr - 0x2000),
            0x3000..=0x3eff => self.name_tables.read(addr - 0x3000),
            0x3f00..=0x3f1f => self.palette.read(addr),
            0x3f20..=0x3fff => self.palette.read(0x3f00 + addr % 0x20),
            _ => self.read(addr % 0x4000),
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000..=0x0fff => self.left_table.as_mut_bytes()[addr as usize] = val,
            0x1000..=0x1fff => {
                let addr = addr - 0x1000;
                (match self.right_table {
                    Some(ref mut table) => table.as_mut_bytes(),
                    None => self.left_table.as_mut_bytes(),
                })[addr as usize] = val;
            }
            0x2000..=0x2fff => self.name_tables.write(addr - 0x2000, val),
            0x3000..=0x3eff => self.name_tables.write(addr - 0x3000, val),
            0x3f00..=0x3f1f => self.palette.write(addr, val),
            0x3f20..=0x3fff => self.palette.write(0x3f00 + addr % 0x20, val),
            _ => self.write(addr % 0x4000, val),
        }
    }
}

// https://www.nesdev.org/wiki/Mirroring
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NameTables {
    Vertical(Box<[NameTable; 2]>),
    Horizontal(Box<[NameTable; 2]>),
    Single(Box<NameTable>),
    FourScreen(Box<[NameTable; 4]>),
}

impl NameTables {
    pub fn read(&self, offset: u16) -> u8 {
        let addr = offset / 0x400;
        let idx = match offset % 0x400 {
            0 => u2::Zero,
            1 => u2::One,
            2 => u2::Two,
            3 => u2::Three,
            _ => unreachable!(),
        };
        return self.get_table(idx).as_bytes()[addr as usize];
    }

    pub fn write(&mut self, offset: u16, val: u8) {
        let addr = offset / 0x400;
        let idx = match offset % 0x400 {
            0 => u2::Zero,
            1 => u2::One,
            2 => u2::Two,
            3 => u2::Three,
            _ => unreachable!(),
        };

        self.get_mut_table(idx).as_mut_bytes()[addr as usize] = val;
    }

    pub fn get_table(&self, idx: u2) -> &NameTable {
        return match (self, idx) {
            (NameTables::Vertical(table), u2::Zero | u2::One)
            | (NameTables::Horizontal(table), u2::Zero | u2::Two) => &table[0],

            (NameTables::Vertical(table), u2::Two | u2::Three)
            | (NameTables::Horizontal(table), u2::One | u2::Three) => &table[1],

            (NameTables::Single(table), _) => table,
            (NameTables::FourScreen(tables), _) => &tables[idx as usize],
        };
    }

    pub fn get_mut_table(&mut self, idx: u2) -> &mut NameTable {
        return match (self, idx) {
            (NameTables::Vertical(table), u2::Zero | u2::One)
            | (NameTables::Horizontal(table), u2::Zero | u2::Two) => &mut table[0],

            (NameTables::Vertical(table), u2::Two | u2::Three)
            | (NameTables::Horizontal(table), u2::One | u2::Three) => &mut table[1],

            (NameTables::Single(table), _) => table,
            (NameTables::FourScreen(tables), _) => &mut tables[idx as usize],
        };
    }
}
