//! https://github.com/bugzmanov/nes_ebook/blob/master/code/ch5/src/cartridge.rs

const NES_TAG: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
pub const PRG_ROM_PAGE_SIZE: usize = 16384;
pub const CHR_ROM_PAGE_SIZE: usize = 8192;

pub struct Cartridge {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mapper: u8,
    pub screen_mirroring: Mirroring,
}

impl Cartridge {
    pub fn new(raw: &[u8]) -> Result<Cartridge, String> {
        if &raw[0..4] != NES_TAG {
            return Err("File is not in iNES format".to_string());
        }

        let mapper = (raw[7] & 0b1111_0000) | (raw[6] >> 4);

        let ines_ver = (raw[7] >> 2) & 0b11;
        if ines_ver != 0 {
            return Err("NES2.0 format is not supported".to_string());
        }

        let four_screen = raw[6] & 0b1000 != 0;
        let vertical_mirroring = raw[6] & 0b1 != 0;
        let screen_mirroring = match (four_screen, vertical_mirroring) {
            (true, _) => Mirroring::FourScreen,
            (false, true) => Mirroring::Vertical,
            (false, false) => Mirroring::Horizontal,
        };

        let prg_rom_size = raw[4] as usize * PRG_ROM_PAGE_SIZE;
        let chr_rom_size = raw[5] as usize * CHR_ROM_PAGE_SIZE;

        let skip_trainer = raw[6] & 0b100 != 0;

        let prg_rom_start = 16 + if skip_trainer { 512 } else { 0 };
        let chr_rom_start = prg_rom_start + prg_rom_size;

        Ok(Cartridge {
            prg_rom: raw[prg_rom_start..(prg_rom_start + prg_rom_size)].to_vec(),
            chr_rom: raw[chr_rom_start..(chr_rom_start + chr_rom_size)].to_vec(),
            mapper,
            screen_mirroring,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mirroring {
    Vertical,
    Horizontal,
    FourScreen,
}

impl Mirroring {
    pub const fn mirror_vram_addr(self, addr: u16) -> u16 {
        let mirrored_vram = addr & 0b10111111111111; // mirror down 0x3000-0x3eff to 0x2000 - 0x2eff
        let vram_index = mirrored_vram - 0x2000; // to vram vector
        let name_table = vram_index / 0x400; // to the name table index
        match (&self, name_table) {
            (Mirroring::Vertical, 2) | (Mirroring::Vertical, 3) => vram_index - 0x800,
            (Mirroring::Horizontal, 2) => vram_index - 0x400,
            (Mirroring::Horizontal, 1) => vram_index - 0x400,
            (Mirroring::Horizontal, 3) => vram_index - 0x800,
            _ => vram_index,
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
struct Header {
    tag: [u8; 4],
    prg_rom_lsb: u8,
    chr_rom_lsb: u8,
    flags: HeaderFlags,
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
struct HeaderFlags(u32);

impl HeaderFlags {
    pub fn mapper_id(self) -> u32 {
        self.mapper_id_1() as u32
            | ((self.mapper_id_2() as u32) << 4)
            | ((self.mapper_id_3() as u32) << 8)
    }
}

impl HeaderFlags {
    #[inline(always)]
    pub fn vertical_mirror(self) -> bool {
        return (self.0 & 0b1) != 0;
    }

    #[inline(always)]
    pub fn battery(self) -> bool {
        return (self.0 & 0b10) != 0;
    }

    #[inline(always)]
    pub fn trainer(self) -> bool {
        return (self.0 & 0b100) != 0;
    }

    #[inline(always)]
    pub fn four_screen(self) -> bool {
        return (self.0 & 0b1000) != 0;
    }

    #[inline(always)]
    pub fn mapper_id_1(self) -> u8 {
        return ((self.0 >> 4) & 0xf) as u8;
    }

    #[inline(always)]
    pub fn console_type(self) -> u8 {
        return ((self.0 >> 8) & 0b11) as u8;
    }

    #[inline(always)]
    pub fn format_identifier(self) -> u8 {
        return ((self.0 >> 10) & 0b11) as u8;
    }

    #[inline(always)]
    pub fn mapper_id_2(self) -> u8 {
        return ((self.0 >> 12) & 0xf) as u8;
    }

    #[inline(always)]
    pub fn mapper_id_3(self) -> u8 {
        return ((self.0 >> 16) & 0xf) as u8;
    }

    #[inline(always)]
    pub fn submapper_number(self) -> u8 {
        return ((self.0 >> 20) & 0xf) as u8;
    }

    #[inline(always)]
    pub fn prg_rom_msb(self) -> u8 {
        return ((self.0 >> 24) & 0xf) as u8;
    }

    #[inline(always)]
    pub fn chr_rom_msb(self) -> u8 {
        return ((self.0 >> 28) & 0xf) as u8;
    }
}

pub mod test {

    use super::*;

    struct TestRom {
        header: Vec<u8>,
        trainer: Option<Vec<u8>>,
        pgp_rom: Vec<u8>,
        chr_rom: Vec<u8>,
    }

    fn create_rom(rom: TestRom) -> Vec<u8> {
        let mut result = Vec::with_capacity(
            rom.header.len()
                + rom.trainer.as_ref().map_or(0, |t| t.len())
                + rom.pgp_rom.len()
                + rom.chr_rom.len(),
        );

        result.extend(&rom.header);
        if let Some(t) = rom.trainer {
            result.extend(t);
        }
        result.extend(&rom.pgp_rom);
        result.extend(&rom.chr_rom);

        result
    }

    pub fn test_rom() -> Cartridge {
        let test_rom = create_rom(TestRom {
            header: vec![
                0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 00, 00, 00, 00, 00, 00, 00, 00, 00,
            ],
            trainer: None,
            pgp_rom: vec![1; 2 * PRG_ROM_PAGE_SIZE],
            chr_rom: vec![2; 1 * CHR_ROM_PAGE_SIZE],
        });

        Cartridge::new(&test_rom).unwrap()
    }

    #[test]
    fn test() {
        let test_rom = create_rom(TestRom {
            header: vec![
                0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 00, 00, 00, 00, 00, 00, 00, 00, 00,
            ],
            trainer: None,
            pgp_rom: vec![1; 2 * PRG_ROM_PAGE_SIZE],
            chr_rom: vec![2; 1 * CHR_ROM_PAGE_SIZE],
        });

        let rom: Cartridge = Cartridge::new(&test_rom).unwrap();

        assert_eq!(rom.chr_rom, vec!(2; 1 * CHR_ROM_PAGE_SIZE));
        assert_eq!(rom.prg_rom, vec!(1; 2 * PRG_ROM_PAGE_SIZE));
        assert_eq!(rom.mapper, 3);
        assert_eq!(rom.screen_mirroring, Mirroring::Vertical);
    }

    #[test]
    fn test_with_trainer() {
        let test_rom = create_rom(TestRom {
            header: vec![
                0x4E,
                0x45,
                0x53,
                0x1A,
                0x02,
                0x01,
                0x31 | 0b100,
                00,
                00,
                00,
                00,
                00,
                00,
                00,
                00,
                00,
            ],
            trainer: Some(vec![0; 512]),
            pgp_rom: vec![1; 2 * PRG_ROM_PAGE_SIZE],
            chr_rom: vec![2; 1 * CHR_ROM_PAGE_SIZE],
        });

        let rom: Cartridge = Cartridge::new(&test_rom).unwrap();

        assert_eq!(rom.chr_rom, vec!(2; 1 * CHR_ROM_PAGE_SIZE));
        assert_eq!(rom.prg_rom, vec!(1; 2 * PRG_ROM_PAGE_SIZE));
        assert_eq!(rom.mapper, 3);
        assert_eq!(rom.screen_mirroring, Mirroring::Vertical);
    }

    #[test]
    fn test_nes2_is_not_supported() {
        let test_rom = create_rom(TestRom {
            header: vec![
                0x4E, 0x45, 0x53, 0x1A, 0x01, 0x01, 0x31, 0x8, 00, 00, 00, 00, 00, 00, 00, 00,
            ],
            trainer: None,
            pgp_rom: vec![1; 1 * PRG_ROM_PAGE_SIZE],
            chr_rom: vec![2; 1 * CHR_ROM_PAGE_SIZE],
        });
        let rom = Cartridge::new(&test_rom);
        match rom {
            Result::Ok(_) => assert!(false, "should not load rom"),
            Result::Err(str) => assert_eq!(str, "NES2.0 format is not supported"),
        }
    }
}
