use super::Mapper;

pub struct Mmc1 {}

impl Mapper for Mmc1 {
    fn cpu_read(&mut self, addr: u16) -> Option<u8> {
        todo!()
    }

    fn cpu_write(&mut self, addr: u16, val: u8) -> bool {
        todo!()
    }
}
