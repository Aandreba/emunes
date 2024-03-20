pub mod m0;
pub mod m1;
pub mod m2;

pub trait Mapper: Send {
    fn cpu_read(&mut self, addr: u16) -> Option<u8>;
    #[must_use]
    fn cpu_write(&mut self, addr: u16, val: u8) -> bool;
}
