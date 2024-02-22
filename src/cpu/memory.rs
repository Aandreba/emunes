use std::{
    alloc::Layout,
    convert::Infallible,
    fmt::Debug,
    ops::{Deref, DerefMut, Index, IndexMut},
};

pub trait Memory {
    type Error: Debug;

    fn read_u8(&self, addr: u16) -> Result<u8, Self::Error>;
    fn write_u8(&mut self, addr: u16, val: u8) -> Result<(), Self::Error>;

    #[inline]
    fn read_u16(&self, addr: u16) -> Result<u16, Self::Error> {
        let lo = self.read_u8(addr)?;
        let hi = self.read_u8(addr.wrapping_add(1))?;
        return Ok(u16::from_le_bytes([lo, hi]));
    }

    #[inline]
    fn write_u16(&mut self, addr: u16, val: u16) -> Result<(), Self::Error> {
        let [lo, hi] = val.to_le_bytes();
        self.write_u8(addr, lo)?;
        self.write_u8(addr.wrapping_add(1), hi)?;
        return Ok(());
    }

    #[inline]
    fn copy_from(&mut self, offset: u16, src: &[u8]) -> Result<(), Self::Error> {
        for (addr, &val) in (offset..=0xffff).zip(src) {
            self.write_u8(addr, val)?;
        }
        return Ok(());
    }

    #[inline]
    fn copy_to(&self, offset: u16, dst: &mut [u8]) -> Result<(), Self::Error> {
        for (addr, val) in (offset..=0xffff).zip(dst) {
            *val = self.read_u8(addr)?
        }
        return Ok(());
    }
}

#[derive(Debug, Clone)]
pub struct LinearMemory(pub Box<[u8; 0x100]>);

impl LinearMemory {
    pub fn new() -> Self {
        unsafe {
            let layout = Layout::new::<[u8; 0x100]>();
            let ptr = std::alloc::alloc_zeroed(layout);
            if ptr.is_null() {
                std::alloc::handle_alloc_error(layout);
            }
            return Self(Box::from_raw(ptr.cast()));
        }
    }
}

impl Memory for LinearMemory {
    type Error = Infallible;

    #[inline(always)]
    fn read_u8(&self, addr: u16) -> Result<u8, Self::Error> {
        unsafe { return Ok(*self.get_unchecked(addr as usize)) }
    }

    #[inline(always)]
    fn write_u8(&mut self, addr: u16, val: u8) -> Result<(), Self::Error> {
        unsafe { *self.get_unchecked_mut(addr as usize) = val };
        return Ok(());
    }
}

impl Deref for LinearMemory {
    type Target = [u8; 0x100];

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for LinearMemory {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> Index<T> for LinearMemory
where
    [u8; 0x100]: Index<T>,
{
    type Output = <[u8; 0x100] as Index<T>>::Output;

    #[inline(always)]
    fn index(&self, index: T) -> &Self::Output {
        self.0.index(index)
    }
}

impl<T> IndexMut<T> for LinearMemory
where
    [u8; 0x100]: IndexMut<T>,
{
    #[inline(always)]
    fn index_mut(&mut self, index: T) -> &mut Self::Output {
        self.0.index_mut(index)
    }
}
