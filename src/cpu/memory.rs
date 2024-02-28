use std::{
    alloc::Layout,
    convert::Infallible,
    fmt::Debug,
    sync::{
        atomic::{AtomicU8, Ordering},
        LockResult,
    },
};

pub mod debug;

pub trait Memory {
    type Error: Debug;

    fn read_u8(&mut self, addr: u16) -> Result<u8, Self::Error>;
    fn write_u8(&mut self, addr: u16, val: u8) -> Result<(), Self::Error>;

    #[inline]
    fn read_u16(&mut self, addr: u16) -> Result<u16, Self::Error> {
        let lo = self.read_u8(addr)?;
        let hi = self.read_u8(addr.wrapping_add(1))?;
        return Ok(u16::from_le_bytes([lo, hi]));
    }

    #[inline]
    #[track_caller]
    fn write_u16(&mut self, addr: u16, val: u16) -> Result<(), Self::Error> {
        let [lo, hi] = val.to_le_bytes();
        self.write_u8(addr, lo)?;
        self.write_u8(addr.wrapping_add(1), hi)?;
        return Ok(());
    }

    #[inline]
    #[track_caller]
    fn copy_from(&mut self, offset: u16, src: &[u8]) -> Result<(), Self::Error> {
        for (addr, &val) in (offset..=0xffff).zip(src) {
            self.write_u8(addr, val)?;
        }
        return Ok(());
    }

    #[inline]
    #[track_caller]
    fn copy_to(&mut self, offset: u16, dst: &mut [u8]) -> Result<(), Self::Error> {
        for (addr, val) in (offset..=0xffff).zip(dst) {
            *val = self.read_u8(addr)?
        }
        return Ok(());
    }
}

impl Memory for [u8; 0x10000] {
    type Error = Infallible;

    #[inline(always)]
    fn read_u8(&mut self, addr: u16) -> Result<u8, Self::Error> {
        return Ok(unsafe { *self.get_unchecked(addr as usize) });
    }

    #[inline(always)]
    fn write_u8(&mut self, addr: u16, val: u8) -> Result<(), Self::Error> {
        unsafe { *self.get_unchecked_mut(addr as usize) = val }
        return Ok(());
    }

    fn copy_from(&mut self, offset: u16, src: &[u8]) -> Result<(), Self::Error> {
        let start = offset as usize;
        let end = start + src.len();

        self[start..end].copy_from_slice(src);
        return Ok(());
    }

    fn copy_to(&mut self, offset: u16, dst: &mut [u8]) -> Result<(), Self::Error> {
        let start = offset as usize;
        let end = start + dst.len();

        dst.copy_from_slice(&mut self[start..end]);
        return Ok(());
    }
}

impl Memory for [AtomicU8; 0x10000] {
    type Error = Infallible;

    #[inline(always)]
    fn read_u8(&mut self, addr: u16) -> Result<u8, Self::Error> {
        return Ok(unsafe { self.get_unchecked(addr as usize).load(Ordering::Acquire) });
    }

    #[inline(always)]
    fn write_u8(&mut self, addr: u16, val: u8) -> Result<(), Self::Error> {
        unsafe { *self.get_unchecked_mut(addr as usize).get_mut() = val }
        return Ok(());
    }

    fn copy_from(&mut self, offset: u16, src: &[u8]) -> Result<(), Self::Error> {
        let start = offset as usize;
        let end = start + src.len();

        let dst = unsafe { &mut *(&mut self[start..end] as *mut [AtomicU8] as *mut [u8]) };
        dst.copy_from_slice(src);
        return Ok(());
    }
}

// BLANKET IMPLS
impl<M: Memory> Memory for &mut M {
    type Error = M::Error;

    #[inline(always)]
    fn read_u8(&mut self, addr: u16) -> Result<u8, Self::Error> {
        M::read_u8(self, addr)
    }

    #[inline(always)]
    fn write_u8(&mut self, addr: u16, val: u8) -> Result<(), Self::Error> {
        M::write_u8(self, addr, val)
    }

    #[inline(always)]
    fn read_u16(&mut self, addr: u16) -> Result<u16, Self::Error> {
        M::read_u16(self, addr)
    }

    #[inline(always)]
    fn write_u16(&mut self, addr: u16, val: u16) -> Result<(), Self::Error> {
        M::write_u16(self, addr, val)
    }

    #[inline(always)]
    fn copy_from(&mut self, offset: u16, src: &[u8]) -> Result<(), Self::Error> {
        M::copy_from(self, offset, src)
    }

    #[inline(always)]
    fn copy_to(&mut self, offset: u16, dst: &mut [u8]) -> Result<(), Self::Error> {
        M::copy_to(self, offset, dst)
    }
}

impl<M: Memory> Memory for Box<M> {
    type Error = M::Error;

    #[inline(always)]
    fn read_u8(&mut self, addr: u16) -> Result<u8, Self::Error> {
        M::read_u8(self, addr)
    }

    #[inline(always)]
    fn write_u8(&mut self, addr: u16, val: u8) -> Result<(), Self::Error> {
        M::write_u8(self, addr, val)
    }

    #[inline(always)]
    fn read_u16(&mut self, addr: u16) -> Result<u16, Self::Error> {
        M::read_u16(self, addr)
    }

    #[inline(always)]
    fn write_u16(&mut self, addr: u16, val: u16) -> Result<(), Self::Error> {
        M::write_u16(self, addr, val)
    }

    #[inline(always)]
    fn copy_from(&mut self, offset: u16, src: &[u8]) -> Result<(), Self::Error> {
        M::copy_from(self, offset, src)
    }

    #[inline(always)]
    fn copy_to(&mut self, offset: u16, dst: &mut [u8]) -> Result<(), Self::Error> {
        M::copy_to(self, offset, dst)
    }
}

pub fn create_linear_memory() -> Box<[u8; 0x10000]> {
    unsafe {
        let layout = Layout::new::<[u8; 0x10000]>();
        let ptr = std::alloc::alloc_zeroed(layout);
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        return Box::from_raw(ptr.cast());
    }
}

pub fn create_atomic_linear_memory() -> Box<[AtomicU8; 0x10000]> {
    unsafe {
        let layout = Layout::new::<[AtomicU8; 0x10000]>();
        let ptr = std::alloc::alloc_zeroed(layout);
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        return Box::from_raw(ptr.cast());
    }
}

#[inline(always)]
pub(super) fn unwrap_lock<Guard>(res: LockResult<Guard>) -> Guard {
    match res {
        Ok(guard) => guard,
        Err(e) => e.into_inner(),
    }
}
