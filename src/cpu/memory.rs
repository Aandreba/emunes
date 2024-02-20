use std::{
    alloc::Layout,
    num::{NonZeroU16, NonZeroU32},
    ops::{Bound, RangeBounds, RangeInclusive},
};
use vector_mapp::binary::BinaryMap;

pub struct Memory<'a> {
    regions: Vec<(u16, Region<'a>)>,
}

impl<'a> Memory<'a> {
    pub fn read_u8(&self, addr: u16) -> u8 {
        let (region, offset) = match self.regions.binary_search_by_key(&addr, |&(k, _)| k) {
            Ok(idx) => (&self.regions[idx].1, 0),
            Err(idx) => {
                let (offset, region) = &self.regions[idx.saturating_sub(1)];
                (region, addr - *offset)
            }
        };

        match region {
            Region::Owned(mem) => mem[offset as usize],
            Region::Mirror { offset: delta, len } => self.read_u8(*delta + (offset % *len)),
            Region::Closure { read, .. } => read(offset),
        }
    }

    pub fn write_u8(&mut self, addr: u16, value: u8) {
        let (region, offset) = match self.regions.binary_search_by_key(&addr, |&(k, _)| k) {
            Ok(idx) => (&mut self.regions[idx].1, 0),
            Err(idx) => {
                let (offset, region) = &mut self.regions[idx.saturating_sub(1)];
                (region, addr - *offset)
            }
        };

        match region {
            Region::Owned(mem) => mem[offset as usize] = value,
            Region::Mirror { offset: delta, len } => {
                let addr = *delta + (offset % *len);
                self.write_u8(addr, value)
            }
            Region::Closure { write, .. } => write(offset, value),
        };
    }

    pub fn read_u16(&self, addr: u16) -> u16 {
        let lo = self.read_u8(addr);
        let hi = self.read_u8(addr + 1);
        return u16::from_le_bytes([lo, hi]);
    }

    pub fn write_u16(&mut self, addr: u16, value: u16) {
        let [lo, hi] = value.to_le_bytes();
        self.write_u8(addr, lo);
        self.write_u8(addr + 1, hi);
    }
}

pub struct Builder<'a> {
    regions: BinaryMap<u16, (Region<'a>, u16)>,
}

impl<'a> Builder<'a> {
    pub fn mirror(
        mut self,
        dst: impl RangeBounds<u16>,
        src: impl RangeBounds<u16>,
    ) -> Result<Self, BuilderError> {
        let start = match dst.start_bound().cloned() {
            Bound::Unbounded => 0,
            Bound::Included(x) => x,
            Bound::Excluded(x) => x + 1,
        };

        let end = match dst.end_bound().cloned() {
            Bound::Unbounded => 0xffff,
            Bound::Included(x) => x,
            Bound::Excluded(x) => x + 1,
        };

        let offset = match src.start_bound().cloned() {
            Bound::Unbounded => 0,
            Bound::Included(x) => x,
            Bound::Excluded(x) => x + 1,
        };

        let len = match src.end_bound().cloned() {
            Bound::Unbounded => 0xffff,
            Bound::Included(x) => x - 1,
            Bound::Excluded(x) => x,
        } - start;

        self.valid_range(&(start..=end))?;
        self.regions
            .insert(start, (Region::Mirror { offset, len }, end - start + 1));
        return Ok(self);
    }

    pub fn closure(
        mut self,
        range: impl RangeBounds<u16>,
        read: impl 'a + Fn(u16) -> u8,
        write: impl 'a + FnMut(u16, u8),
    ) -> Result<Self, BuilderError> {
        let range = range_incl(range);
        self.valid_range(&range)?;

        self.regions.insert(
            *range.start(),
            (
                Region::Closure {
                    read: Box::new(read),
                    write: Box::new(write),
                },
                range.end() - range.start() + 1,
            ),
        );

        return Ok(self);
    }

    pub fn build(self) -> Memory<'a> {
        // Fill the gaps
        let mut gaps = Vec::with_capacity(self.regions.len() + 1);
        let mut offset = 0;

        for (&region_offset, (_, len)) in self.regions.iter() {
            if let Some(delta) = region_offset.checked_sub(offset).and_then(NonZeroU16::new) {
                gaps.push((offset, Region::new(delta.into())))
            }
            offset = region_offset.saturating_add(*len)
        }
        if let Some(delta) = 0x10000u32
            .checked_sub(offset as u32)
            .and_then(NonZeroU32::new)
        {
            gaps.push((offset, (Region::new(delta))))
        }

        // Merge
        let mut regions = self
            .regions
            .into_iter()
            .map(|(x, (y, _))| (x, y))
            .collect::<BinaryMap<_, _>>();
        regions.extend(gaps);

        return Memory {
            regions: regions.into_vec(),
        };
    }

    fn valid_range(&self, needle: &RangeInclusive<u16>) -> Result<(), BuilderError> {
        for (offset, (_, length)) in self.regions.iter() {
            let haystack = *offset..=*offset + *length - 1;
            if regions_intersect(&needle, &haystack) {
                return Err(BuilderError::Overlap(needle.clone(), haystack));
            }
        }
        return Ok(());
    }
}

pub enum Region<'a> {
    Owned(Box<[u8]>),
    Mirror {
        offset: u16,
        len: u16,
    },
    Closure {
        read: Box<dyn 'a + Fn(u16) -> u8>,
        write: Box<dyn 'a + FnMut(u16, u8)>,
    },
}

impl<'a> Region<'a> {
    pub fn new(len: NonZeroU32) -> Self {
        let len = len.get() as usize;
        let layout = Layout::array::<u8>(len).unwrap();

        unsafe {
            let ptr = std::alloc::alloc_zeroed(layout);
            if ptr.is_null() {
                std::alloc::handle_alloc_error(layout);
            }

            let slice = std::slice::from_raw_parts_mut(ptr, len);
            return Self::Owned(Box::from_raw(slice));
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum BuilderError {
    #[error("Range provided is invalid")]
    InvalidRange,
    #[error("This memory region is already mapped. {0:?} overlaps with existing {1:?}")]
    Overlap(RangeInclusive<u16>, RangeInclusive<u16>),
    #[error("Provided memory region is too big to fit inside the 6502's memory")]
    Overflow,
    #[error("{0}")]
    Str(&'static str),
}

#[inline]
fn regions_intersect(x: &RangeInclusive<u16>, y: &RangeInclusive<u16>) -> bool {
    return x.end() >= y.start() && x.start() <= y.end();
}

fn range_incl(range: impl RangeBounds<u16>) -> RangeInclusive<u16> {
    let start = match range.start_bound().cloned() {
        Bound::Unbounded => 0,
        Bound::Included(x) => x,
        Bound::Excluded(x) => x + 1,
    };

    let end = match range.end_bound().cloned() {
        Bound::Unbounded => 0xffff,
        Bound::Included(x) => x,
        Bound::Excluded(x) => x + 1,
    };

    return start..=end;
}
