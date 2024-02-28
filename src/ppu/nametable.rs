use super::tiles::u2;
use bytemuck::{Pod, Zeroable};
use std::hint::unreachable_unchecked;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Pod, Zeroable)]
#[repr(C)]
pub struct NameTable {
    pub tiles: [[u8; 32]; 30],
    pub attributes: [[Attribute; 8]; 8],
}

impl NameTable {
    #[inline(always)]
    pub fn as_bytes(&self) -> &[u8; 1024] {
        return bytemuck::cast_ref(self);
    }

    #[inline(always)]
    pub fn as_mut_bytes(&mut self) -> &mut [u8; 1024] {
        return bytemuck::cast_mut(self);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Pod, Zeroable)]
#[repr(transparent)]
pub struct Attribute(u8);

impl Attribute {
    pub fn get(self, x: X, y: Y) -> u2 {
        let offset = offset(x, y);
        return match (self.0 >> offset) & 0b11 {
            0 => u2::Zero,
            1 => u2::One,
            2 => u2::Two,
            3 => u2::Three,
            _ => unsafe { unreachable_unchecked() },
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum X {
    Top,
    Bottom,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Y {
    Left,
    Right,
}

#[inline(always)]
pub fn offset(x: X, y: Y) -> u8 {
    return match (x, y) {
        (X::Top, Y::Left) => 0,
        (X::Top, Y::Right) => 2,
        (X::Bottom, Y::Left) => 4,
        (X::Bottom, Y::Right) => 8,
    };
}
