use super::tiles::u2;
use bytemuck::{Pod, Zeroable};
use std::{
    hint::unreachable_unchecked,
    ops::{Deref, DerefMut},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Pod, Zeroable)]
#[repr(transparent)]
pub struct Oam([Sprite; 64]);

impl Oam {
    pub fn as_bytes(&self) -> &[u8; 256] {
        return bytemuck::cast_ref(self);
    }

    pub fn as_mut_bytes(&mut self) -> &mut [u8; 256] {
        return bytemuck::cast_mut(self);
    }
}

impl Default for Oam {
    fn default() -> Self {
        return Self([Sprite::default(); 64]);
    }
}

impl Deref for Oam {
    type Target = [Sprite; 64];

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Oam {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Pod, Zeroable)]
#[repr(C)]
pub struct Sprite {
    pub top_y: u8,
    pub index: u8,
    pub attributes: Attributes,
    pub left_x: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Pod, Zeroable)]
#[repr(transparent)]
pub struct Attributes(u8);

impl Attributes {
    pub const fn new(palette: u2, priority: Priority, flip_hoz: bool, flip_vert: bool) -> Self {
        return Self(
            palette as u8 | (priority as u8) << 5 | (flip_hoz as u8) << 6 | (flip_vert as u8) << 7,
        );
    }

    pub const fn into_inner(self) -> u8 {
        return self.0 & 0xe3;
    }
}

impl Attributes {
    pub const fn palette(self) -> u2 {
        match self.0 & 0b11 {
            0 => u2::Zero,
            1 => u2::One,
            2 => u2::Two,
            3 => u2::Three,
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub const fn priority(self) -> Priority {
        match self.0 & 0b100000 != 0 {
            true => Priority::Back,
            false => Priority::Front,
        }
    }

    pub const fn flip_horizontal(self) -> bool {
        return self.0 & 0b1000000 != 0;
    }

    pub const fn flip_vertical(self) -> bool {
        return self.0 & 0b10000000 != 0;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Priority {
    Front,
    Back,
}
