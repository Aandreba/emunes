//! https://www.nesdev.org/wiki/PPU_rendering#Drawing_overview

use super::{
    oam::Sprite,
    tiles::{u2, Bank},
    Ppu,
};
use crate::{
    ppu::memory::NameTables,
    video::{palette::Palette, Video},
};
use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Rect {
    pub x: Range<usize>,
    pub y: Range<usize>,
}

impl Ppu {
    pub fn render(&mut self, video: &mut Video, colors: &Palette) {
        self.render_background(video, colors);
        self.render_sprites(video, colors);
    }

    pub fn render_background(&mut self, video: &mut Video, colors: &Palette) {
        let scroll_x = (self.scroll.x) as usize;
        let scroll_y = (self.scroll.y) as usize;

        let (main_nametable, second_nametable) = match (
            &self.memory.name_tables,
            self.controller.base_nametable_index(),
        ) {
            (NameTables::Vertical(_), u2::Zero | u2::Two)
            | (NameTables::Horizontal(_), u2::Zero | u2::One) => (Bank::Left, Bank::Right),
            (NameTables::Vertical(_), u2::One | u2::Three)
            | (NameTables::Horizontal(_), u2::Two | u2::Three) => (Bank::Right, Bank::Left),
            _ => todo!(),
        };

        self.render_nametable(
            main_nametable,
            Rect {
                x: scroll_x..256,
                y: scroll_y..240,
            },
            -(scroll_x as isize),
            -(scroll_y as isize),
            video,
            colors,
        );

        if scroll_x > 0 {
            self.render_nametable(
                second_nametable,
                Rect {
                    x: 0..scroll_x,
                    y: 0..240,
                },
                (256 - scroll_x) as isize,
                0,
                video,
                colors,
            )
        } else if scroll_y > 0 {
            self.render_nametable(
                second_nametable,
                Rect {
                    x: 0..256,
                    y: 0..scroll_y,
                },
                0,
                (240 - scroll_y) as isize,
                video,
                colors,
            )
        }
    }

    pub fn render_sprites(&mut self, video: &mut Video, colors: &Palette) {
        let pattern_table = match self.controller.sprite_pattern_table_bank() {
            Bank::Left => &self.memory.left_table,
            Bank::Right => self.memory.right_table(),
        };

        for Sprite {
            top_y,
            index,
            attributes,
            left_x,
        } in self.oam_data.iter().copied()
        {
            let tile = &pattern_table[index as usize];
            let top_y = top_y as usize;
            let left_x = left_x as usize;

            let flip_vert = attributes.flip_vertical();
            let flip_hoz = attributes.flip_horizontal();
            let priority = attributes.priority();
            let palette = self
                .memory
                .palette
                .get_sprite_palette(attributes.palette(), colors);

            for y in 0..8 {
                for x in 0..8 {
                    let color = match tile.get_pixel(x, y).unwrap() {
                        u2::Zero => continue,
                        u2::One => palette[0],
                        u2::Two => palette[1],
                        u2::Three => palette[2],
                    };

                    let x = x as usize;
                    let y = y as usize;

                    let (pixel_x, pixel_y) = match (flip_hoz, flip_vert) {
                        (false, false) => (left_x + x, top_y + y),
                        (true, false) => (left_x + 7 - x, top_y + y),
                        (false, true) => (left_x + x, top_y + 7 - y),
                        (true, true) => (left_x + 7 - x, top_y + 7 - y),
                    };

                    video.set_pixel(pixel_x, pixel_y, color);
                }
            }
        }
    }

    fn render_nametable(
        &mut self,
        bank: Bank,
        viewport: Rect,
        shift_x: isize,
        shift_y: isize,
        video: &mut Video,
        colors: &Palette,
    ) {
        let nametable = self.memory.name_tables.get_bank(bank).unwrap();
        let pattern_table = match self.controller.background_pattern_table_bank() {
            Bank::Left => &self.memory.left_table,
            Bank::Right => self.memory.right_table(),
        };

        for tile_row in 0..30 {
            for tile_col in 0..32 {
                let tile_idx = nametable.tiles[tile_row][tile_col];
                let tile = &pattern_table[tile_idx as usize];

                let palette_idx = nametable.get_palette_idx(tile_row, tile_col);
                let palette = self
                    .memory
                    .palette
                    .get_background_palette(palette_idx, colors);

                for y in 0..8 {
                    for x in 0..8 {
                        let pixel_x = 8 * tile_col + x as usize;
                        let pixel_y = 8 * tile_row + y as usize;

                        if !viewport.x.contains(&pixel_x) || !viewport.y.contains(&pixel_y) {
                            continue;
                        }

                        let color = match tile.get_pixel(x, y).unwrap() {
                            u2::Zero => self.memory.palette.get_ubc(colors),
                            u2::One => palette[0],
                            u2::Two => palette[1],
                            u2::Three => palette[2],
                        };

                        video.set_pixel(
                            pixel_x.wrapping_add_signed(shift_x),
                            pixel_y.wrapping_add_signed(shift_y),
                            color,
                        );
                    }
                }
            }
        }
    }
}
