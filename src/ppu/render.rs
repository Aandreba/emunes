//! https://www.nesdev.org/wiki/PPU_rendering#Drawing_overview

use super::{
    tiles::{u2, Bank},
    Ppu,
};
use crate::video::{palette::Palette, Video};

impl Ppu {
    pub fn render(&mut self, video: &mut Video, colors: &Palette) {
        self.render_background(video, colors)
    }

    pub fn render_background(&mut self, video: &mut Video, colors: &Palette) {
        let pattern_table = match self.controller.background_pattern_table_bank() {
            Bank::Left => &self.memory.left_table,
            Bank::Right => self.memory.right_table(),
        };

        let nametable = self
            .memory
            .name_tables
            .get_table(self.controller.base_nametable_index());

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
                        let color = match tile.get_pixel(x, y).unwrap() {
                            u2::Zero => self.memory.palette.get_ubc(colors),
                            u2::One => palette[0],
                            u2::Two => palette[1],
                            u2::Three => palette[2],
                        };

                        video.set_pixel(
                            8 * tile_col + x as usize,
                            8 * tile_row + y as usize,
                            color,
                        );
                    }
                }
            }
        }
    }
}
