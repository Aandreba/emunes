use bytemuck::{Pod, Zeroable};
use pixels::{Pixels, SurfaceTexture};
use winit::{
    dpi::LogicalSize,
    error::{EventLoopError, OsError},
    event_loop::EventLoop,
    window::{Window, WindowBuilder},
};

const WIDTH: usize = 256;
const HEIGHT: usize = 240;

pub mod palette;

pub struct Video {
    pub window: Window,
    pub pixels: Pixels,
}

impl Video {
    pub async fn new() -> Result<(Self, EventLoop<()>), Error> {
        let event_loop = EventLoop::new()?;
        let window = WindowBuilder::new()
            .with_title("emunes")
            .with_min_inner_size(LogicalSize::new(WIDTH as u32, HEIGHT as u32))
            .build(&event_loop)?;

        let size = window.inner_size();
        let surface_texture = SurfaceTexture::new(size.width, size.height, &window);
        let pixels = Pixels::new_async(WIDTH as u32, HEIGHT as u32, surface_texture).await?;

        return Ok((Self { window, pixels }, event_loop));
    }

    #[inline]
    pub fn set_title(&mut self, title: &str) {
        self.window.set_title(title)
    }

    pub fn pixels(&self) -> &[Color] {
        bytemuck::cast_slice(self.pixels.frame())
    }

    pub fn pixels_mut(&mut self) -> &mut [Color] {
        bytemuck::cast_slice_mut(self.pixels.frame_mut())
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, color: Color) -> bool {
        if y >= HEIGHT || x >= WIDTH {
            return false;
        }

        self.pixels_mut()[y * WIDTH + x] = color;
        return true;
    }

    pub fn get_pixel(&self, x: usize, y: usize) -> Option<Color> {
        if y >= HEIGHT || x >= WIDTH {
            return None;
        }
        return Some(self.pixels()[y * WIDTH + x]);
    }

    pub fn show_chr_tiles(&mut self) {}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Pod, Zeroable)]
#[repr(C)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Color {
    #[inline]
    pub const fn new(r: u8, g: u8, b: u8) -> Self {
        return Self::with_alpha(r, g, b, 0xff);
    }

    #[inline]
    pub const fn with_alpha(r: u8, g: u8, b: u8, a: u8) -> Self {
        return Self { r, g, b, a };
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    EventLoop(#[from] EventLoopError),
    #[error("{0}")]
    Os(#[from] OsError),
    #[error("{0}")]
    Pixels(#[from] pixels::Error),
    #[error("Unknown mapper: {0}")]
    Mapper(u8),
}
