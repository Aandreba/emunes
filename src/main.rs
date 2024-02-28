use emunes::{cartridge::Cartridge, Nes};

pub fn main() {
    let pacman = std::fs::read("Pac-Man (USA) (Namco).nes").unwrap();
    let cartridge = Cartridge::new(&pacman).unwrap();
    let nes = Nes::new(cartridge);
}
