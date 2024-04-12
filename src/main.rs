use color_eyre::eyre::Report;
use emunes::{cartridge::Cartridge, joystick::Joystick, Nes};
use std::{
    future::Future,
    sync::Arc,
    task::{Context, Poll, Wake},
    thread::Thread,
};

pub fn main() -> color_eyre::Result<()> {
    color_eyre::install().unwrap();
    simple_logger::SimpleLogger::new()
        .with_level(log::LevelFilter::Debug)
        .init()
        .unwrap();

    return block_on(async move {
        let pacman = std::fs::read("Bomberman (USA).nes")?;
        let cartridge = Cartridge::new(&pacman).map_err(color_eyre::Report::msg)?;

        log::debug!("PRG ROM data: {} byte(s)", cartridge.prg_rom.len());
        log::debug!("CHR ROM data: {} byte(s)", cartridge.chr_rom.len());
        log::debug!("Mapper: {}", cartridge.mapper);
        log::debug!("Mirroring: {:?}", cartridge.screen_mirroring);

        let nes = Nes::new(cartridge).await?;
        nes.run(Joystick::ARROW, None)
            .map_err(|e| Report::msg(format!("{e:?}")))?;
        return Ok(());
    });
}

// #[cfg(not(target_family = "wasm"))]
pub fn block_on<Fut: Future>(fut: Fut) -> Fut::Output {
    /// A waker that wakes up the current thread when called.
    struct ThreadWaker(Thread);

    impl Wake for ThreadWaker {
        #[inline(always)]
        fn wake(self: Arc<Self>) {
            self.0.unpark();
        }

        #[inline(always)]
        fn wake_by_ref(self: &Arc<Self>) {
            self.0.unpark()
        }
    }

    // Pin the future so it can be polled.
    let mut fut = std::pin::pin!(fut);

    // Create a new context to be passed to the future.
    let t = std::thread::current();
    let waker = Arc::new(ThreadWaker(t)).into();
    let mut cx = Context::from_waker(&waker);

    // Run the future to completion.
    loop {
        match fut.as_mut().poll(&mut cx) {
            Poll::Ready(res) => return res,
            Poll::Pending => std::thread::park(),
        }
    }
}
