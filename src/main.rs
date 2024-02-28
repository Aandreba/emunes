use emunes::{cartridge::Cartridge, Nes};
use std::{
    future::Future,
    sync::Arc,
    task::{Context, Poll, Wake},
    thread::Thread,
};

pub fn main() {
    block_on(async move {
        let pacman = std::fs::read("Pac-Man (USA) (Namco).nes").unwrap();
        let cartridge = Cartridge::new(&pacman).unwrap();

        let mut nes = Nes::new(cartridge).await.unwrap();
        nes.run().unwrap();
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
