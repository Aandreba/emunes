test:
    cargo test --release

run:
    cargo run --release --bin emunes

flamegraph:
    RUSTFLAGS=-g cargo build --release --bin emunes
    flamegraph -- ./target/release/emunes
