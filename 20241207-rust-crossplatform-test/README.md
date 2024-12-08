# Rust cross-platform test

*2024-12-07*

I want to write a "Hello, world!" program that works on Linux, MacOS, and Windows.

## Notes

### Cross platform development

It seems like cross-compilation from Linux to MacOS/Windows
[doesn't really work](https://github.com/japaric/rust-cross?tab=readme-ov-file#faq),
and you're better off building natively on each platform. I had to switch between platforms
to iterate on the functions that required different behaviour. At some point that will
stabilize and the target-independent logic can be developed on any platform.

### `build-std`

I'm using `panic = "abort"` and when I tried to use the `alloc` create I got linker
errors (`undefined reference to rust_eh_personality`).
[This thread](https://users.rust-lang.org/t/unexpected-undefined-reference-to-rust-eh-personality-when-compiling-with-c-panic-abort-for-no-std-library/120311) explained the issue. In my case, the problem is that
the `alloc` crate is pre-built with `panic = "unwind"`. Using a nightly Rust toolchain
and `build-std` fixes this.

I've set the `build-std` packages in `.cargo/config.toml`.
