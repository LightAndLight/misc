# Rust cross-platform test

*2024-12-07*

I want to write a "Hello, world!" program that works on Linux, MacOS, and Windows.

## Notes

It seems like cross-compilation from Linux to MacOS/Windows
[doesn't really work](https://github.com/japaric/rust-cross?tab=readme-ov-file#faq),
and you're better off building natively on each platform. So it seems like I'll have
to switch between platforms to iterate on the functions that require different behaviour.
At some point that will stabilize and the target-independent logic can be developed on any
platform.
