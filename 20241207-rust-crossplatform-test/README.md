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

### Windows 11 issues

After installing Rust on Windows 11, I got an error while building `libc`. It was complaining that
a DLL wasn't found ("failed to run custom build command", with status `STATUS_DLL_NOT_FOUND`).
I re-ran the command in the latest version of PowerShell, and a dialog told me that a specific DLL
was missing: `vcruntime140.dll`. I searched for this in my Visual Studio installation and found
it in `C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.42.34433\bin\Hostx64\x86`.
I added this directory to my `PATH` environment variable, and `libc` build successfully.

