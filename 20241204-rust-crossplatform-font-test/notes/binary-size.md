# Binary size

The release binary with `lto` enabled is 8.8MB. That's massive. For comparison, the entirety
of Sublime Text is 9.8MB.

Rust's "Hello, world!" binary size is 408KB by default. And with `no_std`, using `libc`
instead, it's 16KB.

Following <https://github.com/johnthagen/min-sized-rust>:

* Adding `opt-level = "z"` takes it to 7.3MB
* Adding `codegen-units = 1` takes it to 7MB
* Adding `panic = "abort"` takes it to 6.4MB
