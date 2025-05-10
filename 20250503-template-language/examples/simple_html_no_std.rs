#![no_std]
#![no_main]
extern crate alloc;
extern crate core;

use alloc::format;
use core::ffi::c_void;
use core::panic::PanicInfo;

use libc::size_t;
use x20250503_template_language::compile::{Value, read_compile, run};

use libc_alloc::LibcAlloc;

#[global_allocator]
static ALLOCATOR: LibcAlloc = LibcAlloc;

#[panic_handler]
fn panic_handler(panic_info: &PanicInfo) -> ! {
    let string = match panic_info.location() {
        None => format!("panic@(location unknown): {}\n", panic_info.message()),
        Some(location) => format!(
            "panic@{}:{}:{}: {}\n",
            location.file(),
            location.line(),
            location.column(),
            panic_info.message()
        ),
    };

    unsafe {
        let buffer: *const libc::c_void = string.as_ptr().cast();
        let size: libc::size_t = string.len();
        libc::write(libc::STDERR_FILENO, buffer, size);
        libc::abort()
    }
}

#[unsafe(no_mangle)]
fn main() {
    let template = read_compile("examples/simple_html.html.tpl");

    let write: &mut dyn FnMut(&[u8]) = &mut |bytes| unsafe {
        let buffer: *const c_void = bytes.as_ptr().cast();
        let size: size_t = bytes.len();
        libc::write(libc::STDOUT_FILENO, buffer, size);
    };

    run(
        write,
        &template,
        &[Value::from("Simple HTML"), Value::from("Hello, world!")],
    );
}
