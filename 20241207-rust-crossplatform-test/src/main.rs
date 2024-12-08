#![no_std]
#![no_main]
#![windows_subsystem = "windows"]

extern crate alloc;

use core::{ffi::c_void, panic::PanicInfo};

use alloc::string::String;
use libc::{abort, exit, fwrite};

use crossplatform_test::{
    alloc::LibcAllocator,
    io::{eprint, print},
    stdio::stderr,
    thread::sleep,
    window::Display,
};

#[global_allocator]
static LIBC_ALLOCATOR: LibcAllocator = LibcAllocator;

#[panic_handler]
fn panic_handler(_panic_info: &PanicInfo) -> ! {
    unsafe {
        let s = "panic";
        let ptr = s.as_ptr().cast::<c_void>();
        let len = s.len();
        let _bytes_written = fwrite(ptr, 1, len, stderr());
        abort()
    }
}

// Test the global allocator
#[inline(never)]
fn message() -> String {
    String::from("Hello, world!\n")
}

#[no_mangle]
fn main() -> usize {
    print(&message());

    let mut display = Display::open().unwrap_or_else(|| {
        eprint("Failed to open display");
        unsafe { exit(1) }
    });

    let window = display.create_window(0, 0, 640, 480);

    sleep(2);

    window.destroy();

    0
}
