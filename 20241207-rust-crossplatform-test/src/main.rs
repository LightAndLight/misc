#![no_std]
#![no_main]

extern crate alloc;

use core::{ffi::c_void, panic::PanicInfo};

use alloc::string::String;
use libc::{abort, fwrite};

use crossplatform_test::{alloc::LibcAllocator, io::print, stdio::stderr};

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
    String::from("Hello, world!")
}

#[no_mangle]
fn main() -> usize {
    print(&message());

    0
}
