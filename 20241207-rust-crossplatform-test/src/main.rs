#![no_std]
#![no_main]

extern crate alloc;

use core::panic::PanicInfo;

use alloc::string::String;
use libc::{abort, fprintf};

use crossplatform_test::{
    alloc::LibcAllocator,
    stdio::{stderr, stdout},
};

#[global_allocator]
static LIBC_ALLOCATOR: LibcAllocator = LibcAllocator;

#[panic_handler]
fn panic_handler(_panic_info: &PanicInfo) -> ! {
    unsafe {
        fprintf(stderr(), "panic".as_ptr().cast::<i8>());
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
    unsafe {
        fprintf(stdout(), message().as_ptr().cast::<i8>());
    }

    0
}
