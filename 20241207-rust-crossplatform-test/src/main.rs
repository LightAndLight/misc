#![no_std]
#![no_main]

use core::panic::PanicInfo;

use libc::{abort, fprintf};

use crossplatform_test::stdio::{stderr, stdout};

#[panic_handler]
fn panic_handler(_panic_info: &PanicInfo) -> ! {
    unsafe {
        fprintf(stderr(), "panic".as_ptr().cast::<i8>());
        abort()
    }
}

#[no_mangle]
fn main() -> usize {
    unsafe {
        fprintf(stdout(), "Hello, world!\n".as_ptr().cast::<i8>());
    }

    0
}
