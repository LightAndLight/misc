#![no_std]
#![no_main]

use core::panic::PanicInfo;

use libc::{abort, fprintf, FILE};

#[link(name = "c")]
extern "C" {
    pub static mut stdin: *mut FILE;
    pub static mut stdout: *mut FILE;
    pub static mut stderr: *mut FILE;
}

#[panic_handler]
fn panic_handler(_panic_info: &PanicInfo) -> ! {
    unsafe {
        fprintf(stderr, "panic".as_ptr().cast::<i8>());
        abort()
    }
}

#[no_mangle]
fn main() {
    unsafe {
        fprintf(stdout, "Hello, world!".as_ptr().cast::<i8>());
    }
}
