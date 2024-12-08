use core::ffi::c_void;

use libc::fwrite;

use crate::stdio::{stderr, stdout};

pub fn print(s: &str) {
    unsafe {
        let ptr = s.as_ptr().cast::<c_void>();
        let len = s.len();
        let _bytes_written = fwrite(ptr, 1, len, stdout());
    }
}

pub fn eprint(s: &str) {
    unsafe {
        let ptr = s.as_ptr().cast::<c_void>();
        let len = s.len();
        let _bytes_written = fwrite(ptr, 1, len, stderr());
    }
}
