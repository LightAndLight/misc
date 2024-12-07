#![no_std]
#![no_main]

use core::panic::PanicInfo;

use libc::{abort, fprintf, FILE};

#[cfg(target_os = "linux")]
#[link(name = "c")]
extern "C" {
    pub static mut linux_stdin: *mut FILE;
    pub static mut linux_stdout: *mut FILE;
    pub static mut linux_stderr: *mut FILE;
}

// https://users.rust-lang.org/t/how-to-capture-cs-stdout-stderr/43079/3

#[cfg(target_os = "windows")]
#[link(name = "ucrt")]
extern "C" {
    fn __acrt_iob_func(fileno: u32) -> *mut FILE;
}

#[cfg(target_os = "linux")]
fn stdin() -> *mut FILE {
  linux_stdin
}

#[cfg(target_os = "windows")]
fn stdin() -> *mut FILE {
  unsafe { __acrt_iob_func(0) }
}

#[cfg(not(any(target_os = "linux", target_os = "windows")))]
fn stdin() -> *mut FILE {
  compile_error!("stdin: unsupported target_os")
}

#[cfg(target_os = "linux")]
fn stdout() -> *mut FILE {
  linux_stdout
}

#[cfg(target_os = "windows")]
fn stdout() -> *mut FILE {
  unsafe { __acrt_iob_func(1) }
}

#[cfg(not(any(target_os = "linux", target_os = "windows")))]
fn stdout() -> *mut FILE {
  compile_error!("stdout: unsupported target_os")
}

#[cfg(target_os = "linux")]
fn stderr() -> *mut FILE {
  linux_stderr
}

#[cfg(target_os = "windows")]
fn stderr() -> *mut FILE {
  unsafe { __acrt_iob_func(2) }
}

#[cfg(not(any(target_os = "linux", target_os = "windows")))]
fn stderr() -> *mut FILE {
  compile_error!("stderr: unsupported target_os")
}

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
