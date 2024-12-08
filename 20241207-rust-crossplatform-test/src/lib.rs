#![no_std]

pub mod alloc;
pub mod io;
pub mod stdio;
pub mod thread;
pub mod window;

#[cfg(target_os = "linux")]
pub mod x11;
