use core::alloc::GlobalAlloc;

use libc::{c_void, free};

#[cfg(not(target_os = "windows"))]
use libc::aligned_alloc;

#[cfg(target_os = "windows")]
use libc::aligned_malloc;

pub struct LibcAllocator;

unsafe impl GlobalAlloc for LibcAllocator {
    unsafe fn alloc(&self, layout: core::alloc::Layout) -> *mut u8 {
        #[cfg(not(target_os = "windows"))]
        {
            aligned_alloc(layout.align(), layout.size()).cast::<u8>()
        }

        #[cfg(target_os = "windows")]
        {
            aligned_malloc(layout.align(), layout.size()).cast::<u8>()
        }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: core::alloc::Layout) {
        free(ptr.cast::<c_void>())
    }
}
