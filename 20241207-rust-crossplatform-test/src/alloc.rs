use core::alloc::GlobalAlloc;

use libc::c_void;

#[cfg(not(target_os = "windows"))]
use libc::{aligned_alloc, free};

#[cfg(target_os = "windows")]
use libc::{aligned_free, aligned_malloc};

pub struct LibcAllocator;

unsafe impl GlobalAlloc for LibcAllocator {
    unsafe fn alloc(&self, layout: core::alloc::Layout) -> *mut u8 {
        #[cfg(not(target_os = "windows"))]
        {
            aligned_alloc(layout.align(), layout.size()).cast::<u8>()
        }

        #[cfg(target_os = "windows")]
        {
            aligned_malloc(layout.size(), layout.align()).cast::<u8>()
        }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: core::alloc::Layout) {
        #[cfg(not(target_os = "windows"))]
        {
            free(ptr.cast::<c_void>())
        }

        #[cfg(target_os = "windows")]
        {
            aligned_free(ptr.cast::<c_void>())
        }
    }
}
