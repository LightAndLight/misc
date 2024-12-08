use core::alloc::GlobalAlloc;

use libc::{aligned_alloc, c_void, free};

pub struct LibcAllocator;

unsafe impl GlobalAlloc for LibcAllocator {
    unsafe fn alloc(&self, layout: core::alloc::Layout) -> *mut u8 {
        aligned_alloc(layout.align(), layout.size()).cast::<u8>()
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: core::alloc::Layout) {
        free(ptr.cast::<c_void>())
    }
}
