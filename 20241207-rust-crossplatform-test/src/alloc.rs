use core::alloc::GlobalAlloc;

use libc::c_void;

#[cfg(not(target_os = "windows"))]
use libc::{aligned_alloc, free};

#[cfg(target_os = "windows")]
use libc::{aligned_free, aligned_malloc};

#[cfg(not(target_os = "windows"))]
/** Compute alignment and size arguments for [`aligned_alloc`].

The alignment must be a power of 2 at least the size of `*void`, and the size must
be a(n integer) multiple of the alignment.
*/
pub fn aligned_alloc_args(layout: core::alloc::Layout) -> (usize, usize) {
    let align = layout.align().max(core::mem::size_of::<*const c_void>());
    (align, align * layout.size().div_ceil(align))
}

pub struct LibcAllocator;

unsafe impl GlobalAlloc for LibcAllocator {
    unsafe fn alloc(&self, layout: core::alloc::Layout) -> *mut u8 {
        #[cfg(not(target_os = "windows"))]
        {
            let (align, size) = aligned_alloc_args(layout);
            aligned_alloc(align, size).cast::<u8>()
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
