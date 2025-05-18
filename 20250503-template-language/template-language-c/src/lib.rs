#![no_std]

extern crate alloc;
extern crate libc_alloc;

#[cfg(not(test))]
use alloc::format;
use alloc::vec::Vec;
#[cfg(not(test))]
use core::panic::PanicInfo;

use core::ffi::{CStr, c_char, c_void};
use template_language::compile::{self, Template, compile_inlined, read_inlined};

#[cfg(not(test))]
#[panic_handler]
fn panic_handler(panic_info: &PanicInfo) -> ! {
    let message = if let Some(location) = panic_info.location() {
        format!(
            "tpl panic ({}:{}:{}): {}",
            location.file(),
            location.line(),
            location.column(),
            panic_info.message()
        )
    } else {
        format!("tpl panic (location unknown): {}", panic_info.message())
    };

    unsafe {
        libc::write(
            libc::STDERR_FILENO,
            message.as_ptr().cast::<c_void>(),
            message.len(),
        );

        libc::abort()
    }
}

/**
# Safety

* `len` must not be larger than the size of `buffer`
*/
#[unsafe(no_mangle)]
pub unsafe extern "C" fn tpl_template_compile(buffer: *const u8, len: usize) -> *mut Template {
    let input = unsafe { core::slice::from_raw_parts(buffer, len) };
    let template = compile_inlined(input);
    unsafe {
        let ptr = libc::malloc(size_of::<Template>()).cast::<Template>();
        *ptr = template;
        ptr
    }
}

/**
# Safety

* `path` must be a valid null-terminated string
*/
#[unsafe(no_mangle)]
pub unsafe extern "C" fn tpl_template_read(path: *const c_char) -> *mut Template {
    let path = unsafe { CStr::from_ptr(path) };
    let template = read_inlined(path);
    unsafe {
        let ptr = libc::malloc(size_of::<Template>()).cast::<Template>();
        *ptr = template;
        ptr
    }
}

/**
# Safety

* The [`Template`] must not be used after it has been freed
*/
#[unsafe(no_mangle)]
pub unsafe extern "C" fn tpl_template_free(template: *mut Template) {
    unsafe { libc::free(template.cast::<c_void>()) }
}

#[repr(C)]
pub struct Arg<'a> {
    pub name: &'a CStr,
    pub value: &'a CStr,
}

/**
# Safety

* `args` must be a valid pointer
* `args_count` must less than or equal to the number of items in `args`
*/
#[unsafe(no_mangle)]
pub unsafe extern "C" fn tpl_template_run(
    template: *mut Template,
    write: extern "C" fn(*const c_void, usize),
    args: *const Arg,
    args_count: usize,
) {
    let args: &[Arg] = unsafe { core::slice::from_raw_parts(args, args_count) };

    // TODO: I don't like that I'm forced to allocate to work well with FFI types.
    let args = args
        .iter()
        .map(|arg| compile::Arg {
            name: arg.name.to_str().unwrap(),
            value: arg.value.to_str().unwrap().into(),
        })
        .collect::<Vec<_>>();

    unsafe {
        template.as_ref().unwrap().run(
            &mut |slice| {
                write(slice.as_ptr().cast::<c_void>(), slice.len());
            },
            &args,
        );
    }
}
