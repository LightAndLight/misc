#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use core::ffi::{c_char, c_int, c_void};

// https://github.com/opensource-apple/objc4/blob/master/runtime/objc-private.h
pub enum objc_class {}
pub enum objc_object {}
pub enum objc_selector {}

// https://github.com/opensource-apple/objc4/blob/master/runtime/objc.h
pub type Class = *mut objc_class;
pub type SEL = *mut objc_selector;
pub type id = *mut objc_object;
pub type IMP = *const extern "C" fn(id, SEL, ...) -> id;

// runtime/objc.h

/* pub type BOOL = c_char;

`BOOL` is declared as a signed char, but due to ABI constraints
`objc_msgSend` can't recieve signed chars. Instead, it needs
to take signed ints. To avoid casting every time, we widen
`BOOL`'s type here.
*/
pub type BOOL = c_int;

pub const YES: BOOL = 1;
pub const NO: BOOL = 0;

pub fn nil<T>() -> *mut T {
    core::ptr::null_mut()
}

#[cfg(target_os = "macos")]
#[link(name = "objc")]
extern "C" {
    // https://github.com/opensource-apple/objc4/blob/master/runtime/runtime.h
    pub fn objc_getClass(name: *const c_char) -> Class;
    pub fn objc_msgSend(this: *mut c_void, op: SEL, ...) -> id;
    pub fn objc_allocateClassPair(
        superclass: Class,
        name: *const c_char,
        extraBytes: usize,
    ) -> Class;
    pub fn objc_registerClassPair(cls: Class);
    pub fn class_getName(cls: Class) -> *const c_char;
    pub fn class_addMethod(cls: Class, name: SEL, imp: IMP, types: *const c_char) -> BOOL;
    pub fn sel_getUid(name: *const c_char) -> SEL;
    pub fn sel_registerName(name: *const c_char) -> SEL;
}

#[macro_export]
macro_rules! msg_send {
    (fn($($arg:ty),*), $obj:expr, $msg:expr) => {
        {
            use core::ffi::c_void;
            use $crate::macos::objc::SEL;

            core::mem::transmute::<
                *const unsafe extern "C" fn(*mut c_void, SEL, ...) -> c_void,
                extern "C" fn($($arg),*),
            >(objc_msgSend as *const _)(
                $obj,
                sel_getUid($msg.as_ptr())
            )
        }
    };

    (fn($($arg:ty),*) -> $ret:ty, $obj:expr, $msg:expr) => {
        {
            use core::ffi::c_void;
            use $crate::macos::objc::SEL;

            core::mem::transmute::<
                *const unsafe extern "C" fn(*mut c_void, SEL, ...) -> c_void,
                extern "C" fn($($arg),*) -> $ret,
            >(objc_msgSend as *const _)(
                $obj,
                sel_getUid($msg.as_ptr())
            )
        }
    };

    (fn($($arg_ty:ty),*), $obj:expr, $msg:expr, $($arg:expr),* ) => {
        {
            use core::ffi::c_void;
            use $crate::macos::objc::SEL;

            core::mem::transmute::<
                *const unsafe extern "C" fn(*mut c_void, SEL, ...) -> id,
                extern "C" fn($($arg_ty),*),
            >(objc_msgSend as *const _)(
                $obj,
                sel_getUid($msg.as_ptr()),
                $($arg,)*
            )
        }
    };

    (fn($($arg_ty:ty),*) -> $ret:ty, $obj:expr, $msg:expr, $($arg:expr),* ) => {
        {
            use core::ffi::c_void;
            use $crate::macos::objc::SEL;

            core::mem::transmute::<
                *const unsafe extern "C" fn(*mut c_void, SEL, ...) -> id,
                extern "C" fn($($arg_ty),*) -> $ret,
            >(objc_msgSend as *const _)(
                $obj,
                sel_getUid($msg.as_ptr()),
                $($arg,)*
            )
        }
    };
}

pub struct NSObject;

impl NSObject {
    pub const CLASSNAME: *const c_char = c"NSObject".as_ptr();
}
