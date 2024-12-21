//! Foundation.framework

#![allow(non_snake_case)]

use core::ffi::{c_char, CStr};

use crate::{macos::objc::*, msg_send};

use super::core_graphics::cg_geometry::CGRect;

#[link(name = "Foundation", kind = "framework")]
extern "C" {}

#[allow(dead_code)]
pub struct NSString(id);

impl NSString {
    pub const CLASSNAME: *const c_char = c"NSString".as_ptr();

    pub fn stringWithUTF8String(str: &CStr) -> Self {
        Self(unsafe {
            msg_send!(
                fn(Class, SEL, *const c_char) -> id,
                objc_getClass(Self::CLASSNAME),
                c"stringWithUTF8String:",
                str.as_ptr()
            )
        })
    }
}

pub type NSRect = CGRect;

#[repr(C)]
pub struct NSNotification(id);

impl NSNotification {
    pub fn object(&self) -> id {
        unsafe { msg_send!(fn(id, SEL) -> id, self.0, c"object") }
    }
}
