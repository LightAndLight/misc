//! AppKit.framework
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

use core::ffi::c_char;

use crate::msg_send;

use super::{
    foundation::{NSRect, NSString},
    objc::*,
};

#[link(name = "AppKit", kind = "framework")]
extern "C" {}

#[repr(C)]
pub struct NSApplication(id);

impl NSApplication {
    pub const CLASSNAME: *const c_char = c"NSApplication".as_ptr();

    pub unsafe fn from_id(id: id) -> Self {
        Self(id)
    }

    pub fn sharedApplication() -> Self {
        Self(unsafe {
            msg_send!(
                fn(Class, SEL) -> id,
                objc_getClass(Self::CLASSNAME),
                c"sharedApplication"
            )
        })
    }

    pub fn setActivationPolicy(&mut self, activationPolicy: NSApplicationActivationPolicy) -> BOOL {
        unsafe {
            msg_send!(
                fn(id, SEL, NSApplicationActivationPolicy) -> BOOL,
                self.0,
                c"setActivationPolicy:",
                activationPolicy
            )
        }
    }

    pub fn activateIgnoringOtherApps(&mut self, ignoreOtherApps: BOOL) {
        unsafe {
            msg_send!(
                fn(id, SEL, BOOL),
                self.0,
                c"activateIgnoringOtherApps:",
                ignoreOtherApps
            )
        }
    }

    pub fn finishLaunching(&mut self) {
        unsafe { msg_send!(fn(id, SEL), self.0, c"finishLaunching") }
    }

    pub fn run(&mut self) {
        unsafe { msg_send!(fn(id, SEL), self.0, c"run") }
    }

    pub fn stop(&mut self, sender: id) {
        unsafe { msg_send!(fn(id, SEL, id), self.0, c"stop:", sender) }
    }

    pub fn setDelegate(&mut self, val: id) {
        unsafe { msg_send!(fn(id, SEL, id), self.0, c"setDelegate:", val) }
    }
}

pub const run: *const c_char = c"run".as_ptr();

#[repr(isize)]
pub enum NSApplicationActivationPolicy {
    Regular,
    Accessory,
    Prohibited,
}

// NSWindow.h
pub struct NSWindow(id);

impl NSWindow {
    pub const CLASSNAME: *const c_char = c"NSWindow".as_ptr();

    pub fn alloc() -> Self {
        Self(unsafe {
            msg_send!(
                fn(Class, SEL) -> id,
                objc_getClass(Self::CLASSNAME),
                c"alloc"
            )
        })
    }

    pub fn initWithContentRect(
        self,
        contentRect: NSRect,
        styleMask: NSWindowStyleMask,
        backing: NSBackingStoreType,
        defer: BOOL,
    ) -> Self {
        Self(unsafe {
            msg_send!(
                fn(id, SEL, NSRect, NSWindowStyleMask, NSBackingStoreType, BOOL) -> id,
                self.0,
                c"initWithContentRect:styleMask:backing:defer:",
                contentRect,
                styleMask,
                backing,
                defer
            )
        })
    }

    pub fn setReleasedWhenClosed(&mut self, value: BOOL) {
        unsafe { msg_send!(fn(id, SEL, BOOL), self.0, c"setReleasedWhenClosed:", value) }
    }

    pub fn close(self) {
        unsafe { msg_send!(fn(id, SEL), self.0, c"close") }
    }

    pub fn styleMask(&self) -> NSWindowStyleMask {
        unsafe { msg_send!(fn(id, SEL) -> NSWindowStyleMask, self.0, c"styleMask") }
    }

    pub fn setTitle(&mut self, title: NSString) {
        unsafe { msg_send!(fn(id, SEL, NSString), self.0, c"setTitle:", title) }
    }

    pub fn makeKeyAndOrderFront(&mut self, sender: id) {
        unsafe { msg_send!(fn(id, SEL, id), self.0, c"makeKeyAndOrderFront:", sender) }
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct NSWindowStyleMask(u64);

impl NSWindowStyleMask {
    pub const Borderless: Self = Self(0);
    pub const Titled: Self = Self(1 << 0);
    pub const Closable: Self = Self(1 << 1);
    pub const Miniaturizable: Self = Self(1 << 2);
    pub const Resizable: Self = Self(1 << 3);
}

impl core::ops::BitOr for NSWindowStyleMask {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl core::ops::BitAnd for NSWindowStyleMask {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

// NSGraphics.h

#[repr(C)]
pub struct NSBackingStoreType(u64);

impl NSBackingStoreType {
    // Retained API_DEPRECATED_WITH_REPLACEMENT("NSBackingStoreBuffered", macos(10.0,10.13)) = 0,
    // Nonretained API_DEPRECATED_WITH_REPLACEMENT("NSBackingStoreBuffered", macos(10.0,10.13)) = 1,
    pub const Buffered: Self = Self(2);
}
