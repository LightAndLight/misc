use libc::{c_char, c_int, c_uint, c_ulong};

pub type Display = _XDisplay;

pub enum _XDisplay {}

#[link(name = "X11")]
extern "C" {
    pub fn XOpenDisplay(display_name: *const c_char) -> *mut Display;
    pub fn XCloseDisplay(display: *mut Display) -> c_int;

    pub fn XFlush(display: *mut Display);

    pub fn XBlackPixel(display: *mut Display, screen_number: c_int) -> c_ulong;
    pub fn XWhitePixel(display: *mut Display, screen_number: c_int) -> c_ulong;
}

pub type XID = c_ulong;

pub type Window = XID;

#[link(name = "X11")]
extern "C" {
    pub fn XRootWindow(display: *mut Display, screen_number: c_int) -> Window;

    pub fn XCreateSimpleWindow(
        display: *mut Display,
        parent: Window,
        x: c_int,
        y: c_int,
        width: c_uint,
        height: c_uint,
        border_width: c_uint,
        border: c_ulong,
        background: c_ulong,
    ) -> Window;

    pub fn XMapWindow(display: *mut Display, w: Window);

    pub fn XDestroyWindow(display: *mut Display, w: Window) -> c_int;
}
