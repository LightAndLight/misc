extern crate alloc;

#[cfg(target_os = "linux")]
use alloc::ffi::CString;

#[cfg(target_os = "linux")]
use crate::x11;

pub struct Display {
    #[cfg(target_os = "linux")]
    display: *mut x11::Display,

    #[cfg(not(any(target_os = "linux")))]
    display: compile_error!("Display: unsupported target_os"),
}

pub struct Window<'a> {
    display: &'a Display,

    #[cfg(target_os = "linux")]
    window: x11::Window,

    #[cfg(not(any(target_os = "linux")))]
    windows: compile_error!("Window: unsupported target_os"),
}

impl Drop for Display {
    fn drop(&mut self) {
        #[cfg(target_os = "linux")]
        unsafe {
            x11::XCloseDisplay(self.display);
        }

        #[cfg(not(any(target_os = "linux")))]
        compile_error!("Display::drop: unsupported target_os");
    }
}

impl Display {
    pub fn open() -> Option<Self> {
        #[cfg(target_os = "linux")]
        unsafe {
            let display_name = CString::new(":0").unwrap();
            let display = x11::XOpenDisplay(display_name.as_ptr());
            if display.is_null() {
                None
            } else {
                Some(Display { display })
            }
        }

        #[cfg(not(any(target_os = "linux")))]
        {
            compile_error!("Display::open: unsupported target_os")
        }
    }

    pub fn create_window(&mut self, x: i32, y: i32, width: u32, height: u32) -> Window {
        #[cfg(target_os = "linux")]
        unsafe {
            let screen_number = 0;
            let root_window = x11::XRootWindow(self.display, screen_number);

            let window = x11::XCreateSimpleWindow(
                self.display,
                root_window,
                x,
                y,
                width,
                height,
                0,
                0,
                x11::XWhitePixel(self.display, screen_number),
            );

            x11::XMapWindow(self.display, window);
            x11::XFlush(self.display);

            Window {
                display: self,
                window,
            }
        }

        #[cfg(not(any(target_os = "linux")))]
        {
            compile_error!("Display::create_window: unsupported target_os")
        }
    }
}

impl Window<'_> {
    pub fn destroy(self) {
        #[cfg(target_os = "linux")]
        unsafe {
            x11::XDestroyWindow(self.display.display, self.window);
        }

        #[cfg(not(any(target_os = "linux")))]
        {
            compile_error!("Window::destroy: unsupported target_os")
        }
    }
}
