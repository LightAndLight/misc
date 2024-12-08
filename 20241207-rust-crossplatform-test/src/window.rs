extern crate alloc;

#[cfg(target_os = "linux")]
use alloc::ffi::CString;

#[cfg(target_os = "linux")]
use crate::x11;

#[cfg(windows)]
use windows_sys::{
    s,
    Win32::{
        Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, WPARAM},
        System::{
            LibraryLoader::GetModuleHandleA,
            Threading::{GetStartupInfoA, STARTF_USESHOWWINDOW, STARTUPINFOA},
        },
        UI::WindowsAndMessaging::{
            CreateWindowExA, DefWindowProcA, DestroyWindow, RegisterClassA, ShowWindow, SW_NORMAL,
            WNDCLASSA, WNDPROC, WS_OVERLAPPEDWINDOW,
        },
    },
};

pub struct Display {
    #[cfg(target_os = "linux")]
    display: *mut x11::Display,

    #[cfg(windows)]
    startup_info: STARTUPINFOA,

    #[cfg(windows)]
    instance: HINSTANCE,

    #[cfg(not(any(target_os = "linux", windows)))]
    display: compile_error!("Display: unsupported target_os"),
}

pub struct Window<'a> {
    display: &'a Display,

    #[cfg(target_os = "linux")]
    window: x11::Window,

    #[cfg(windows)]
    window: HWND,

    #[cfg(not(any(target_os = "linux", windows)))]
    unsupported: compile_error!("Window: unsupported target_os"),
}

impl Drop for Display {
    fn drop(&mut self) {
        #[cfg(target_os = "linux")]
        unsafe {
            x11::XCloseDisplay(self.display);
        }

        #[cfg(windows)]
        {
            return;
        }

        #[cfg(not(any(target_os = "linux", windows)))]
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
                return None;
            } else {
                return Some(Display { display });
            }
        }

        #[cfg(windows)]
        unsafe {
            let startup_info = {
                let mut startup_info: STARTUPINFOA = core::mem::zeroed();
                GetStartupInfoA(&mut startup_info);
                startup_info
            };

            let instance = GetModuleHandleA(core::ptr::null());
            if instance.is_null() {
                return None;
            }

            return Some(Display {
                startup_info,
                instance,
            });
        }

        #[cfg(not(any(target_os = "linux", windows)))]
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

        #[cfg(windows)]
        #[allow(non_snake_case)]
        unsafe {
            unsafe extern "system" fn WindowProc(
                hwnd: HWND,
                uMsg: u32,
                wParam: WPARAM,
                lParam: LPARAM,
            ) -> LRESULT {
                DefWindowProcA(hwnd, uMsg, wParam, lParam)
            }

            // TODO: what should this actually be?
            let class_name = s!("Test Window Class");

            let wc = WNDCLASSA {
                style: 0,
                lpfnWndProc: WNDPROC::Some(WindowProc),
                cbClsExtra: 0,
                cbWndExtra: 0,
                hInstance: self.instance,
                hIcon: core::ptr::null_mut(),
                hCursor: core::ptr::null_mut(),
                hbrBackground: core::ptr::null_mut(),
                lpszMenuName: core::ptr::null(),
                lpszClassName: class_name,
            };
            let class = RegisterClassA(&wc);
            if class == 0 {
                panic!("RegisterClassA failed");
            }

            let window = CreateWindowExA(
                0,
                class_name,
                s!("Some text"),
                WS_OVERLAPPEDWINDOW,
                x.try_into().unwrap(),
                y.try_into().unwrap(),
                width.try_into().unwrap(),
                height.try_into().unwrap(),
                core::ptr::null_mut(),
                core::ptr::null_mut(),
                self.instance,
                core::ptr::null_mut(),
            );
            if window.is_null() {
                panic!("CreateWindowExA failed");
            }

            let nShowCmd = if self.startup_info.dwFlags & STARTF_USESHOWWINDOW == 1 {
                self.startup_info.wShowWindow.into()
            } else {
                SW_NORMAL
            };
            ShowWindow(window, nShowCmd);

            Window {
                display: self,
                window,
            }
        }

        #[cfg(not(any(target_os = "linux", windows)))]
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

        #[cfg(windows)]
        unsafe {
            DestroyWindow(self.window);
        }

        #[cfg(not(any(target_os = "linux", windows)))]
        {
            compile_error!("Window::destroy: unsupported target_os")
        }
    }
}
