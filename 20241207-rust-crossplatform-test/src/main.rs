#![no_std]
#![no_main]
#![windows_subsystem = "windows"]
#![feature(alloc_error_handler)]

extern crate alloc;

use core::{ffi::c_int, panic::PanicInfo};

use alloc::{alloc::Layout, format, string::String};
use libc::abort;

use crossplatform_test::{
    alloc::LibcAllocator,
    io::{eprint, eprint_usize, eprintln, println},
    thread::sleep,
};

#[cfg(not(target_os = "windows"))]
use crossplatform_test::alloc::aligned_alloc_args;

#[cfg(target_os = "macos")]
use crossplatform_test::{
    macos::{app_kit, core_graphics::cg_geometry::*, foundation, objc::*},
    msg_send,
};

#[global_allocator]
static LIBC_ALLOCATOR: LibcAllocator = LibcAllocator;

#[panic_handler]
fn panic_handler(panic_info: &PanicInfo) -> ! {
    match panic_info.location() {
        None => eprintln(&format!(
            "(unknown location): panic: {}",
            panic_info.message()
        )),
        Some(location) => {
            eprintln(&format!(
                "{}:{}:{}: panic: {}",
                location.file(),
                location.line(),
                location.column(),
                panic_info.message()
            ));
        }
    }
    unsafe { abort() }
}

#[cfg(target_os = "macos")]
#[link(name = "c")]
extern "C" {
    static mut errno: c_int;
}

#[alloc_error_handler]
fn alloc_error_handler(layout: Layout) -> ! {
    #[cfg(target_os = "macos")]
    {
        eprint("alloc error (");
        unsafe {
            match errno {
                libc::EINVAL => eprint("EINVAL"),
                libc::ENOMEM => eprint("ENOMEM"),
                _ => eprint("unknown error"),
            }
        }
        eprint(") (");
        let (align, size) = aligned_alloc_args(layout);
        eprint("align = ");
        eprint_usize(align);
        eprint(", size = ");
        eprint_usize(size);
        eprintln(")");
    }

    #[cfg(not(target_os = "macos"))]
    {
        eprintln("alloc error");
    }

    unsafe { abort() }
}

// Test the global allocator
#[inline(never)]
fn message() -> String {
    String::from("Hello, world!")
}

#[no_mangle]
fn main() -> usize {
    println(&message());

    #[cfg(target_os = "macos")]
    {
        let mut app: app_kit::NSApplication = app_kit::NSApplication::sharedApplication();
        let _ = app.setActivationPolicy(app_kit::NSApplicationActivationPolicy::Regular);

        unsafe {
            let MyDelegate = objc_allocateClassPair(
                objc_getClass(NSObject::CLASSNAME),
                c"MyDelegate".as_ptr(),
                0,
            );

            extern "C" fn applicationShouldTerminateAfterLastWindowClosed_(
                _this: id,
                _cmd: SEL,
                _sender: app_kit::NSApplication,
            ) -> BOOL {
                YES
            }

            class_addMethod(
                MyDelegate,
                sel_registerName(c"applicationShouldTerminateAfterLastWindowClosed:".as_ptr()),
                applicationShouldTerminateAfterLastWindowClosed_ as IMP,
                c"B@:@".as_ptr(),
            );

            extern "C" fn applicationDidFinishLaunching_(
                _this: id,
                _cmd: SEL,
                notification: foundation::NSNotification,
            ) {
                let mut app = unsafe { app_kit::NSApplication::from_id(notification.object()) };
                app.stop(nil());
            }

            class_addMethod(
                MyDelegate,
                sel_registerName(c"applicationDidFinishLaunching:".as_ptr()),
                applicationDidFinishLaunching_ as IMP,
                c"v@:@".as_ptr(),
            );

            objc_registerClassPair(MyDelegate);

            let app_delegate = msg_send!(fn(Class, SEL) -> id, MyDelegate, c"new");
            app.setDelegate(app_delegate);
        }

        let rect = CGRect {
            origin: CGPoint { x: 0., y: 0. },
            size: CGSize {
                width: 640.,
                height: 480.,
            },
        };

        let mut window: app_kit::NSWindow = app_kit::NSWindow::alloc().initWithContentRect(
            rect,
            app_kit::NSWindowStyleMask::Titled
                | app_kit::NSWindowStyleMask::Closable
                | app_kit::NSWindowStyleMask::Miniaturizable,
            app_kit::NSBackingStoreType::Buffered,
            NO,
        );

        // window.setTitle(foundation::NSString::stringWithUTF8String(c"Hello, world!"));
        window.makeKeyAndOrderFront(nil());
        window.setReleasedWhenClosed(YES);
        app.activateIgnoringOtherApps(YES);
        app.run();

        sleep(2);

        window.close();
        eprintln("Bye!");
    }

    #[cfg(not(target_os = "macos"))]
    {
        let mut display = Display::open().unwrap_or_else(|| {
            eprintln("Failed to open display");
            unsafe { exit(1) }
        });

        let window = display.create_window(0, 0, 640, 480);

        sleep(2);

        window.destroy();
    }

    0
}
