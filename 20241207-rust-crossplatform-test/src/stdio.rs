use libc::FILE;

#[cfg(target_os = "linux")]
mod linux {
    use libc::FILE;

    #[link(name = "c")]
    extern "C" {
        pub static mut stdin: *mut FILE;
        pub static mut stdout: *mut FILE;
        pub static mut stderr: *mut FILE;
    }
}

#[cfg(target_os = "macos")]
mod macos {
    use libc::FILE;

    // https://stackoverflow.com/questions/28362994/where-is-my-stdio-h-in-mac
    // https://stackoverflow.com/questions/47584399/how-to-get-the-corresponding-ctype-for-stdout-on-os-x-using-libc-dylib
    #[link(name = "c")]
    extern "C" {
        pub static mut __stdinp: *mut FILE;
        pub static mut __stdoutp: *mut FILE;
        pub static mut __stderrp: *mut FILE;
    }
}

#[cfg(target_os = "windows")]
mod windows {
    use libc::FILE;

    // https://users.rust-lang.org/t/how-to-capture-cs-stdout-stderr/43079/3
    #[link(name = "ucrt")]
    extern "C" {
        pub fn __acrt_iob_func(fileno: u32) -> *mut FILE;
    }
}

pub unsafe fn stdin() -> *mut FILE {
    #[cfg(target_os = "linux")]
    {
        linux::stdin
    }

    #[cfg(target_os = "macos")]
    {
        macos::__stdinp
    }

    #[cfg(target_os = "windows")]
    {
        windows::__acrt_iob_func(0)
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
    {
        compile_error!("stdin: unsupported target_os")
    }
}

pub unsafe fn stdout() -> *mut FILE {
    #[cfg(target_os = "linux")]
    {
        linux::stdout
    }

    #[cfg(target_os = "macos")]
    {
        macos::__stdoutp
    }

    #[cfg(target_os = "windows")]
    {
        windows::__acrt_iob_func(1)
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
    {
        compile_error!("stdout: unsupported target_os")
    }
}

pub unsafe fn stderr() -> *mut FILE {
    #[cfg(target_os = "linux")]
    {
        linux::stderr
    }

    #[cfg(target_os = "macos")]
    {
        macos::__stderrp
    }

    #[cfg(target_os = "windows")]
    {
        windows::__acrt_iob_func(2)
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
    {
        compile_error!("stderr: unsupported target_os")
    }
}
