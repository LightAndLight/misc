pub fn sleep(num_seconds: u32) {
    #[cfg(target_os = "linux")]
    unsafe {
        libc::sleep(num_seconds);
    }

    #[cfg(windows)]
    unsafe {
        windows_sys::Win32::System::Threading::Sleep(num_seconds * 1_000);
    }

    #[cfg(target_os = "macos")]
    unsafe {
        libc::sleep(num_seconds);
    }

    #[cfg(not(any(target_os = "linux", windows, target_os = "macos")))]
    {
        compile_error!("sleep: unsupported target_os");
    }
}
