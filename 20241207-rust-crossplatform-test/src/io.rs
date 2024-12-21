use core::{ffi::{c_char, c_void}, hint::unreachable_unchecked};

use libc::{fputc, fwrite};

use crate::stdio::{stderr, stdout};

pub fn print(s: &str) {
    unsafe {
        let ptr = s.as_ptr().cast::<c_void>();
        let len = s.len();
        let _bytes_written = fwrite(ptr, 1, len, stdout());
    }
}

pub fn put(c: char) {
    unsafe {
        fputc(c as i32, stdout());
    }
}

pub fn println(s: &str) {
    print(s);
    put('\n');
}

pub fn print_cstr(ptr: *const c_char) {
    unsafe {
        let mut ptr = ptr;
        while *ptr != 0 {
            let _bytes_written = fwrite(ptr.cast::<c_void>(), 1, 1, stdout());
            ptr = ptr.add(1);
        }
    }
}

pub fn println_cstr(ptr: *const c_char) {
    print_cstr(ptr);
    put('\n');
}

pub fn eprint(s: &str) {
    unsafe {
        let ptr = s.as_ptr().cast::<c_void>();
        let len = s.len();
        let _bytes_written = fwrite(ptr, 1, len, stderr());
    }
}

pub fn eput(c: char) {
    unsafe {
        fputc(c as i32, stderr());
    }
}

pub fn eprintln(s: &str) {
    eprint(s);
    eput('\n');
}

pub fn eprint_cstr(ptr: *const c_char) {
    unsafe {
        let mut ptr = ptr;
        while *ptr != 0 {
            let _bytes_written = fwrite(ptr.cast::<c_void>(), 1, 1, stderr());
            ptr = ptr.add(1);
        }
    }
}

pub fn eprint_usize(mut i: usize) {
    fn to_char(i: usize) -> char {
        match i {
           0 => '0',
           1 => '1',
           2 => '2',
           3 => '3',
           4 => '4',
           5 => '5',
           6 => '6',
           7 => '7',
           8 => '8', 
           9 => '9',
           _ => unsafe { unreachable_unchecked() }
        }
    }

    loop {
        let q = i / 10;
        if q > 0 {
            eput(to_char(q));
            i = i % (q * 10);
        } else {
            let r = i % 10;
            eput(to_char(r));
            break;
        }
    }
}
