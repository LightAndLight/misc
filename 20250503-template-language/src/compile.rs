use core::mem::MaybeUninit;
use core::ptr::null_mut;

use alloc::ffi::CString;
use alloc::vec;
use alloc::vec::Vec;

#[derive(Debug)]
pub struct Template {
    args: u32,
    ops: Vec<Op>,
    strings: Vec<u8>,
}

#[derive(Debug, PartialEq, Eq)]
#[repr(u64)]
pub enum Op {
    Var(u32),
    String { addr: u32, len: u32 },
}

fn skip_whitespace(input: &[u8], pos: &mut usize) {
    while *pos < input.len() {
        if input[*pos].is_ascii_whitespace() {
            *pos += 1;
        } else {
            break;
        }
    }
}

fn parse_ident<'a>(input: &'a [u8], pos: &mut usize) -> &'a [u8] {
    let ident_start = *pos;
    while *pos < input.len() {
        if input[*pos].is_ascii_alphanumeric() {
            *pos += 1;
        } else {
            break;
        }
    }
    let ident_end = *pos;

    skip_whitespace(input, pos);
    &input[ident_start..ident_end]
}

pub fn compile(input: &[u8]) -> Template {
    let mut args = vec![];
    let mut ops = vec![];
    let mut strings = vec![];

    let mut pos = 0;
    let mut current_string_addr = 0;
    let mut current_chunk_start_pos = 0;
    while pos < input.len() {
        match input[pos] {
            b'{' => {
                ops.push(Op::String {
                    addr: current_string_addr,
                    len: pos as u32 - current_chunk_start_pos as u32,
                });
                strings.extend_from_slice(&input[current_chunk_start_pos..pos]);
                current_string_addr = strings.len() as u32;

                pos += 1;

                skip_whitespace(input, &mut pos);
                let ident = parse_ident(input, &mut pos);

                let o_arg_index = args
                    .iter()
                    .enumerate()
                    .find_map(|(ix, arg)| if *arg == ident { Some(ix) } else { None });
                let arg_index = if let Some(arg_index) = o_arg_index {
                    arg_index
                } else {
                    let arg_index = args.len();
                    args.push(ident);
                    arg_index
                };

                ops.push(Op::Var(arg_index as u32));

                match input[pos] {
                    b'}' => {
                        pos += 1;
                        current_chunk_start_pos = pos;
                    }
                    _ => todo!(),
                }
            }
            b'\\' => match input[pos + 1] {
                b'\\' => {
                    strings.push(b'\\');
                    pos += 2;
                    current_chunk_start_pos = pos;
                }
                b'{' => {
                    strings.push(b'{');
                    pos += 2;
                    current_chunk_start_pos = pos;
                }
                _ => todo!(),
            },
            _ => {
                pos += 1;
            }
        }
    }

    let len = pos as u32 - current_chunk_start_pos as u32;
    if len > 0 {
        ops.push(Op::String {
            addr: current_string_addr,
            len,
        });
        strings.extend_from_slice(&input[current_chunk_start_pos..pos]);
    }

    Template {
        args: args.len() as u32,
        ops,
        strings,
    }
}

pub fn read_compile(path: &str) -> Template {
    let path_c = CString::new(path).unwrap();

    let input = unsafe {
        let fd = libc::open(path_c.as_ptr(), libc::O_RDONLY);
        if fd == -1 {
            let errno = *libc::__errno_location();
            if errno == 2 {
                panic!("file {} not found", path);
            } else {
                panic!("reading file {} failed with error code {}", path, errno);
            }
        }

        let mut stat = MaybeUninit::<libc::stat>::uninit();
        libc::fstat(fd, stat.as_mut_ptr());
        // TODO: check errors
        let len = stat.assume_init().st_size as usize;
        let ptr = libc::mmap(null_mut(), len, libc::PROT_READ, libc::MAP_PRIVATE, fd, 0);
        // TODO: check errors
        core::slice::from_raw_parts(ptr.cast::<u8>(), len)
    };

    compile(input)
}

pub enum Value<'a> {
    String(&'a [u8]),
    Template {
        template: &'a Template,
        args: &'a [Value<'a>],
    },
}

impl<'a> From<&'a str> for Value<'a> {
    fn from(value: &'a str) -> Self {
        Value::String(value.as_bytes())
    }
}

pub struct Arg<'a> {
    pub name: &'a str,
    pub value: Value<'a>,
}

pub struct Chunks<'a> {
    template: &'a Template,
    op: usize,
    stack: Vec<(&'a Template, &'a [Value<'a>], usize)>,
    args: &'a [Arg<'a>],
}

impl Template {
    pub fn chunks<'a>(&'a self, args: &'a [Arg<'a>]) -> Chunks<'a> {
        Chunks {
            template: self,
            op: 0,
            stack: Vec::new(),
            args,
        }
    }
}

impl<'a> Iterator for Chunks<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.op < self.template.ops.len() {
                match self.template.ops[self.op] {
                    Op::Var(ix) => match self.args[ix as usize] {
                        Value::String(str) => {
                            self.op += 1;
                            return Some(str);
                        }
                        Value::Template { template, args } => {
                            self.stack.push((self.template, self.args, self.op + 1));

                            self.template = template;
                            self.args = args;
                            self.op = 0;
                            continue;
                        }
                    },
                    Op::String { addr, len } => {
                        self.op += 1;
                        return Some(
                            &self.template.strings[addr as usize..addr as usize + len as usize],
                        );
                    }
                }
            } else if let Some((template, args, op)) = self.stack.pop() {
                self.template = template;
                self.args = args;
                self.op = op;
                continue;
            } else {
                return None;
            }
        }
    }
}

impl Template {
    pub fn run(&self, write: &mut dyn FnMut(&[u8]), args: &[Arg]) {
        for chunk in self.chunks(args) {
            write(chunk);
        }
    }
}
