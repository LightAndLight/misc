use std::io::Read;

const MAX_WIDTH: usize = 120;
const PADDING: usize = 2;

fn main() -> Result<(), std::io::Error> {
    let mut stdin_bytes = Vec::new();
    std::io::stdin().read_to_end(&mut stdin_bytes)?;

    let stdin_str = simdutf8::basic::from_utf8(&stdin_bytes).unwrap();
    let lines = stdin_str.lines().collect::<Vec<&str>>();

    let len = &term_printable_len;
    let line_cb = |line: &str| {
        println!("{}", line);
    };

    let mut widths = Vec::new();
    let mut satisfied = None;
    for num_cols in (2..=lines.len()).rev() {
        if summarise_widths(len, MAX_WIDTH, num_cols, &mut widths, &lines) {
            satisfied = Some(num_cols);
            break;
        }
    }

    match satisfied {
        Some(num_cols) => {
            write_result(len, num_cols, &widths, &lines, line_cb);
        }
        None => {
            lines.into_iter().for_each(line_cb);
        }
    }

    Ok(())
}

fn term_printable_len(input: &str) -> usize {
    let mut in_escape = false;
    let mut count = 0;
    for c in input.chars() {
        match c {
            '\x1B' => {
                in_escape = true;
                continue;
            }
            _ => {
                if in_escape {
                    // in_escape &&
                    if c == 'm' {
                        in_escape = false;
                    }
                } else {
                    // !in_escape &&
                    if !c.is_control() {
                        count += 1;
                    }
                }

                continue;
            }
        }
    }
    count
}

fn summarise_widths(
    len: &dyn Fn(&str) -> usize,
    max_width: usize,
    num_cols: usize,
    widths: &mut Vec<usize>,
    input: &[&str],
) -> bool {
    let max_padding = (num_cols - 1) * PADDING;

    if max_padding > max_width {
        // If the entries were length 0, then the padding alone would cause overflow.
        // Too many columns!
        return false;
    }

    widths.clear();
    widths.extend(std::iter::repeat(0).take(num_cols));
    for chunk in input.chunks(num_cols) {
        for (ix, string) in chunk.iter().enumerate() {
            widths[ix] = widths[ix].max(len(string));
        }
        if widths.iter().sum::<usize>() + max_padding > max_width {
            return false;
        }
    }

    true
}

fn write_result<F: FnMut(&str)>(
    len: &dyn Fn(&str) -> usize,
    num_cols: usize,
    widths: &[usize],
    input: &[&str],
    mut cb: F,
) {
    let mut line = String::new();
    for chunk in input.chunks(num_cols) {
        line.clear();
        let mut widths_and_strings = widths.iter().zip(chunk.iter()).peekable();
        while let Some((width, string)) = widths_and_strings.next() {
            line.push_str(string);
            let padding = if widths_and_strings.peek().is_none() {
                0
            } else {
                PADDING
            };
            for _ in 0..(padding + width - len(string)) {
                line.push(' ');
            }
        }
        cb(&line);
    }
}
