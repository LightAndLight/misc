/** This program is intended to lay out text in a terminal. It uses conservative
estimates of line lengths. The maximum line length is 2^16 terminal columns, because that
already seems well beyond the number of columns that could fit on a screen.
*/
use std::io::Read;

const MAX_WIDTH: u16 = 120;
const PADDING: u16 = 2;

struct Item<'a> {
    content: &'a str,
    len: u16,
}

fn main() -> Result<(), std::io::Error> {
    let mut stdin_bytes = Vec::new();
    std::io::stdin().read_to_end(&mut stdin_bytes)?;

    let stdin_str = simdutf8::basic::from_utf8(&stdin_bytes).unwrap();
    let len = &term_printable_len;

    let lines = stdin_str
        .lines()
        .map(|line| Item {
            content: line,
            len: len(line),
        })
        .collect::<Vec<Item>>();

    let mut widths = Vec::new();
    let mut satisfied = None;

    // Laying out items into 2^16 columns is already ludicrous, so use that value
    // as an upper bound.
    let max_cols = (u16::MAX as usize).min(lines.len()) as u16;

    for num_cols in (2..=max_cols).rev() {
        if summarise_widths(MAX_WIDTH, num_cols, &mut widths, &lines) {
            satisfied = Some(num_cols);
            break;
        }
    }

    let line_cb = |line: &str| {
        println!("{}", line);
    };
    match satisfied {
        Some(num_cols) => {
            write_result(num_cols, &widths, &lines, line_cb);
        }
        None => {
            lines.into_iter().for_each(|line| line_cb(line.content));
        }
    }

    Ok(())
}

fn term_printable_len(input: &str) -> u16 {
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

fn summarise_widths(max_width: u16, num_cols: u16, widths: &mut Vec<u16>, input: &[Item]) -> bool {
    let max_padding = (num_cols - 1) * PADDING;

    if max_padding > max_width {
        // If the entries were length 0, then the padding alone would cause overflow.
        // Too many columns!
        return false;
    }

    widths.clear();
    widths.extend(std::iter::repeat(0).take(num_cols as usize));
    for chunk in input.chunks(num_cols as usize) {
        for (ix, item) in chunk.iter().enumerate() {
            widths[ix] = widths[ix].max(item.len);
        }
        if widths.iter().sum::<u16>() + max_padding > max_width {
            return false;
        }
    }

    true
}

fn write_result<F: FnMut(&str)>(num_cols: u16, widths: &[u16], input: &[Item], mut cb: F) {
    let mut line = String::new();
    for chunk in input.chunks(num_cols as usize) {
        line.clear();

        let mut widths_and_items = widths.iter().zip(chunk.iter()).peekable();
        while let Some((width, item)) = widths_and_items.next() {
            line.push_str(item.content);
            if widths_and_items.peek().is_some() {
                for _ in 0..(PADDING + width - item.len) {
                    line.push(' ');
                }
            };
        }

        cb(&line);
    }
}
