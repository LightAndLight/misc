use std::io::{BufRead, BufReader};

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
                if !c.is_control() && !in_escape {
                    count += 1;
                }

                if c == 'm' && in_escape {
                    in_escape = false;
                }

                continue;
            }
        }
    }
    count
}

const MAX_WIDTH: usize = 120;

fn main() -> Result<(), std::io::Error> {
    let lines = BufReader::new(std::io::stdin())
        .lines()
        .collect::<Result<Vec<String>, std::io::Error>>()?;

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
            lines.into_iter().for_each(|line| line_cb(&line));
        }
    }

    Ok(())
}

const PADDING: usize = 2;

fn summarise_widths(
    len: &dyn Fn(&str) -> usize,
    max_width: usize,
    num_cols: usize,
    widths: &mut Vec<usize>,
    input: &[String],
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
    input: &[String],
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
