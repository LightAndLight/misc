use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn main() {
    let file = File::open("measurements.txt").unwrap();
    let mut reader = BufReader::new(file);
    let mut count = 0;
    let mut line = String::new();
    loop {
        line.clear();
        let n = reader.read_line(&mut line).unwrap();
        count += 1;

        if n == 0 {
            break;
        } else {
            continue;
        }
    }
    println!("{:?}", count);
}
