extern crate x20250503_template_language;

use std::io::Write;

use x20250503_template_language::compile::*;

fn main() {
    let template = read_compile("examples/simple_html.html.tpl");

    let write: &mut dyn FnMut(&[u8]) = &mut |chunk| {
        std::io::stdout().write_all(chunk).unwrap();
    };

    run(
        write,
        &template,
        &[Value::from("Simple HTML"), Value::from("Hello, world!")],
    );
}
