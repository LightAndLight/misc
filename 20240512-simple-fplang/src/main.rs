use std::{
    io::{Cursor, Write},
    path::{Path, PathBuf},
};

use simple_fplang::{
    check::check_module,
    compile::{compile_module, compile_runtime, writers::CModuleWriter},
    internal_error,
    parse::{parse_module, Parser},
};

macro_rules! user_error {
    () => {
        |err| {
            eprintln!("error: {:?}", err);
            std::process::exit(1)
        }
    };
}

fn main() {
    let cwd: PathBuf = std::env::current_dir().unwrap_or_else(internal_error!());
    let file_names: Vec<String> = std::env::args().collect();
    let src = read_src(&cwd, &file_names[1]);
    let mut parser = Parser::new(&src);
    let module = parse_module(&mut parser).unwrap_or_else(user_error!());
    check_module(&module).unwrap_or_else(user_error!());
    let compiled_module = {
        let mut buffer = Vec::new();
        {
            let mut buffer = Cursor::new(&mut buffer);
            let mut module_writer = CModuleWriter::new(&mut buffer);
            compile_runtime(&mut module_writer);
            compile_module(&mut module_writer, &module);
        }
        buffer
    };
    std::io::stdout()
        .write_all(&compiled_module)
        .unwrap_or_else(internal_error!());
}

fn read_src(cwd: &Path, file_name: &str) -> String {
    let file_path: PathBuf = cwd.join(file_name);
    std::fs::read_to_string(file_path).unwrap_or_else(internal_error!())
}
