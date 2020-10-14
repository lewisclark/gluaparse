use clap::{App, Arg};
use std::fs::File;
use std::io::Read;

fn main() {
    let matches = App::new("gluaparse")
        .version(env!("CARGO_PKG_VERSION"))
        .author("Lewis Clark @ lewis.to")
        .arg(
            Arg::with_name("file")
                .short("f")
                .long("file")
                .value_name("file.lua")
                .help("Path to the Lua file to parse")
                .takes_value(true)
                .required(true),
        )
        .get_matches();

    let file_name = matches.value_of("file").expect("Expected file arg");
    let mut file = File::open(file_name).expect("Failed to open file specified by file arg");

    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Failed to read file contents");

    let ast = gluaparse::parse(&contents).expect("Failed to parse Lua");
    println!("{}", ast);
}
