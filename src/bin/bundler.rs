use std::path::PathBuf;

fn main() {
    let mut entry = PathBuf::from("index.js");

    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match &*arg {
            "-e" | "--entry" => entry = args.next().map(PathBuf::from).unwrap(),
            "-v" | "--version" => {
                println!("{} v{}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
                return;
            }
            e => entry = PathBuf::from(e),
        }
    }

    match rakastu::bundle(&entry) {
        Ok(data) => println!("{}", data),
        Err(err) => eprintln!("{:?}", err),
    };
}
