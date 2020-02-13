use std::path::PathBuf;

extern crate paq;

fn main() {
	let entry = std::env::args().nth(1).expect("Missing argument: entry point");
  let entry = PathBuf::from(entry).canonicalize().expect("Can't find entry point");
	let result = paq::bundle(&entry).expect("Error during bundling");
	println!("{}", result);
}
