use std::path::PathBuf;

extern crate js_bundler;

fn main() {
	let entry = std::env::args().nth(1).expect("Missing argument: entry point");
  let entry = PathBuf::from(entry).canonicalize().expect("Can't find entry point");
	let result = js_bundler::bundle(&entry).expect("Error during bundling");
	println!("{}", result);
}
