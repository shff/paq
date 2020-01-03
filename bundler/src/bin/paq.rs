extern crate js_bundler;

fn main() {
	let entry = std::env::args().nth(1).expect("Missing argument: entry point");
	let root = std::env::current_dir().expect("Can't access current directory");
	let result = js_bundler::bundle(entry, &root).expect("Error during bundling");
	println!("{}", result);
}
