extern crate js_resolve;
mod queue;

use std::path::{Path};
use std::fs::{read_to_string};

fn bundle(file: String, root: &Path) -> Result<String, String> {
    let entry = js_resolve::resolve_entry(file, &root).or(Err("Can't resolve entry point"))?;
    let modules = queue::run(entry.clone(), |path| {
        let source = read_to_string(&path).expect("Can't read file");

        Ok((source, vec![]))
    }).expect("Can't get dependencies");
    let content = modules.iter().map(|(_,b)| b.to_string()).collect::<Vec<String>>().join("");

    Ok(content)
}

#[test]
fn test_bundler() {
  fn assert_bundle(path: &str, substring: &str) {
    let fixtures = std::env::current_dir().unwrap().join("fixtures");
    let result = bundle("index.js".to_string(), &fixtures.join(path)).expect("Error");
    assert!(result.contains(substring))
  }
  assert_bundle("basic", "hello()");
}
