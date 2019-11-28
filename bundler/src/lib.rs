extern crate js_resolve;
extern crate json;
mod queue;
mod writer;

use std::path::{Path};
use std::error::Error;
use std::fs::{read_to_string};

fn bundle(file: String, root: &Path) -> Result<String, Box<dyn Error>> {
    let entry = js_resolve::resolve_entry(file, &root).ok_or("No entry point")?;
    let modules = queue::run(entry.clone(), |path| {
        let source = read_to_string(&path)?;

        Ok((source, vec![]))
    })?;
    let content = writer::write(&modules, &entry)?;

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
