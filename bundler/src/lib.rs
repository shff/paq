use std::collections::{HashMap};
use std::path::{Path, PathBuf};
use std::fs::{read_to_string};

mod writer;

fn bundle(file: String, root: &Path) -> Result<String, Box<dyn std::error::Error>> {
    let entry = js_resolve::resolve_entry(file, &root).ok_or("No entry point")?;
    let modules = miniqueue::run(entry.clone(), |path| {
        let source = read_to_string(&path)?;

        let regexp = regex::Regex::new(r#"require\s*\(\s*['"](.*)['"]\s*\)"#)?;
        let deps = regexp.captures_iter(&source).map(|dep| {
            (dep[1].to_string(), js_resolve::resolve(dep[1].to_string(), &path).unwrap())
        }).collect::<HashMap::<String, PathBuf>>();
        let modules = deps.values().cloned().collect();

        Ok((writer::Module { source, deps }, modules))
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
    assert_bundle("with-dep", "/* math.js */");
    assert_bundle("double-quotes", "/* math.js */");
    assert_bundle("crazy-indent", "/* math.js */");
}
