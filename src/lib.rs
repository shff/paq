use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};

mod writer;
mod queue;
mod resolve;

pub fn bundle(file: String, root: &Path) -> Result<String, Box<dyn std::error::Error>> {
    let entry = resolve::resolve(file, &root).ok_or("No entry point")?;
    let modules = queue::run(entry.clone(), |path| {
        let source = read_to_string(&path)?;

        let regexp = regex::Regex::new(r#"require\s*\(\s*['"](.+?)['"]\s*\)"#)?;
        let deps = regexp.captures_iter(&source).map(|dep| {
            (dep[1].to_string(), resolve::resolve(dep[1].to_string(), &path.parent().unwrap()).unwrap())
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
        let result = bundle("./index.js".to_string(), &fixtures.join(path)).expect("Error");
        assert!(result.contains(substring))
    }
    fn assert_node(path: &str, value: &str) {
        let fixtures = std::env::current_dir().unwrap().join("fixtures");
        let result = bundle("./index.js".to_string(), &fixtures.join(path)).expect("Error");
        let output = std::process::Command::new("node").arg("-e").arg(&result).output().expect("Error running node");
        assert_eq!(String::from_utf8_lossy(&output.stdout), value);
    }
    assert_bundle("basic", "console.log('hello')");
    assert_bundle("with-dep", "/* math.js */");
    assert_bundle("double-quotes", "/* math.js */");
    assert_bundle("crazy-indent", "/* math.js */");
    assert_node("basic", "hello\n");
    assert_node("with-dep", "2\n");
}
