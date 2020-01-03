use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
struct Module {
    source: String,
    deps: HashMap<String, PathBuf>,
}

pub fn bundle(file: String, root: &Path) -> Result<String, Box<dyn std::error::Error>> {
    let entry = js_resolve::resolve(file, &root).ok_or("No entry point")?;
    let regexp = regex::Regex::new(r#"require\s*\(\s*['"](.+?)['"]\s*\)"#)?;
    let modules = miniqueue::run(entry.clone(), |path| {
        let source = read_to_string(&path)?;

        let deps = regexp.captures_iter(&source).map(|dep| {
            (dep[1].to_string(), js_resolve::resolve(dep[1].to_string(), &path.parent().unwrap()).unwrap())
        }).collect::<HashMap::<String, PathBuf>>();
        let modules = deps.values().cloned().collect();

        Ok((Module { source, deps }, modules))
    })?;
    let content = write(&modules, &entry);

    Ok(content)
}

fn write(modules: &HashMap<PathBuf, Module>, entry_point: &Path) -> String {
    let mods = modules.iter().map(|(file, module)| {
        let filename = modules.keys().position(|v| v == file).unwrap();
        let deps = json::stringify(module.deps.iter().map(|(dep, path)|
            (dep.to_string(), modules.keys().position(|v| v == path).unwrap())
        ).collect::<HashMap::<String, usize>>());

        format!("__deps[{}] = {{ deps: {}, func: function(module, exports, require) {{\n{} \n}} }};", filename, deps, module.source)
    }).collect::<Vec<String>>().join("\n");

    let prelude = include_str!("prelude.js");
    let entry_id = modules.keys().position(|v| v == entry_point).unwrap();
    format!("{}; {}; __req(null)({}) }})()", prelude, mods, entry_id)
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
        assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), value);
    }
    assert_bundle("basic", "console.log('hello')");
    assert_bundle("with-dep", "/* math.js */");
    assert_bundle("double-quotes", "/* math.js */");
    assert_bundle("crazy-indent", "/* math.js */");
    assert_bundle("with-modules", "PETER RULES");
    assert_bundle("with-modules-2", "PartitionIter");
    assert_node("basic", "hello");
    assert_node("with-dep", "2");
    assert_node("double-quotes", "");
    assert_node("crazy-indent", "");
    assert_node("with-modules", "Once upon a day there was a person, named Peter DeMartini\nHe is awesome!\nHave a nice day...\n \n \n \nPETER RULES!!!");
    assert_node("with-modules-2", "0 1 2 3 4");
}
