use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
struct Module {
    source: String,
    deps: HashMap<String, PathBuf>,
}

pub fn bundle(entry: &PathBuf) -> Result<String, Box<dyn std::error::Error>> {
    let regexp = regex::Regex::new(r#"require\s*\(\s*['"](.+?)['"]\s*\)"#)?;
    let modules = miniqueue::run(entry.clone(), |path| {
        let source = read_to_string(&path)?;
        let deps = regexp.captures_iter(&source).map(|dep| {
            (dep[1].to_string(), js_resolve::resolve(dep[1].to_string(), &path.parent().unwrap()).unwrap())
        }).collect::<HashMap::<String, PathBuf>>();
        let modules = deps.values().cloned().collect();

        Ok((Module { source, deps }, modules))
    })?;
    Ok(write(&modules, &entry))
}

fn write(modules: &HashMap<PathBuf, Module>, entry: &Path) -> String {
    let get_id = |file: &Path| modules.keys().position(|v| v == file).unwrap();
    let prelude = include_str!("prelude.js");
    let mods = modules.iter().map(|(file, module)| {
        let deps = module.deps.iter().map(|(dep, path)|
            format!("\"{}\": a{}", dep, get_id(path))
        ).collect::<Vec<String>>().join(",");
        format!("function a{}(module, exports, require) {{\n{} \n}};\na{}.deps = {{{}}};\n", get_id(file), module.source, get_id(file), deps)
    }).collect::<Vec<String>>().join("\n");
    format!("{}; {}; __req({{ deps: {{ entry: a{} }} }})('entry'); }})()", prelude, mods, get_id(entry))
}

#[test]
fn test_bundler() {
    fn assert_bundle(path: &str, substring: &str) {
        let fixtures = std::env::current_dir().unwrap().join("fixtures");
        let result = bundle(&fixtures.join(path).join("index.js")).expect("Error");
        assert!(result.contains(substring))
    }
    fn assert_node(path: &str, value: &str) {
        let fixtures = std::env::current_dir().unwrap().join("fixtures");
        let result = bundle(&fixtures.join(path).join("index.js")).expect("Error");
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
