pub mod queue;
pub mod resolve;
pub mod combinators;

use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
struct Module {
    source: String,
    deps: HashMap<String, PathBuf>,
}

pub fn bundle(entry: &PathBuf) -> Result<String, Box<dyn std::error::Error + Send>> {
    let modules = queue::run(entry.clone(), |path| {
        let re = regex::Regex::new(r#"require\s*\(\s*['"](.+?)['"]\s*\)"#).unwrap();
        let source = read_to_string(&path).expect("Can't open file");
        let deps = re.captures_iter(&source).map(|dep| {
            (dep[1].to_string(), resolve::resolve(dep[1].to_string(), &path.parent().unwrap(), true).unwrap())
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
