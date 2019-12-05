use std::collections::HashMap;
use std::fmt::{Write, Error};
use std::path::{PathBuf, Path};

const PRELUDE_JS: &str = include_str!("prelude.js");

#[derive(Debug, Clone)]
pub struct Module {
    pub source: String,
    pub deps: HashMap<String, PathBuf>,
}

pub fn write(modules: &HashMap<PathBuf, Module>, entry_point: &Path) -> Result<String, Error> {
    let files: Vec<PathBuf> = modules.keys().cloned().collect();

    let mut w = String::new();
    write!(w, "{}", PRELUDE_JS).unwrap();
    for (file, module) in modules {
        let filename = files.iter().position(|v| v == file).unwrap();
        let deps = json::stringify(module.deps.iter().map(|(dep, path)|
            (dep.to_string(), files.iter().position(|v| v == path).unwrap())
        ).collect::<HashMap::<String, usize>>());

        write!(w, "__deps[{}] = {{ deps: {}, func: function(module, exports, require) {{\n{} \n}} }};", filename, deps, module.source).unwrap();
    }
    let entry_id = files.iter().position(|v| v == entry_point).unwrap();
    write!(w, "__req(null)({}) }})()", entry_id).unwrap();

    Ok(w)
}
