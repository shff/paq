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
    let mut map = HashMap::<PathBuf, usize>::new();
    let entry_id = get_unique_id(&mut map, entry_point.to_path_buf());

    let mut w = String::new();
    write!(w, "{}", PRELUDE_JS).unwrap();
    for (file, module) in modules {
        let filename = get_unique_id(&mut map, file.to_path_buf());
        let deps = json::stringify(module.deps.iter().map(|(dep, path)|
            (dep.to_string(), get_unique_id(&mut map, path.to_path_buf()))
        ).collect::<HashMap::<String, usize>>());

        write!(w, "__deps[{}] = {{ deps: {}, func: function(module, exports, require) {{\n{} \n}} }};", filename, deps, module.source).unwrap();
    }
    write!(w, "__req(null)({}) }})()", entry_id).unwrap();

    Ok(w)
}

fn get_unique_id(map: &mut HashMap<PathBuf, usize>, item: PathBuf) -> usize {
    let len = map.len();
    *map.entry(item).or_insert(len)
}
