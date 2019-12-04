use std::collections::HashMap;
use std::fmt::{Write, Error};
use std::path::{PathBuf, Path};

#[derive(Debug, Clone)]
pub struct Module {
    pub source: String,
    pub deps: HashMap<String, PathBuf>,
}

pub fn write(modules: &HashMap<PathBuf, Module>, entry_point: &Path) -> Result<String, Error> {
    let mut w = String::new();
    write!(w, "function() {{").unwrap();

    for (file, module) in modules {
        let filename = json::stringify(file.to_string_lossy().to_string());

        write!(w, "bundle.files[{}] = {{ func: function(module, exports, require) {{\n{} \n}} }};", filename, module.source).unwrap();
    }
    write!(w, "\n").unwrap();
    write!(w, "bundle.main = bundle.files['{}'];\n", entry_point.to_string_lossy()).unwrap();
    write!(w, "bundle.make_require(null)()\n").unwrap();
    write!(w, "}}();").unwrap();

    Ok(w)
}
