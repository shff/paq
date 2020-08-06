pub mod parser;
pub mod queue;
pub mod resolve;

use std::collections::HashMap;
use std::fmt::Write;
use std::fs::read_to_string;
use std::path::PathBuf;

pub fn bundle(entry: &PathBuf) -> Result<String, queue::Error> {
    let modules = queue::run(entry.clone(), |path| {
        let source = read_to_string(&path)?;

        let mut deps = HashMap::new();
        let ast = parser::block(&source).expect("Parser error");
        for dep in parser::get_deps(ast.1) {
            deps.insert(dep.clone(), resolve::resolve(dep, &path)?);
        }
        let paths = deps.values().cloned().collect();

        Ok((Module { source, deps }, paths))
    })?;

    Ok(write(&modules, entry)?)
}

#[derive(Debug, Clone)]
pub struct Module {
    pub source: String,
    pub deps: HashMap<String, PathBuf>,
}

fn write(modules: &HashMap<PathBuf, Module>, entry: &PathBuf) -> Result<String, std::fmt::Error> {
    let id = |file: &PathBuf| modules.keys().position(|v| v == file).unwrap();

    let mut w = String::new();
    write!(
        w,
        "(function() {{
          var __req = self => dep => {{
            let fn = self.deps[dep];
            if (!fn.module) {{
              fn.module = {{ exports: {{}}, require: __req(fn) }};
              fn(fn.module, fn.module.exports, fn.module.require);
            }}
            return fn.module.exports;
          }};
        "
    )?;
    for (file, module) in modules {
        write!(w, "function a{}(module, exports, require) {{", id(file))?;
        write!(w, "{}", module.source)?;
        write!(w, "}}; a{}.deps = {{}};", id(file))?;
        for (dep, path) in module.deps.clone() {
            write!(w, "a{}.deps['{}'] = a{};", id(file), dep, id(&path))?;
        }
    }
    write!(w, "__req({{ deps: {{ 0: a{} }} }})(0); }})()", id(entry))?;
    Ok(w)
}
