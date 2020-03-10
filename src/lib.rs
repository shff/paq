extern crate regex;

pub mod js;
pub mod queue;
pub mod resolve;

use std::collections::HashMap;
use std::path::PathBuf;

pub fn bundle(entry: &PathBuf) -> Result<String, queue::Error> {
    let modules = queue::run(entry.clone(), |path| {
        let module = js::parse(&path).unwrap();
        let modules = module.deps.values().cloned().collect();

        Ok((module, modules))
    })?;
    Ok(write(&modules, &entry))
}

fn write(modules: &HashMap<PathBuf, js::Module>, entry: &PathBuf) -> String {
    let get_id = |file: &PathBuf| modules.keys().position(|v| v == file).unwrap();
    let prelude = "
(function() {
  var __req = self => dep => {
    let fn = self.deps[dep];
    if (!fn.module) {
      fn.module = { exports: {}, require: __req(fn) };
      fn(fn.module, fn.module.exports, fn.module.require);
    }
    return fn.module.exports;
  };
;";
    let mods = modules
        .iter()
        .map(|(file, module)| {
            let deps = module
                .deps
                .iter()
                .map(|(dep, path)| format!("\"{}\": a{}", dep, get_id(path)))
                .collect::<Vec<String>>()
                .join(",");
            format!(
                "function a{}(module, exports, require) {{\n{} \n}};\na{}.deps = {{{}}};\n",
                get_id(file),
                module.source,
                get_id(file),
                deps
            )
        })
        .collect::<Vec<String>>()
        .join("\n");
    format!(
        "{}; {}; __req({{ deps: {{ entry: a{} }} }})('entry'); }})()",
        prelude,
        mods,
        get_id(entry)
    )
}
