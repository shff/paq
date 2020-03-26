#![allow(clippy::cognitive_complexity)]

pub mod lexer;
pub mod queue;
pub mod resolve;

use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::PathBuf;

pub fn bundle(entry: PathBuf) -> Result<String, queue::Error> {
    let modules = queue::run(entry, |path| {
        let source = read_to_string(&path)?;

        let mut deps = HashMap::new();
        for dep in lexer::get_deps(&source) {
            deps.insert(dep.clone(), resolve::resolve(dep, &path)?);
        }
        let paths = deps.values().cloned().collect();

        Ok((Module { source, deps }, paths))
    })?;

    let all = modules
        .iter()
        .map(|(_, m)| m.source.clone())
        .collect::<Vec<_>>()
        .join("");

    Ok(all)
}

#[derive(Debug, Clone)]
pub struct Module {
    pub source: String,
    pub deps: HashMap<String, PathBuf>,
}
