pub mod combinators;
pub mod parser;

use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct Module {
    pub source: String,
    pub deps: HashMap<String, PathBuf>,
}

pub fn parse(path: &PathBuf) -> Result<Module, std::io::Error> {
    let re = regex::Regex::new(r#"require\s*\(\s*['"](.+?)['"]\s*\)"#).unwrap();
    let source = read_to_string(&path).expect("Can't open file");
    let deps = re.captures_iter(&source).map(|dep| {
        (dep[1].to_string(), crate::resolve::resolve(dep[1].to_string(), &path.parent().unwrap()).unwrap())
    }).collect::<HashMap::<String, PathBuf>>();

    Ok(Module { source, deps })
}
