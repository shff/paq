pub mod linker;
pub mod parser;
pub mod queue;
pub mod resolve;

use std::fs::read_to_string;
use std::path::{Path, PathBuf};

pub fn bundle(entry: &Path) -> Result<String, queue::Error> {
    let module_infos = queue::run(entry.to_path_buf(), |path| {
        let source = read_to_string(&path)?;
        let info = linker::analyze(source, &path, |dep| resolve::resolve(dep.to_string(), &path))?;
        let dep_paths: Vec<PathBuf> = info
            .imports
            .values()
            .map(|b| b.source_path.clone())
            .collect::<std::collections::HashSet<_>>()
            .into_iter()
            .collect();
        Ok((info, dep_paths))
    })?;

    let live = linker::collect_live_stmts(entry, &module_infos);
    let mut module_infos = module_infos;
    linker::assign_canonical_names(&live, &mut module_infos);
    Ok(linker::render(&live, &module_infos))
}
