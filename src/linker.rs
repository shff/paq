use crate::parser::{self, Node};
use std::cmp::Reverse;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

#[derive(Clone)]
pub struct ImportBinding {
    pub exported_name: String,
    pub local_name: String,
    pub source_path: PathBuf,
}

#[derive(Clone)]
pub struct ExportEntry {
    pub exported_name: String,
    pub local_name: String,
    pub stmt_index: usize,
}

#[derive(Clone)]
pub struct Stmt {
    pub start: usize,
    pub end: usize,
    pub defines: Vec<String>,
    pub is_import: bool,
    pub is_export: bool,
}

#[derive(Clone)]
pub struct ModuleInfo {
    pub source: String,
    pub stmts: Vec<Stmt>,
    pub imports: HashMap<String, ImportBinding>,
    pub exports: HashMap<String, ExportEntry>,
    pub definitions: HashMap<String, usize>,
    pub canonical_names: HashMap<String, String>,
}

pub struct Edit {
    pub start: usize,
    pub end: usize,
    pub replacement: String,
}

pub fn analyze<E>(
    source: String,
    path: &Path,
    resolve: impl Fn(&str) -> Result<PathBuf, E>,
) -> Result<ModuleInfo, E> {
    let mut info = ModuleInfo {
        source: source.clone(),
        stmts: Vec::new(),
        imports: HashMap::new(),
        exports: HashMap::new(),
        definitions: HashMap::new(),
        canonical_names: HashMap::new(),
    };

    let canon_default = path
        .file_stem()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_default()
        .chars()
        .filter(|c| c.is_alphanumeric() || *c == '_')
        .collect::<String>();
    info.canonical_names
        .insert("default".to_string(), canon_default);

    let spans = parser::stmts_with_spans(&info.source);

    for (node, start, end) in spans {
        let stmt_idx = info.stmts.len();
        let mut stmt = Stmt {
            start,
            end,
            defines: Vec::new(),
            is_import: false,
            is_export: false,
        };

        match &node {
            Node::Import((binding, path_node)) => {
                stmt.is_import = true;
                let dep_path_str = match path_node.as_ref() {
                    Node::Str(s) => s.clone(),
                    _ => String::new(),
                };
                if !dep_path_str.is_empty() {
                    let resolved = resolve(&dep_path_str)?;
                    match binding {
                        Some(b) => {
                            // default import: `import Foo from './foo'`
                            if let Node::Ident(local) = b.as_ref() {
                                info.imports.insert(
                                    local.to_string(),
                                    ImportBinding {
                                        exported_name: "default".to_string(),
                                        local_name: local.to_string(),
                                        source_path: resolved,
                                    },
                                );
                            }
                        }
                        None => {
                            // bare import or named specifiers — re-parse for names
                            let slice = &info.source[start..end];
                            let names = parser::parse_import_names(slice);
                            if names.is_empty() {
                                // side-effect import only
                            } else {
                                for (exported, local) in names {
                                    info.imports.insert(
                                        local.clone(),
                                        ImportBinding {
                                            exported_name: exported,
                                            local_name: local,
                                            source_path: resolved.clone(),
                                        },
                                    );
                                }
                            }
                        }
                    }
                }
            }

            Node::Export(inner) => {
                stmt.is_export = true;
                match inner.as_ref() {
                    // export function foo() {}
                    Node::Function((Some(name), _, _)) => {
                        let name = name.to_string();
                        stmt.defines.push(name.clone());
                        info.definitions.insert(name.clone(), stmt_idx);
                        info.exports.insert(
                            name.clone(),
                            ExportEntry {
                                exported_name: name.clone(),
                                local_name: name,
                                stmt_index: stmt_idx,
                            },
                        );
                    }
                    // export class Foo {}
                    Node::Class((Some(name), _, _)) => {
                        let name = name.to_string();
                        stmt.defines.push(name.clone());
                        info.definitions.insert(name.clone(), stmt_idx);
                        info.exports.insert(
                            name.clone(),
                            ExportEntry {
                                exported_name: name.clone(),
                                local_name: name,
                                stmt_index: stmt_idx,
                            },
                        );
                    }
                    // export const/let/var foo = ...
                    Node::Declaration((_, declarators)) => {
                        for decl in declarators {
                            if let Node::Variable((_, binding)) = decl {
                                let name = extract_binding_name(binding);
                                if let Some(name) = name {
                                    stmt.defines.push(name.clone());
                                    info.definitions.insert(name.clone(), stmt_idx);
                                    info.exports.insert(
                                        name.clone(),
                                        ExportEntry {
                                            exported_name: name.clone(),
                                            local_name: name,
                                            stmt_index: stmt_idx,
                                        },
                                    );
                                }
                            }
                        }
                    }
                    // export default <expr>
                    Node::Default(_) => {
                        info.exports.insert(
                            "default".to_string(),
                            ExportEntry {
                                exported_name: "default".to_string(),
                                local_name: "default".to_string(),
                                stmt_index: stmt_idx,
                            },
                        );
                    }
                    // export { foo, bar as baz } — specifiers list parsed as Object([])
                    // We re-parse the source slice to get the actual names
                    Node::Object(_) => {
                        let slice = &info.source[start..end];
                        let names = parser::parse_import_names(slice);
                        for (local, exported) in names {
                            // in export { local as exported }, parse_import_names returns (local, exported)
                            if let Some(def_idx) = info.definitions.get(&local).copied() {
                                info.exports.insert(
                                    exported.clone(),
                                    ExportEntry {
                                        exported_name: exported,
                                        local_name: local,
                                        stmt_index: def_idx,
                                    },
                                );
                            }
                        }
                    }
                    _ => {}
                }
            }

            // Non-import, non-export top-level statement
            _ => {
                match &node {
                    Node::Declaration((_, declarators)) => {
                        for decl in declarators {
                            if let Node::Variable((_, binding)) = decl {
                                if let Some(name) = extract_binding_name(binding) {
                                    stmt.defines.push(name.clone());
                                    info.definitions.insert(name, stmt_idx);
                                }
                            }
                        }
                    }
                    Node::Function((Some(name), _, _)) => {
                        stmt.defines.push(name.to_string());
                        info.definitions.insert(name.to_string(), stmt_idx);
                    }
                    Node::Class((Some(name), _, _)) => {
                        stmt.defines.push(name.to_string());
                        info.definitions.insert(name.to_string(), stmt_idx);
                    }
                    _ => {}
                }
            }
        }

        info.stmts.push(stmt);
    }

    Ok(info)
}

fn extract_binding_name<'a>(node: &Node<'a>) -> Option<String> {
    match node {
        Node::Ident(s) => Some(s.to_string()),
        _ => None,
    }
}

pub fn collect_live_stmts(
    entry: &Path,
    modules: &HashMap<PathBuf, ModuleInfo>,
) -> Vec<(PathBuf, usize)> {
    let mut result: Vec<(PathBuf, usize)> = Vec::new();
    let mut visited: HashSet<(PathBuf, usize)> = HashSet::new();
    // Collect all non-export statements from entry, plus imported exports
    collect_module(entry, None, modules, &mut result, &mut visited);
    result
}

/// Collect live statements from a module.
/// If `requested_export` is Some(name), only pull in that specific export's defining statement
/// (plus any transitive deps it needs). If None, collect all non-export statements plus
/// recurse into imports.
fn collect_module(
    path: &Path,
    requested_export: Option<&str>,
    modules: &HashMap<PathBuf, ModuleInfo>,
    result: &mut Vec<(PathBuf, usize)>,
    visited: &mut HashSet<(PathBuf, usize)>,
) {
    let module = match modules.get(path) {
        Some(m) => m,
        None => return,
    };

    if let Some(export_name) = requested_export {
        // Only pull in the stmt that defines this export
        if let Some(entry) = module.exports.get(export_name) {
            let stmt_idx = entry.stmt_index;
            let key = (path.to_path_buf(), stmt_idx);
            if !visited.contains(&key) {
                visited.insert(key.clone());
                // Recurse into any imports this stmt depends on (conservative: pull all imports)
                for binding in module.imports.values() {
                    collect_module(&binding.source_path, Some(&binding.exported_name.clone()), modules, result, visited);
                }
                result.push(key);
            }
        }
    } else {
        // Collect the whole module: process each statement in order
        for (stmt_idx, stmt) in module.stmts.iter().enumerate() {
            if stmt.is_import {
                // For each named import, pull in the specific export from the dep
                let bindings: Vec<ImportBinding> = module.imports.values().cloned().collect();
                let seen_deps: HashSet<PathBuf> = HashSet::new();
                let _ = seen_deps; // suppress warning
                // Collect unique dep paths from imports associated with this stmt
                // (We associate all imports since we don't track which import stmt owns which binding)
                for binding in &bindings {
                    collect_module(
                        &binding.source_path,
                        Some(&binding.exported_name),
                        modules,
                        result,
                        visited,
                    );
                }
            } else if stmt.is_export {
                // Export stmts are only included when a caller requests them specifically
                // (don't include unreferenced exports)
            } else {
                let key = (path.to_path_buf(), stmt_idx);
                if !visited.contains(&key) {
                    visited.insert(key.clone());
                    result.push(key);
                }
            }
        }
    }
}

pub fn assign_canonical_names(
    live: &[(PathBuf, usize)],
    modules: &mut HashMap<PathBuf, ModuleInfo>,
) {
    let mut seen: HashMap<String, u32> = HashMap::new();

    // Collect (path, name) pairs first to avoid borrow conflicts
    let pairs: Vec<(PathBuf, Vec<String>)> = live
        .iter()
        .map(|(path, stmt_idx)| {
            let defines = modules
                .get(path)
                .map(|m| m.stmts[*stmt_idx].defines.clone())
                .unwrap_or_default();
            (path.clone(), defines)
        })
        .collect();

    for (path, defines) in pairs {
        for name in defines {
            let counter = seen.entry(name.clone()).or_insert(0);
            if *counter > 0 {
                let canonical = format!("{}{}", name, counter);
                if let Some(m) = modules.get_mut(&path) {
                    m.canonical_names.insert(name.clone(), canonical);
                }
            }
            *counter += 1;
        }
    }
}

pub fn render(live: &[(PathBuf, usize)], modules: &HashMap<PathBuf, ModuleInfo>) -> String {
    let mut output = String::new();

    for (path, stmt_idx) in live {
        let module = match modules.get(path) {
            Some(m) => m,
            None => continue,
        };
        let stmt = &module.stmts[*stmt_idx];
        let source_slice = &module.source[stmt.start..stmt.end];
        let base_ptr = module.source[stmt.start..].as_ptr() as usize;

        let mut edits: Vec<Edit> = Vec::new();

        // Strip export/import keyword prefix
        let trimmed = source_slice.trim_start();
        let prefix_offset = source_slice.len() - trimmed.len();
        if trimmed.starts_with("export default ") {
            edits.push(Edit {
                start: prefix_offset,
                end: prefix_offset + "export default ".len(),
                replacement: format!(
                    "var {} = ",
                    module
                        .canonical_names
                        .get("default")
                        .cloned()
                        .unwrap_or_else(|| "default".to_string())
                ),
            });
        } else if trimmed.starts_with("export ") {
            edits.push(Edit {
                start: prefix_offset,
                end: prefix_offset + "export ".len(),
                replacement: String::new(),
            });
        }

        // Collect identifier positions and build rename edits
        if let Ok((_, node)) = parser::block(source_slice) {
            let mut ident_positions = Vec::new();
            parser::collect_ident_positions(&node, base_ptr, &mut ident_positions);

            for (name, start, end) in ident_positions {
                // Check canonical rename for this module
                if let Some(renamed) = module.canonical_names.get(name) {
                    if renamed != name {
                        edits.push(Edit {
                            start,
                            end,
                            replacement: renamed.clone(),
                        });
                    }
                }
                // Check if it's an import reference → resolve to the canonical name in the dep
                else if let Some(binding) = module.imports.get(name) {
                    if let Some(dep) = modules.get(&binding.source_path) {
                        let local = &binding.exported_name;
                        let canonical = dep
                            .canonical_names
                            .get(local)
                            .cloned()
                            .unwrap_or_else(|| local.clone());
                        if canonical != name {
                            edits.push(Edit {
                                start,
                                end,
                                replacement: canonical,
                            });
                        }
                    }
                }
            }
        }

        // Sort edits in reverse order and apply
        edits.sort_by_key(|e| Reverse(e.start));
        let mut result = source_slice.to_string();
        for edit in edits {
            if edit.start <= edit.end && edit.end <= result.len() {
                result.replace_range(edit.start..edit.end, &edit.replacement);
            }
        }

        let trimmed_result = result.trim();
        if !trimmed_result.is_empty() {
            output.push_str(trimmed_result);
            output.push('\n');
        }
    }

    output
}
