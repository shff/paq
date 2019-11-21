extern crate json;
mod utils;

use std::path::Path;
use std::fs::{read_to_string};

use utils::PathExt;

#[derive(Debug, PartialEq)]
pub enum Error {
    NameEmpty,
    ModuleNotFound(String),
    Internal,
}

pub fn resolve(name: String, context: &Path) -> Result<String, Error> {
    if name.is_empty() {
        return Err(Error::NameEmpty);
    }

    let path = Path::new(&name);
    if path.is_explicitly_relative() {
        let new_path = context.parent().unwrap().join_normalizing(path);

        load(&new_path).ok_or(Error::ModuleNotFound(name))
    } else if path.is_absolute() {
        load(path).ok_or(Error::ModuleNotFound(name))
    } else if name == "fs" {
        Err(Error::Internal)
    } else {
        if let Some(parent) = context.parent() {
            let new_path = parent.join("node_modules").join(&path);

            if let Some(result) = load(&new_path) {
                Ok(result)
            } else {
                resolve(name, parent)
            }
        } else {
            Err(Error::ModuleNotFound(name))
        }
    }
}

fn load(path: &Path) -> Option<String> {
    if path.is_file() {
        return Some(path.to_string_lossy().to_string());
    }

    let extensions = vec!["js", "mjs", "json"];
    for extension in extensions {
        let new_path = path.with_extension(extension);
        if new_path.is_file() {
            return Some(new_path.to_string_lossy().to_string());
        }
    }

    let pkg_path = path.join("package.json");
    if let Ok(data) = read_to_string(&pkg_path) {
        if let Ok(pkg_info) = json::parse(&data) {
            if let Some(main) = pkg_info["main"].as_str() {
                if main != "." && main != ".." {
                    return load(&path.join(main))
                }
            }
        }
    }
    if path.is_dir() {
        return load(&path.join("index"));
    }
    None
}

#[test]
fn test_resolve() {
    let fixtures = std::env::current_dir().unwrap().join("fixtures");

    assert_eq!(
        resolve("./counter".to_string(), &fixtures.join("relative/index.js")),
        Ok(fixtures.join("relative/counter").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("./counter".to_string(), &fixtures.join("relative-js/index.js")),
        Ok(fixtures.join("relative-js/counter.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("./counter".to_string(), &fixtures.join("relative-mjs/index.mjs")),
        Ok(fixtures.join("relative-mjs/counter.mjs").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("../counter".to_string(), &fixtures.join("parent-js/entry/index.js")),
        Ok(fixtures.join("parent-js/counter.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("./counter/counter".to_string(), &fixtures.join("relative-nested/index.js")),
        Ok(fixtures.join("relative-nested/counter/counter.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("../counter".to_string(), &fixtures.join("relative-dir/entry/index.js")),
        Ok(fixtures.join("relative-dir/counter/index.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("../counter/counter".to_string(), &fixtures.join("parent-nested/entry/index.js")),
        Ok(fixtures.join("parent-nested/counter/counter.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("./".to_string(), &fixtures.join("pkginfo-basic/index.js")),
        Ok(fixtures.join("pkginfo-basic/counter.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve(".".to_string(), &fixtures.join("pkginfo-basic/index.js")),
        Ok(fixtures.join("pkginfo-basic/counter.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("./counter".to_string(), &fixtures.join("pkginfo-nested/index.js")),
        Ok(fixtures.join("pkginfo-nested/counter/counter.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("../".to_string(), &fixtures.join("pkginfo-parent/entry/index.js")),
        Ok(fixtures.join("pkginfo-parent/counter.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("..".to_string(), &fixtures.join("pkginfo-parent/entry/index.js")),
        Ok(fixtures.join("pkginfo-parent/counter.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve(".".to_string(), &fixtures.join("pkginfo-dot/index.js")),
        Ok(fixtures.join("pkginfo-dot/index.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("..".to_string(), &fixtures.join("pkginfo-dot/entry/index.js")),
        Ok(fixtures.join("pkginfo-dot/index.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("package".to_string(), &fixtures.join("modules-basic/index.js")),
        Ok(fixtures.join("modules-basic/node_modules/package/index.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("package".to_string(), &fixtures.join("modules-file/index.js")),
        Ok(fixtures.join("modules-file/node_modules/package.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("package".to_string(), &fixtures.join("modules-pkginfo/index.js")),
        Ok(fixtures.join("modules-pkginfo/node_modules/package/entry.js").to_string_lossy().to_string())
    );
    assert_eq!(
        resolve("package/lib/counter".to_string(), &fixtures.join("modules-nested/index.js")),
        Ok(fixtures.join("modules-nested/node_modules/package/lib/counter.js").to_string_lossy().to_string())
    );
}
