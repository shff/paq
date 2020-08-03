use std::fs::read_to_string;
use std::io::{Error, ErrorKind};
use std::path::{Component, Path, PathBuf};

pub fn resolve(name: String, context: &Path) -> Result<PathBuf, Error> {
    let parent = context.parent().unwrap();
    let path = Path::new(&name);
    if path.starts_with("./") || path.starts_with("../") {
        let new_path = normalize(&parent.join(path));

        load(&new_path)
    } else if path.is_absolute() {
        load(path)
    } else if name.is_empty() {
        load(parent)
    } else {
        let new_path = parent.join("node_modules").join(&path);

        load(&new_path).or_else(|_| resolve(name, parent))
    }
}

fn load(path: &Path) -> Result<PathBuf, Error> {
    if path.is_file() {
        return Ok(path.to_path_buf());
    }

    let extensions = vec!["js", "mjs", "json"];
    for extension in extensions {
        let new_path = path.with_extension(extension);
        if new_path.is_file() {
            return Ok(new_path);
        }
    }

    let pkg_path = path.join("package.json");
    if let Ok(data) = read_to_string(&pkg_path) {
        if let Ok(pkg_info) = json::parse(&data) {
            if let Some(main) = pkg_info["main"].as_str() {
                if main != "." && main != ".." {
                    return load(&path.join(main));
                }
            }
        }
    }
    if path.is_dir() {
        return load(&path.join("index"));
    }
    Err(Error::new(
        ErrorKind::NotFound,
        format!("Can't find {}", path.display()),
    ))
}

pub fn normalize(p: &Path) -> PathBuf {
    p.components().fold(PathBuf::from(""), |path, c| match c {
        Component::Prefix(ref prefix) => PathBuf::from(prefix.as_os_str().to_owned()),
        Component::RootDir => path.join("/"),
        Component::CurDir => unreachable!(),
        Component::Normal(part) => path.join(part),
        Component::ParentDir => match path.parent() {
            Some(path) => path.to_owned(),
            None => path,
        },
    })
}
