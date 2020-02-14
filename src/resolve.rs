use std::fs::read_to_string;
use std::path::{Component, Path, PathBuf};

pub fn resolve(name: String, context: &Path, is_browser: bool) -> Option<PathBuf> {
    let path = Path::new(&name);
    if path.starts_with("./") || path.starts_with("../") {
        let new_path = normalize(&context.join(path));

        load(&new_path, is_browser)
    } else if path.is_absolute() {
        load(path, is_browser)
    } else if name.is_empty() {
        load(context, is_browser)
    } else {
        let parent = context.parent()?;
        let new_path = context.join("node_modules").join(&path);

        load(&new_path, is_browser).or(resolve(name, parent, is_browser))
    }
}

fn load(path: &Path, is_browser: bool) -> Option<PathBuf> {
    if path.is_file() {
        return Some(path.to_path_buf());
    }

    let extensions = vec!["js", "mjs", "json"];
    for extension in extensions {
        let new_path = path.with_extension(extension);
        if new_path.is_file() {
            return Some(new_path);
        }
    }

    let pkg_path = path.join("package.json");
    if let Ok(data) = read_to_string(&pkg_path) {
        if let Ok(pkg_info) = json::parse(&data) {
            if let Some(browser) = pkg_info["browser"].as_str() {
                if is_browser && browser != "." && browser != ".." {
                    return load(&path.join(browser), is_browser);
                }
            }
            if let Some(main) = pkg_info["main"].as_str() {
                if main != "." && main != ".." {
                    return load(&path.join(main), is_browser);
                }
            }
        }
    }
    if path.is_dir() {
        return load(&path.join("index"), is_browser);
    }
    None
}

pub fn normalize(p: &Path) -> PathBuf {
    p.components().fold(PathBuf::from("/"), |path, c| match c {
        Component::Prefix(ref prefix) => PathBuf::from(prefix.as_os_str().to_owned()),
        Component::RootDir => path.join("/"),
        Component::CurDir => path,
        Component::ParentDir => path.parent().unwrap().to_owned(),
        Component::Normal(part) => path.join(part),
    })
}
