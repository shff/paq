use std::fs::read_to_string;
use std::path::{Component, Path, PathBuf};

pub fn resolve(name: String, context: &Path) -> Option<PathBuf> {
    let path = Path::new(&name);
    if path.starts_with("./") || path.starts_with("../") {
        let new_path = normalize(&context.join(path));

        load(&new_path)
    } else if path.is_absolute() {
        load(path)
    } else if name.is_empty() {
        load(context)
    } else {
        let parent = context.parent()?;
        let new_path = context.join("node_modules").join(&path);

        load(&new_path).or(resolve(name, parent))
    }
}

fn load(path: &Path) -> Option<PathBuf> {
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
    None
}

fn normalize(p: &Path) -> PathBuf {
    p.components().fold(PathBuf::from("/"), |path, c| match c {
        Component::Prefix(ref prefix) => PathBuf::from(prefix.as_os_str().to_owned()),
        Component::RootDir => path.join("/"),
        Component::CurDir => path,
        Component::ParentDir => path.parent().unwrap().to_owned(),
        Component::Normal(part) => path.join(part),
    })
}

#[test]
fn test_resolve() {
    fn assert_resolves(name: &str, path: &str, expected: &str) {
        let fixtures = std::env::current_dir().unwrap().join("fixtures");
        assert_eq!(resolve(name.to_string(), &fixtures.join(path)), Some(
            normalize(&fixtures.join(path).join(expected.to_string()))
        ));
    }

    assert_resolves("", "no-entry", "index.js");
    assert_resolves("./counter", "relative-file", "counter");
    assert_resolves("./counter", "relative-file-js", "counter.js");
    assert_resolves("./counter", "relative-file-mjs", "counter.mjs");
    assert_resolves("./counter/counter", "relative-file-nested", "counter/counter.js");
    assert_resolves("./😅", "relative-file-unicode", "😅.js");
    assert_resolves("./😅", "relative-dir-unicode", "😅/index.js");
    assert_resolves("./😅/🤔", "relative-nested-unicode", "😅/🤔.js");
    assert_resolves("../counter", "parent-dir/entry", "../counter/index.js");
    assert_resolves("../counter", "parent-js/entry", "../counter.js");
    assert_resolves("../counter/counter", "parent-nested/entry", "../counter/counter.js");
    assert_resolves("./counter", "subdir", "counter/index.js");
    assert_resolves("./counter", "subdir-noext", "counter/index");
    assert_resolves("./", "pkginfo-basic", "counter.js");
    assert_resolves(".", "pkginfo-basic", "counter.js");
    assert_resolves("./counter", "pkginfo-nested", "counter/counter.js");
    assert_resolves("../", "pkginfo-parent/entry", "../counter.js");
    assert_resolves("..", "pkginfo-parent/entry", "../counter.js");
    assert_resolves(".", "pkginfo-dot", "index.js");
    assert_resolves("..", "pkginfo-dot/entry", "../index.js");
    assert_resolves("package", "modules-basic", "node_modules/package/index.js");
    assert_resolves("package", "modules-file", "node_modules/package.js");
    assert_resolves("package", "modules-pkginfo", "node_modules/package/entry.js");
    assert_resolves("package", "modules-pkginfo-relative", "node_modules/package/lib/index.js");
    assert_resolves("package/lib/counter", "modules-nested", "node_modules/package/lib/counter.js");
    assert_resolves(".package", "modules-dotted", "node_modules/.package/index.js");
    assert_resolves("counter", "modules-parent/subdir", "../node_modules/counter/index.js");
    assert_resolves("counter", "modules-multilevels/subdir/subdir/subdir/subdir", "../../../../node_modules/counter/index.js");
    assert_resolves("😅", "unicode-pkg", "node_modules/😅/index.js");
    assert_resolves("package", "unicode-pkg-entry", "node_modules/package/🤔.js");
    assert_resolves("🤔", "unicode-both", "node_modules/🤔/😅");
}

#[test]
fn test_normalize() {
    assert_eq!(
        normalize(&Path::new("/Users/shf/Projects").join(Path::new("/Users/shf/Projects/paq"))),
        PathBuf::from("/Users/shf/Projects/paq")
    );
    assert_eq!(
        normalize(&Path::new("/Users/shf/Projects").join(Path::new("paq"))),
        PathBuf::from("/Users/shf/Projects/paq")
    );
}
