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
    if is_browser {
        if let Some(new_path) = get_substitution(path) {
            return load(&new_path, is_browser);
        }
    }

    if path.is_file() {
        return Some(path.to_path_buf());
    }

    let extensions = vec!["js", "mjs", "json"];
    for extension in extensions {
        let new_path = path.with_extension(extension);
        if new_path.is_file() {
            return load(&new_path, is_browser);
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

fn normalize(p: &Path) -> PathBuf {
    p.components().fold(PathBuf::from("/"), |path, c| match c {
        Component::Prefix(ref prefix) => PathBuf::from(prefix.as_os_str().to_owned()),
        Component::RootDir => path.join("/"),
        Component::CurDir => path,
        Component::ParentDir => path.parent().unwrap().to_owned(),
        Component::Normal(part) => path.join(part),
    })
}

fn get_substitution(path: &Path) -> Option<PathBuf> {
    for ancestor in path.ancestors() {
        let pkg_path = ancestor.join("package.json");
        if let Ok(data) = read_to_string(pkg_path) {
            if let Ok(info) = json::parse(&data) {
                if let Ok(name) = path.strip_prefix(&ancestor) {
                    if let Some(name) = name.to_str() {
                        if let Some(new_path) = info["browser"][name].as_str() {
                            return Some(ancestor.join(new_path));
                        }
                    }
                }
            }
            return None;
        }
    }
    None
}

#[test]
fn test_resolve() {
    fn assert_resolves(name: &str, path: &str, expected: &str, is_browser: bool) {
        let fixtures = std::env::current_dir().unwrap().join("fixtures");
        assert_eq!(resolve(name.to_string(), &fixtures.join(path), is_browser), Some(
            normalize(&fixtures.join(path).join(expected.to_string()))
        ));
    }

    assert_resolves("", "no-entry", "index.js", false);
    assert_resolves("./counter", "relative-file", "counter", false);
    assert_resolves("./counter", "relative-file-js", "counter.js", false);
    assert_resolves("./counter", "relative-file-mjs", "counter.mjs", false);
    assert_resolves("./counter/counter", "relative-file-nested", "counter/counter.js", false);
    assert_resolves("./😅", "relative-file-unicode", "😅.js", false);
    assert_resolves("./😅", "relative-dir-unicode", "😅/index.js", false);
    assert_resolves("./😅/🤔", "relative-nested-unicode", "😅/🤔.js", false);
    assert_resolves("../counter", "parent-dir/entry", "../counter/index.js", false);
    assert_resolves("../counter", "parent-js/entry", "../counter.js", false);
    assert_resolves("../counter/counter", "parent-nested/entry", "../counter/counter.js", false);
    assert_resolves("./counter", "subdir", "counter/index.js", false);
    assert_resolves("./counter", "subdir-noext", "counter/index", false);
    assert_resolves("./", "pkginfo-basic", "counter.js", false);
    assert_resolves(".", "pkginfo-basic", "counter.js", false);
    assert_resolves("./counter", "pkginfo-nested", "counter/counter.js", false);
    assert_resolves("../", "pkginfo-parent/entry", "../counter.js", false);
    assert_resolves("..", "pkginfo-parent/entry", "../counter.js", false);
    assert_resolves(".", "pkginfo-dot", "index.js", false);
    assert_resolves("..", "pkginfo-dot/entry", "../index.js", false);
    assert_resolves("package", "modules-basic", "node_modules/package/index.js", false);
    assert_resolves("package", "modules-file", "node_modules/package.js", false);
    assert_resolves("package", "modules-pkginfo", "node_modules/package/entry.js", false);
    assert_resolves("package", "modules-pkginfo-relative", "node_modules/package/lib/index.js", false);
    assert_resolves("package/lib/counter", "modules-nested", "node_modules/package/lib/counter.js", false);
    assert_resolves(".package", "modules-dotted", "node_modules/.package/index.js", false);
    assert_resolves("counter", "modules-parent/subdir", "../node_modules/counter/index.js", false);
    assert_resolves("counter", "modules-multilevels/subdir/subdir/subdir/subdir", "../../../../node_modules/counter/index.js", false);
    assert_resolves("😅", "unicode-pkg", "node_modules/😅/index.js", false);
    assert_resolves("package", "unicode-pkg-entry", "node_modules/package/🤔.js", false);
    assert_resolves("🤔", "unicode-both", "node_modules/🤔/😅", false);

    assert_resolves("package", "browser-key-module-simple", "node_modules/package/non-browser.js", false);
    assert_resolves("package", "browser-key-module-simple", "node_modules/package/browser.js", true);
    assert_resolves("package/browser", "browser-key-module-simple", "node_modules/package/browser.js", false);
    assert_resolves("package/browser", "browser-key-module-simple", "node_modules/package/browser.js", true);
    assert_resolves("package/non-browser", "browser-key-module-simple", "node_modules/package/non-browser.js", false);
    assert_resolves("package/non-browser", "browser-key-module-simple", "node_modules/package/non-browser.js", true);

    assert_resolves("package", "browser-key-module-subdir", "node_modules/package/subdir/non-browser.js", false);
    assert_resolves("package", "browser-key-module-subdir", "node_modules/package/subdir/browser.js", true);
    assert_resolves("package/subdir/browser", "browser-key-module-subdir", "node_modules/package/subdir/browser.js", false);
    assert_resolves("package/subdir/browser", "browser-key-module-subdir", "node_modules/package/subdir/browser.js", true);
    assert_resolves("package/subdir/non-browser", "browser-key-module-subdir", "node_modules/package/subdir/non-browser.js", false);
    assert_resolves("package/subdir/non-browser", "browser-key-module-subdir", "node_modules/package/subdir/non-browser.js", true);

    assert_resolves("package", "browser-key-module-relative", "node_modules/package/non-browser.js", false);
    assert_resolves("package", "browser-key-module-relative", "node_modules/package/browser.js", true);
    assert_resolves("package/browser", "browser-key-module-relative", "node_modules/package/browser.js", false);
    assert_resolves("package/browser", "browser-key-module-relative", "node_modules/package/browser.js", true);
    assert_resolves("package/non-browser", "browser-key-module-relative", "node_modules/package/non-browser.js", false);
    assert_resolves("package/non-browser", "browser-key-module-relative", "node_modules/package/non-browser.js", true);

    assert_resolves(".", "browser-key-relative", "default.js", false);
    assert_resolves(".", "browser-key-relative", "browser.js", true);
    assert_resolves("./default", "browser-key-relative", "default.js", false);
    assert_resolves("./default", "browser-key-relative", "default.js", true);
    assert_resolves("./default.js", "browser-key-relative", "default.js", false);
    assert_resolves("./default.js", "browser-key-relative", "default.js", true);

    assert_resolves("./subdir", "browser-key-pkgjson-in-subdir", "subdir/default.js", false);
    assert_resolves("./subdir/default", "browser-key-pkgjson-in-subdir", "subdir/default.js", false);
    assert_resolves("./subdir/default.js", "browser-key-pkgjson-in-subdir", "subdir/default.js", false);
    assert_resolves("./subdir", "browser-key-pkgjson-in-subdir", "subdir/browser.js", true);
    assert_resolves("./subdir/default", "browser-key-pkgjson-in-subdir", "subdir/default.js", true);
    assert_resolves("./subdir/default.js", "browser-key-pkgjson-in-subdir", "subdir/default.js", true);

    assert_resolves(".", "browser-key-no-main", "index.js", false);
    assert_resolves("./index", "browser-key-no-main", "index.js", false);
    assert_resolves("./index.js", "browser-key-no-main", "index.js", false);
    assert_resolves(".", "browser-key-no-main", "browser.js", true);
    assert_resolves("./index", "browser-key-no-main", "index.js", true);
    assert_resolves("./index.js", "browser-key-no-main", "index.js", true);

    assert_resolves(".", "browser-key-no-main-relative", "index.js", false);
    assert_resolves("./index", "browser-key-no-main-relative", "index.js", false);
    assert_resolves("./index.js", "browser-key-no-main-relative", "index.js", false);
    assert_resolves(".", "browser-key-no-main-relative", "browser.js", true);
    assert_resolves("./index", "browser-key-no-main-relative", "index.js", true);
    assert_resolves("./index.js", "browser-key-no-main-relative", "index.js", true);

    assert_resolves(".", "browser-key-multiple-subdir", "default/main.js", false);
    assert_resolves(".", "browser-key-multiple-subdir", "browser/main.js", true);
    assert_resolves("./default/main", "browser-key-multiple-subdir", "default/main.js", true);
    assert_resolves("./default/main", "browser-key-multiple-subdir", "default/main.js", false);
    assert_resolves("./browser/main", "browser-key-multiple-subdir", "browser/main.js", true);
    assert_resolves("./browser/main", "browser-key-multiple-subdir", "browser/main.js", false);

    assert_resolves("./main.js", "browser-sub-simple", "main.js", false);
    assert_resolves("./main.js", "browser-sub-simple", "browser.js", true);

    assert_resolves(".", "browser-sub-index", "index.js", false);
    assert_resolves(".", "browser-sub-index", "browser.js", true);
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
