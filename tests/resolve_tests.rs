use paq::resolve::{normalize, resolve};
use std::path::{Path, PathBuf};

#[test]
fn test_resolve() {
    fn assert_resolves(name: &str, path: &str, expected: &str) {
        let cur_dir = std::env::current_dir().unwrap();
        let fixtures = cur_dir.join("tests/fixtures/resolve");
        assert_eq!(
            resolve(name.to_string(), &fixtures.join(path).join("index.js")).unwrap(),
            normalize(&fixtures.join(path).join(expected.to_string())),
        );
    }

    assert_resolves("", "no-entry", "index.js");
    assert_resolves("./counter", "relative-file", "counter");
    assert_resolves("./counter", "relative-file-js", "counter.js");
    assert_resolves("./counter", "relative-file-mjs", "counter.mjs");
    assert_resolves(
        "./counter/counter",
        "relative-file-nested",
        "counter/counter.js",
    );
    assert_resolves("./😅", "relative-file-unicode", "😅.js");
    assert_resolves("./😅", "relative-dir-unicode", "😅/index.js");
    assert_resolves("./😅/🤔", "relative-nested-unicode", "😅/🤔.js");
    assert_resolves("../counter", "parent-dir/entry", "../counter/index.js");
    assert_resolves("../counter", "parent-js/entry", "../counter.js");
    assert_resolves(
        "../counter/counter",
        "parent-nested/entry",
        "../counter/counter.js",
    );
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
    assert_resolves(
        "package",
        "modules-pkginfo",
        "node_modules/package/entry.js",
    );
    assert_resolves(
        "package",
        "modules-pkginfo-relative",
        "node_modules/package/lib/index.js",
    );
    assert_resolves(
        "package/lib/counter",
        "modules-nested",
        "node_modules/package/lib/counter.js",
    );
    assert_resolves(
        ".package",
        "modules-dotted",
        "node_modules/.package/index.js",
    );
    assert_resolves(
        "counter",
        "modules-parent/subdir",
        "../node_modules/counter/index.js",
    );
    assert_resolves(
        "counter",
        "modules-multilevels/subdir/subdir/subdir/subdir",
        "../../../../node_modules/counter/index.js",
    );
    assert_resolves("😅", "unicode-pkg", "node_modules/😅/index.js");
    assert_resolves("package", "unicode-pkg-entry", "node_modules/package/🤔.js");
    assert_resolves("🤔", "unicode-both", "node_modules/🤔/😅");

    let absolute = std::env::current_dir()
        .unwrap()
        .join("tests/fixtures/resolve/absolute/counter.js");
    assert_resolves(absolute.to_str().unwrap(), "absolute", "counter.js");
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
    assert_eq!(
        normalize(&Path::new("/../..").join(Path::new("/../.."))),
        PathBuf::from("/")
    );
    assert_eq!(
        normalize(&Path::new("/Users/shf/./Projects")),
        PathBuf::from("/Users/shf/Projects")
    );
}
