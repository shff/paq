use paq::resolve::{resolve, normalize};
use std::path::{Path, PathBuf};

#[test]
fn test_resolve() {
    fn assert_resolves(name: &str, path: &str, expected: &str, is_browser: bool) {
        let fixtures = std::env::current_dir().unwrap().join("tests/fixtures/resolve");
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
