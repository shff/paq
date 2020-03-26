use paq::lexer::get_deps;

use std::fs::read_to_string;

#[test]
fn test_parser_deps() {
    fn assert_dep(path: &str, substring: &str) {
        let entry = std::env::current_dir()
            .unwrap()
            .join("tests/fixtures/lexer")
            .join(path)
            .join("index.js");
        let source = read_to_string(&entry).unwrap();
        let result = get_deps(&source);
        assert!(result.contains(substring))
    }
    assert_dep("deps-modules", "peter");
    assert_dep("deps-crazy-indent", "./math.js");
    assert_dep("deps-double-quotes", "./math");
    assert_dep("deps-modules-2", "itt");
    assert_dep("deps-relative", "./math.js");
    assert_dep("deps-multiple", "math");
    assert_dep("deps-multiple", "./index.js");
    assert_dep("deps-multiple", "fs");
}
