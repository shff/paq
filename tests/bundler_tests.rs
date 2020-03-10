use rakastu::bundle;

#[test]
fn test_bundler() {
    fn assert_bundle(path: &str, substring: &str) {
        let fixtures = std::env::current_dir()
            .unwrap()
            .join("tests/fixtures/bundler");
        let result = bundle(&fixtures.join(path).join("index.js")).expect("Error");
        assert!(result.contains(substring))
    }
    fn assert_node(path: &str, value: &str) {
        let fixtures = std::env::current_dir()
            .unwrap()
            .join("tests/fixtures/bundler");
        let result = bundle(&fixtures.join(path).join("index.js")).expect("Error");
        let output = std::process::Command::new("node")
            .arg("-e")
            .arg(&result)
            .output()
            .expect("Error running node");
        assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), value);
    }
    assert_bundle("basic", "console.log('hello')");
    assert_bundle("with-dep", "/* math.js */");
    assert_bundle("double-quotes", "/* math.js */");
    assert_bundle("crazy-indent", "/* math.js */");
    assert_bundle("with-modules", "PETER RULES");
    assert_bundle("with-modules-2", "PartitionIter");
    assert_node("basic", "hello");
    assert_node("with-dep", "2");
    assert_node("double-quotes", "");
    assert_node("crazy-indent", "");
    assert_node("with-modules", "Once upon a day there was a person, named Peter DeMartini\nHe is awesome!\nHave a nice day...\n \n \n \nPETER RULES!!!");
    assert_node("with-modules-2", "0 1 2 3 4");
}
