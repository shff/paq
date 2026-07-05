use paq::bundle;

#[test]
fn test_bundler() {
    fn assert_node(path: &str, value: &str) {
        let cur_dir = std::env::current_dir().unwrap();
        let fixtures = cur_dir.join("tests/fixtures/shake");
        let result = bundle(&fixtures.join(path)).expect("Error");
        let output = std::process::Command::new("node")
            .arg("-e")
            .arg(&result)
            .output()
            .expect("Error running node");
        assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), value);
    }
    assert_node("entry.js", "");
}
