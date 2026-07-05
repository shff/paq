use paq::bundle;
use std::path::PathBuf;

fn fixture(name: &str) -> PathBuf {
    let dir = std::env::current_dir().unwrap();
    dir.join("tests/fixtures/shake").join(name)
}

#[test]
fn test_export_keywords_stripped() {
    let output = bundle(&fixture("entry.js")).expect("bundle failed");
    assert!(!output.contains("export "), "output should not contain 'export': {}", output);
    assert!(output.contains("function used"), "output should contain the used function: {}", output);
}

#[test]
fn test_dead_code_eliminated() {
    let output = bundle(&fixture("entry.js")).expect("bundle failed");
    assert!(output.contains("used"), "output should contain 'used': {}", output);
    assert!(!output.contains("unused"), "output should not contain 'unused': {}", output);
}

#[test]
fn test_no_import_statements_in_output() {
    let output = bundle(&fixture("entry.js")).expect("bundle failed");
    assert!(!output.contains("import "), "output should not contain import statements: {}", output);
}
