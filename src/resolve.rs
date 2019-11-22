use std::collections::HashMap;
use std::path::{PathBuf};

pub enum Resolved {
    Ignore,
    Normal(PathBuf),
}

pub fn stringify_deps(deps: &HashMap<String, Resolved>) -> String {
    let collection = deps.iter().map(|(name, resolved)| {
        match resolved {
            Resolved::Ignore =>
                serde_json::to_string(name).unwrap() + ":Pax.ignored",
            Resolved::Normal(path) => {
                serde_json::to_string(name).unwrap() + ":file_" + &path.safe_string()
            }
        }
    }).collect::<Vec<String>>().join(",");
    vec!["{", &collection, "}"].join("")
}

trait SafeString {
    fn safe_string(&self) -> String;
}

impl SafeString for PathBuf {
    fn safe_string(&self) -> String {
        self.to_string_lossy().chars().filter(|a| a.is_ascii_alphanumeric()).collect::<String>()
    }
}

#[test]
fn test_stringify_ignored() {
    let mut deps = HashMap::<String, Resolved>::default();
    deps.insert("dep1".to_string(), Resolved::Ignore);
    assert_eq!(stringify_deps(&deps), "{\"dep1\":Pax.ignored}");
}

#[test]
fn test_stringify_normal() {
    let mut deps = HashMap::<String, Resolved>::default();
    deps.insert("dep1".to_string(), Resolved::Normal(PathBuf::from("node_modules/dep2")));
    assert_eq!(stringify_deps(&deps), "{\"dep1\":file_nodemodulesdep2}");
}

#[test]
fn test_stringify_multiple() {
    let mut deps = HashMap::<String, Resolved>::default();
    deps.insert("depa".to_string(), Resolved::Ignore);
    deps.insert("depb".to_string(), Resolved::Ignore);
    assert_eq!(stringify_deps(&deps), "{\"depa\":Pax.ignored,\"depb\":Pax.ignored}");
}
