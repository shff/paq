use std::path::{Component, Path, PathBuf};

pub trait PathExt {
    fn is_explicitly_relative(&self) -> bool;
    fn normalize(&self) -> PathBuf;
}

impl PathExt for Path {
    fn is_explicitly_relative(&self) -> bool {
        match self.components().next() {
            Some(Component::ParentDir) | Some(Component::CurDir) => true,
            _ => false,
        }
    }
    fn normalize(&self) -> PathBuf {
        self.components().fold(PathBuf::from("/"), |path, c| match c {
            Component::Prefix(ref prefix) => PathBuf::from(prefix.as_os_str().to_owned()),
            Component::RootDir => path.join("/"),
            Component::CurDir => path,
            Component::ParentDir => path.parent().unwrap().to_owned(),
            Component::Normal(part) => path.join(part),
        })
    }
}

#[test]
fn test_utils() {
    assert_eq!(
        Path::new("/Users/shf/Projects").join(Path::new("/Users/shf/Projects/paq")).normalize(),
        PathBuf::from("/Users/shf/Projects/paq")
    );
    assert_eq!(
        Path::new("/Users/shf/Projects").join(Path::new("paq")).normalize(),
        PathBuf::from("/Users/shf/Projects/paq")
    );
}
