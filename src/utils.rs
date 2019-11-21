use std::path::{Component, Path, PathBuf};

pub trait PathExt {
    fn is_explicitly_relative(&self) -> bool;
    fn join_normalizing(&self, more: &Path) -> PathBuf;
}

impl PathExt for Path {
    fn is_explicitly_relative(&self) -> bool {
        match self.components().next() {
            Some(Component::ParentDir) | Some(Component::CurDir) => true,
            _ => false
        }
    }
    fn join_normalizing(&self, more: &Path) -> PathBuf {
        more.components().fold(self.to_owned(), |path, c| match c {
            Component::Prefix(prefix) => PathBuf::from(prefix.as_os_str().to_owned()),
            Component::RootDir => path.join(std::path::MAIN_SEPARATOR.to_string()),
            Component::CurDir => path,
            Component::ParentDir => path.parent().unwrap().to_owned(),
            Component::Normal(part) => path.join(part),
        })
    }
}
