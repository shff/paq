pub mod queue;
pub mod resolve;

use std::fs::read_to_string;
use std::path::PathBuf;

pub fn bundle(entry: PathBuf) -> Result<String, std::io::Error> {
    let source = read_to_string(&entry)?;

    Ok(source)
}
