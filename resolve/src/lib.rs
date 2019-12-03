use std::path::{Path, PathBuf};
use std::fs::{read_to_string};

use utils::PathExt;
mod utils;

pub fn resolve_entry(name: String, context: &Path) -> Option<PathBuf> {
    let path = Path::new(&name);
    let new_path = context.join_normalizing(path);

    load(&new_path)
}

pub fn resolve(name: String, context: &Path) -> Option<PathBuf> {
    let path = Path::new(&name);
    if path.is_explicitly_relative() {
        let parent = context.parent()?;
        let new_path = parent.join_normalizing(path);

        load(&new_path)
    } else if path.is_absolute() {
        load(path)
    } else if name.is_empty() {
        load(context)
    } else if CORE.contains(&name.as_str()) {
        None
    } else {
        let parent = context.parent()?;
        let new_path = parent.join("node_modules").join(&path);

        load(&new_path).or(resolve(name, parent))
    }
}

fn load(path: &Path) -> Option<PathBuf> {
    if path.is_file() {
        return Some(path.to_path_buf());
    }

    let extensions = vec!["js", "mjs", "json"];
    for extension in extensions {
        let new_path = path.with_extension(extension);
        if new_path.is_file() {
            return Some(new_path);
        }
    }

    let pkg_path = path.join("package.json");
    if let Ok(data) = read_to_string(&pkg_path) {
        if let Ok(pkg_info) = json::parse(&data) {
            if let Some(main) = pkg_info["main"].as_str() {
                if main != "." && main != ".." {
                    return load(&path.join(main))
                }
            }
        }
    }
    if path.is_dir() {
        return load(&path.join("index"));
    }
    None
}

const CORE: &[&str] = &[
    "assert",
    "async_hooks",
    "buffer_ieee754",
    "buffer",
    "child_process",
    "cluster",
    "console",
    "constants",
    "crypto",
    "_debug_agent",
    "_debugger",
    "dgram",
    "dns",
    "domain",
    "events",
    "freelist",
    "fs",
    "fs/promises",
    "_http_agent",
    "_http_client",
    "_http_common",
    "_http_incoming",
    "_http_outgoing",
    "_http_server",
    "http",
    "http2",
    "https",
    "inspector",
    "_linklist",
    "module",
    "net",
    "node-inspect/lib/_inspect",
    "node-inspect/lib/internal/inspect_client",
    "node-inspect/lib/internal/inspect_repl",
    "os",
    "path",
    "perf_hooks",
    "process",
    "punycode",
    "querystring",
    "readline",
    "repl",
    "smalloc",
    "_stream_duplex",
    "_stream_transform",
    "_stream_wrap",
    "_stream_passthrough",
    "_stream_readable",
    "_stream_writable",
    "stream",
    "string_decoder",
    "sys",
    "timers",
    "_tls_common",
    "_tls_legacy",
    "_tls_wrap",
    "tls",
    "trace_events",
    "tty",
    "url",
    "util",
    "v8/tools/arguments",
    "v8/tools/codemap",
    "v8/tools/consarray",
    "v8/tools/csvparser",
    "v8/tools/logreader",
    "v8/tools/profile_view",
    "v8/tools/splaytree",
    "v8",
    "vm",
    "worker_threads",
    "zlib",
];

#[test]
fn test_resolve() {
    fn assert_resolves(name: &str, path: &str, expected: &str) {
        let fixtures = std::env::current_dir().unwrap().join("fixtures");
        assert_eq!(resolve(name.to_string(), &fixtures.join(path)), Some(fixtures.join(expected.to_string())));
    }
    fn assert_internal(name: &str) {
        assert_eq!(resolve(name.to_string(), Path::new("/")), None);
    }

    assert_resolves("", "no-entry/index.js", "no-entry/index.js");
    assert_resolves("./counter", "relative/index.js", "relative/counter");
    assert_resolves("./counter", "relative-js/index.js", "relative-js/counter.js");
    assert_resolves("./counter", "relative-mjs/index.mjs", "relative-mjs/counter.mjs");
    assert_resolves("../counter", "parent-js/entry/index.js", "parent-js/counter.js");
    assert_resolves("./counter/counter", "relative-nested/index.js", "relative-nested/counter/counter.js");
    assert_resolves("../counter", "relative-dir/entry/index.js", "relative-dir/counter/index.js");
    assert_resolves("../counter/counter", "parent-nested/entry/index.js", "parent-nested/counter/counter.js");
    assert_resolves("./", "pkginfo-basic/index.js", "pkginfo-basic/counter.js");
    assert_resolves(".", "pkginfo-basic/index.js", "pkginfo-basic/counter.js");
    assert_resolves("./counter", "pkginfo-nested/index.js", "pkginfo-nested/counter/counter.js");
    assert_resolves("../", "pkginfo-parent/entry/index.js", "pkginfo-parent/counter.js");
    assert_resolves("..", "pkginfo-parent/entry/index.js", "pkginfo-parent/counter.js");
    assert_resolves(".", "pkginfo-dot/index.js", "pkginfo-dot/index.js");
    assert_resolves("..", "pkginfo-dot/entry/index.js", "pkginfo-dot/index.js");
    assert_resolves("package", "modules-basic/index.js", "modules-basic/node_modules/package/index.js");
    assert_resolves("package", "modules-file/index.js", "modules-file/node_modules/package.js");
    assert_resolves("package", "modules-pkginfo/index.js", "modules-pkginfo/node_modules/package/entry.js");
    assert_resolves("package/lib/counter", "modules-nested/index.js", "modules-nested/node_modules/package/lib/counter.js");
    assert_resolves(".package", "modules-dotted/index.js", "modules-dotted/node_modules/.package/index.js");

    assert_internal("assert");
    assert_internal("fs");
    assert_internal("events");
}
