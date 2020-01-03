# js-resolve

A simple node.js module resolver in Rust.

It is based on the [Node.js resolution algorithm](https://nodejs.org/api/modules.html#modules_all_together).

## Usage

Just call `resolve`. The first parameter is the module (eg: a package name or a path) and the second one is the reference path.

The reference path should be the directory of the file that is requesting the module.

The first parameter is a string that references a module, a relative path, an absolute path or an empty string.

If the first parameter is an empty string it will check `package.json` and try to resolve to `index.js`/`index.mjs`/`index.json`.

If it is a string that references a module, it will look inside your `node_module` directories, and will do it recursively until it finds your package or reaches the root, in conformance with the original resolution algorithm.

```rust
use js_resolve;

js_resolve::resolve("express", PathBuf::from("/var/apps/server/"));
js_resolve::resolve("./index.js", PathBuf::from("/var/apps/server/"));
js_resolve::resolve("/app/index.js", PathBuf::from("/var/apps/server/"));
```

## LICENSE

```
Copyright (c) 2019 Silvio Henrique Ferreira

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
