# js-resolve

A simple node.js module resolver in Rust.

It is based on the [Node.js resolution algorithm](https://nodejs.org/api/modules.html#modules_all_together).

## Usage

Just call `resolve`. The first parameter is the module (eg: a package name or a path) and the second one is the reference point. It should be the file that is requesting the module.

The first parameter can be a module, a relative path or an absolute path.

If the first parameter is an empty string it will check `package.json` and try to resolve to `index.js`/`index.mjs`/`index.json`.

```rust
use js_resolve;

js_resolve::resolve("express", PathBuf::from("/var/apps/server/"));
js_resolve::resolve("./index.js", PathBuf::from("/var/apps/server/"));
js_resolve::resolve("/app/index.js", PathBuf::from("/var/apps/server/"));
```

A convenience function for finding entry points is also provided:

```
js_resolve::resolve_entry("index.js", PathBuf::from("/var/apps/server/"));
```

The difference between `resolve` and `resolve_entry` is that in this case, the first parameter is always considered a relative path.

Like the first function, if the first parameter is an empty string, it will also check `package.json` and try to resolve to `index.js`/`index.mjs`/`index.json`.

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
