# miniqueue

A minimalist generic single-threaded queue that aggregates results and populates the queue with more tasks.

## Usage

Pass the first **job** that should be run as the first parameter.

The second parameter is a function that receives the **job** and executes, and then returns a tuple containing:

 - The **job result**
 - A vector with more **jobs** to be executed

All the **jobs** returned in the vector will be enqueued to be ran later. Notice that jobs that were already ran in the past will not be scheduled again.

The end result of `run` is a HashMap aggregating **jobs** and **job results**.

Both the **job** and **job result** are generic types.

```rust
use miniqueue;

let result = run(1, |num| {
    match num {
        1 => Ok(("one", vec![ 2 ])),
        2 => Ok(("two", vec![])),
        _ => Ok(("", vec![]))
    }
});

assert_eq!(result.unwrap().get(&1), Some(&"one"));
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
