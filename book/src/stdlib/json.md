# json

The `json` module provides JSON serialization and deserialization.
`json::read` uses type-directed deserialization — the target type is
inferred from the type annotation at the call site.

```graphix
{{#include ../../../stdlib/graphix-package-json/src/graphix/mod.gxi}}
```

Parsing takes `bytes` or a `string`, so parsing from a stream is
reading the stream ([sys::io](sys/io.md)):

```graphix
use sys::io::{Read, Write};

let f = sys::fs::open(`Read, path)?;
let user: {name: string, age: i64} = json::read(Read::read_all(f)?)?;

let out = sys::tcp::connect(addr)?;
Write::write_exact(out, json::write_bytes(user)?)?
```

## Type-directed deserialization

The return type of `json::read` is determined by the type annotation on
the binding. The compiler resolves the concrete type at compile time and
generates the appropriate deserialization code.

```graphix

let n: i64 = json::read("42")?;
let s: string = json::read("\"hello\"")?;
let user: {name: string, age: i64} = json::read("{\"name\": \"Alice\", \"age\": 30}")?;
let items: Array<{id: i64, label: string}> = json::read(data)?;
let maybe: [string, null] = json::read(data)?;
```

## Type-directed serialization

`json::write_str` and `json::write_bytes` write a value as its type at
the call site says: a struct or a `Map` is an object; an array, a tuple
or a `List` is an array (a `List` flat); a variant with a payload is an
array of its tag and payload. An `Array<(string, T)>` is an array of
pairs whatever order its keys are in. Only a value whose type says
nothing (`Any`) is written by its contents, where an array of
`(string, _)` pairs in key order is an object.

<!-- CR claude for claude: [doc-drift/low] The block below does not parse. Its two
statements end with trailing `// [["a",1],["b",2]]` and `// {"a":1,"b":2}` comments, and
a comment must stand on its own line above what it describes. Put each comment on its
own line above its statement, or give the outputs in prose after the block. The outputs
themselves are correct. (book-stdlib-07) -->
```graphix
let pairs: Array<(string, i64)> = [("a", 1), ("b", 2)];
json::write_str(pairs)$;        // [["a",1],["b",2]]
json::write_str({a: 1, b: 2})$  // {"a":1,"b":2}
```
