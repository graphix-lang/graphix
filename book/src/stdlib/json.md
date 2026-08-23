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
