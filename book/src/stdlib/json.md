# json

The `json` module provides JSON serialization and deserialization.
`json::read` uses type-directed deserialization — the target type is
inferred from the type annotation at the call site.

```graphix
{{#include ../../../stdlib/graphix-package-json/src/graphix/mod.gxi}}
```

Parsing takes `bytes` or a `string`, so parsing from a stream is
reading the stream ([sys::io](sys/io.md)):

```graphix,fragment
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

```graphix,fragment

let n: i64 = json::read("42")?;
let s: string = json::read("\"hello\"")?;
let user: {name: string, age: i64} = json::read("{\"name\": \"Alice\", \"age\": 30}")?;
let items: Array<{id: i64, label: string}> = json::read(data)?;
let maybe: [string, null] = json::read(data)?;
```

### Numbers

A number reads exactly where its position in the target holds an
integer or a `decimal`: `-9223372036854775809` read as an `i64` is an
`InvalidCast`, since it does not fit, and `9007199254740993.0` is
9007199254740993, not the float nearest it. A float position reads the
float nearest the text, and a position that holds neither, an `Any` or a
`string` say, reads every number that is not an integer as that `f64`,
whatever the rest of the target holds. Excess decimal fractional precision
rounds to the nearest representable decimal without passing through `f64`.
The same rounding applies to `cast<decimal>` and `str::parse`.

The reader keeps a number's text by way of serde_json's
`arbitrary_precision`, which passes a number as an object of one key,
`$serde_json::private::Number`. An object whose first key is that one
reads as the number its value names.

## Type-directed serialization

`json::write_str` and `json::write_bytes` write a value as its type at
the call site says: a struct is an object; an array, a tuple
or a `List` is an array (a `List` flat); a variant with a payload is an
array of its tag and payload. An `Array<(string, T)>` is an array of
pairs whatever order its keys are in. Only a value whose type says
nothing (`Any`) is written by its contents, where an array of
`(string, _)` pairs in key order is an object.

```graphix
let pairs: Array<(string, i64)> = [("a", 1), ("b", 2)];
// [["a",1],["b",2]]
json::write_str(pairs)$;
// {"a":1,"b":2}
json::write_str({a: 1, b: 2})$
```

A map whose keys all write as strings, numbers, booleans, datetimes, durations
or bare variant tags is an object. If any key has another form, the whole map
is an array of `[key, value]` pairs. The key keeps its data structure, so a typed
read can reconstruct tuple, struct, array, list and payload-variant keys, as
well as null and bytes keys. For example, `{(1, 2) => 3}` writes as
`[[[1,2],3]]`.

A writer refuses a map whose distinct keys serialize as the same data,
including when the map uses the pairs representation.
