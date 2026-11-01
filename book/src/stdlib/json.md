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

### Unions and abstract types

JSON keeps no record of which member of a union a value was written as,
so a reader tells members apart only by a tag the data carries. Every
union in a type `json::read` reads or `json::write_str` writes is one
member beside `null`, or its members other than `null` each carry a tag,
no two the same:

- a variant carries its tag: `` `A `` is written `"A"` and `` `B(1) ``
  `["B",1]`;
- an abstract type carries its name: `Port(80)` is written `{"Port":80}`,
  an object of one entry keyed by the type's unqualified name, holding
  what its `Cast` implementation writes (its `to_value`, else its
  representation), and a read takes it only so.

`` [`A, `B(i64), Port, null] `` and `[T, null]`, for a `T` that is not
itself a union, read back. `[i64, string]`, `[u8, i64]`, `[Port, u8]`, `` [string, `A] ``,
`[{a: i64}, {b: i64}]` and a type variable beside another member are
refused, as a target and as a written value. So is a typedef whose
parameters grow as it recurses, such as ``type N<'a> = [`Nil, `Cons('a,
N<Array<'a>>)]``.

The data inside an abstract value's object is written for its reader:
where `T`'s `Cast` implementation writes `from_value`, which casts it as
data, the abstract values in it are written bare (`{"Tags":["a","b"]}`
for a `Tags` holding `Tag("a")` and `Tag("b")`); where the reader reads
the representation they are wrapped too (`{"Svc":{"port":{"Port":80}}}`).

`Transparent<T>` (in core) writes an abstract value bare, its data alone,
and reads it back through `T`'s `Cast`. It carries no tag, so in a union
it stands only beside `null`; there a `T` that writes `null` reads back as
the outer null. Over a type known not to be abstract it is refused: it
would change nothing (inside a generic definition, over the definition's
own type variable, it is not judged, and writes a non-abstract value's
data as it is). `Transparent<Transparent<T>>` writes what
`Transparent<T>` does. Everywhere else (casts, pack, netidx) a
`Transparent<T>` is its representation.

```graphix
type Port = Abstract<u16>;
impl Cast for Port {};
let p = Port(u16:80);
// {"Port":80}
json::write_str(p)$;
// 80
json::write_str(Transparent(p))$
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
The same rounding applies to `cast<decimal>` and `str::parse`. A
`decimal` is written as an exact number (`1.10`), never a string.

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

A map is an object when its key type is `string`. Under any other key
type, a bare variant's or an abstract type's included, it is an array of
`[key, value]` pairs, as it is where its type is not known (`Any`): the
type decides, never the keys. A key keeps its data structure, so a typed
read can reconstruct tuple, struct, array, list and payload-variant keys,
as well as null and bytes keys. For example, `{(1, 2) => 3}` writes as
`[[[1,2],3]]` and `{1 => 2}` as `[[1,2]]`.

A writer refuses a map whose distinct keys serialize as the same data
(`` `A `` and `"A"` under `Any`).
