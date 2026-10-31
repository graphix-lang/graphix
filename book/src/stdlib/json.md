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

### Unions in the target

JSON keeps no record of which member of a union a value was written as,
so a target holds no union one of whose members writes a value that
reads back as another. The reader picks a member as a cast does: a
variant whose tag the data names first, then one that holds the value
exactly before one that rounds it, then a member keeping the data's kind
(a number, a string) before one reading it as another kind, then the
union's order. A member that is itself a union, such as `Num` in `[Num,
string]` with `type Num = [i64, f64]`, keeps the kind where any of its
members does, so `[Num, string]` reads back, and so does `[Shape,
string]` with `type Shape = [{w: f64, h: f64}, f64]`: each value
`Shape` writes is judged by its member that writes it. A member the
value read back already is takes it whatever the union's order, and a
member that is a union of primitives reads a value as one of them, the
first that holds it exactly: with `type Amount = [f64, string]`, `[Amount,
i64]` reads back, since a JSON integer is the `i64`'s, and `Amount` would
read it as an `f64`, never as its text.
`[u8, i64]` (two integers), `[string, datetime]` (a datetime is written
as a string), `` [string, `A] `` (a variant with no payload is written as
its tag), `[decimal, f64]`, `[duration, i64]` (a whole `duration` is
written as a float such as `2.0`, which the integer member reads), `[f32,
i64]` (a large whole `f32` is written as its shortest text, `1234567936`
as `1234568000.0`, which the integer member holds exactly and the `f32`
only rounded), `[f32, string]` (`0.1` reads back as the string `"0.1"`,
which holds it exactly), `[Array<string>, Array<i64>]` (both write `[]`)
and a struct beside a map of strings (an object reads as both) are
refused. A tuple or a struct is read whole: `[(f32, u8), (f64, f64)]`
writes `(f32:0.1, u8:1)` as `[0.1,1]`, which the `(f64, f64)` member
reads exactly and the first member only rounded; `[(string, f32), (i64,
f64)]` reads `("1", f32:0.1)` back as `(1, 0.1)`; `[(i64, duration), (u8,
string)]` reads `(1, 3.5s)` back as `(u8:1, "3.5")`, since both read the
`3.5` as another kind and `(u8, string)` comes first. Members that
recurse are read at every level: with ``type TA = [i64, `Cons(f32, TA)]``
and ``type TB = [f64, `Cons(f64, TB)]``, `[TA, TB]` is refused, since
`` `Cons(f32:0.1, 3) `` is written as `["Cons",0.1,3]`, which reads back
as TB's `` `Cons(0.1, 3.0) ``. A union two of whose members write the same
value is refused even where it would read back the same:
``type R<'a> = [`Nil, `Cons('a, R<'a>)]`` makes `[R<i64>, R<bool>]`
refused, since both write `` `Nil `` as `"Nil"`; `R<i64>` alone is fine. A
typedef whose parameters grow as it recurses, such as ``type N<'a> =
[`Nil, `Cons('a, N<Array<'a>>)]``, is refused as a target. A map's keys are
written as their text where none can be a string, so
`Map<[decimal, datetime], i64>` is refused: the key `"3"` reads back as a
datetime. An abstract type the reader builds from its representation's
data comes after every member that reads the data as it is: beside
`bool`, `type S = Abstract<string>` (with `impl Cast for S {}`) is
refused, since `S("true")` reads back as `true`. One whose `impl Cast`
writes `from_value` may read any value it is tried on, so it is refused
beside a member tried after it, such as a variant with no payload. One
that writes only `to_value` still reads as its representation: with
`type S = Abstract<string>`, ``[S, `A]`` is refused, since `"A"` reads
back as `S("A")`.
Members JSON writes apart are fine: `[i64, f64]` (an integer and a number
with a fraction), `[f32, u16]` (an integer no wider than 16 bits holds
no `f32` text the `f32` does not hold too, and a real member takes it
first), `[string, i64]`, `` [`A(i64), `B(string)] ``, `[{a: i64}, {b:
i64}]`, `[T, null]`. Pack and db keep the member, so their reads take
these unions.

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

A map whose keys all write as strings, numbers, booleans, datetimes, durations
or bare variant tags is an object, unless it holds strings beside keys of
another kind, or its key type may hold a string while its keys are not
strings: an object's keys read back as strings, so `{1 => 10, "1" => 20}`
could not. If any key has another form, or the keys mix strings with another
kind, the whole map is an array of `[key, value]` pairs. The key keeps its data structure, so a typed
read can reconstruct tuple, struct, array, list and payload-variant keys, as
well as null and bytes keys. For example, `{(1, 2) => 3}` writes as
`[[[1,2],3]]`.

A writer refuses a map whose distinct keys serialize as the same data,
including when the map uses the pairs representation.
