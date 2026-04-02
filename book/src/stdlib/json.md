# json

The `json` module provides JSON serialization and deserialization.
`json::read` uses type-directed deserialization — the target type is
inferred from the type annotation at the call site.

```graphix
use sys::io;

/// Parse JSON from a string, byte array, or I/O stream.
val read: fn([string, bytes, Stream<'a>]) -> Result<'b, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]>;

/// Serialize a value to a JSON string.
val write_str: fn(?#pretty: bool, Any) -> Result<string, `JsonErr(string)>;

/// Serialize a value to JSON bytes.
val write_bytes: fn(?#pretty: bool, Any) -> Result<bytes, `JsonErr(string)>;

/// Serialize a value and write JSON to a stream.
val write_stream: fn(?#pretty: bool, Stream<'a>, Any) -> Result<null, [`JsonErr(string), `IOErr(string)]>;
```

## Type-directed deserialization

The return type of `json::read` is determined by the type annotation on
the binding. The compiler resolves the concrete type at compile time and
generates the appropriate deserialization code.

```graphix
use json;

let n: i64 = json::read("42")?;
let s: string = json::read("\"hello\"")?;
let user: {name: string, age: i64} = json::read("{\"name\": \"Alice\", \"age\": 30}")?;
let items: Array<{id: i64, label: string}> = json::read(data)?;
let maybe: [string, null] = json::read(data)?;
```
