# pack

The `pack` module provides native binary serialization using the netidx
Pack format. Like `json::read`, `pack::read` uses type-directed
deserialization.

```graphix
use sys::io;

/// Decode a value from packed binary bytes or stream.
val read: fn([bytes, Stream<'a>]) -> Result<'b, [`PackErr(string), `IOErr(string), `InvalidCast(string)]>;

/// Encode a value to packed binary bytes.
val write_bytes: fn(Any) -> Result<bytes, `PackErr(string)>;

/// Encode a value and write to a stream.
val write_stream: fn(Stream<'a>, Any) -> Result<null, [`PackErr(string), `IOErr(string)]>;
```

The Pack format is a compact binary encoding native to netidx. It is
more space-efficient than JSON or TOML and supports the full range of
Graphix types including bytes, datetime, and duration.
