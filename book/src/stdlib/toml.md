# toml

The `toml` module provides TOML serialization and deserialization.
Like `json::read`, `toml::read` uses type-directed deserialization.

```graphix
use sys::io;

/// Parse TOML from a string, byte array, or I/O stream.
val read: fn([string, bytes, Stream<'a>]) -> Result<'b, [`TomlErr(string), `IOErr(string), `InvalidCast(string)]>;

/// Serialize a value to a TOML string.
val write_str: fn(?#pretty: bool, Any) -> Result<string, `TomlErr(string)>;

/// Serialize a value to TOML bytes.
val write_bytes: fn(?#pretty: bool, Any) -> Result<bytes, `TomlErr(string)>;

/// Serialize a value and write TOML to a stream.
val write_stream: fn(?#pretty: bool, Stream<'a>, Any) -> Result<null, [`TomlErr(string), `IOErr(string)]>;
```

## Example

```graphix
use toml;

type Config = {
    host: string,
    port: i64,
    debug: bool
};

let cfg: Config = toml::read(sys::fs::read_all("config.toml")?)?;
let out = toml::write_str(#pretty: true, cfg)?;
```
