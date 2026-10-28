# toml

The `toml` module provides TOML serialization and deserialization.
Like `json::read`, `toml::read` uses type-directed deserialization, and
the writers write a value as its type says, as `json::write_str` does: a
document is a struct or a map with scalar keys; an `Array<(string, T)>`
is refused as a document.
As with json, only a value whose type says nothing (`Any`) is written by
its contents, where an array of `(string, _)` pairs in key order is a
table.

Maps follow JSON's key rule: scalar keys form a table, while a map holding
a composite or bytes key is an array of `[key, value]` pairs. Such a map must
be inside a document field because TOML requires a table at the root. TOML
cannot represent a null key. Bytes write as an array of byte integers.

```graphix
{{#include ../../../stdlib/graphix-package-toml/src/graphix/mod.gxi}}
```

## Example

```graphix

type Config = {
    host: string,
    port: i64,
    debug: bool
};

let cfg: Config = toml::read(sys::fs::read_all("config.toml")?)?;
let out = toml::write_str(#pretty: true, cfg)?;
```

A writer refuses a map whose distinct keys serialize as the same data,
including when the map uses the pairs representation.
