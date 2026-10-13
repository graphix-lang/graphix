# toml

The `toml` module provides TOML serialization and deserialization.
Like `json::read`, `toml::read` uses type-directed deserialization, and
the writers write a value as its type says, as `json::write_str` does: a
document is a struct or a `Map`, and an `Array<(string, T)>` is refused.
As with json, only a value whose type says nothing (`Any`) is written by
its contents, where an array of `(string, _)` pairs in key order is a
table.

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
