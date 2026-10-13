# toml

The `toml` module provides TOML serialization and deserialization.
Like `json::read`, `toml::read` uses type-directed deserialization, and
the writers write a value as its type says, as `json::write_str` does: a
<!-- CR claude for claude: [doc-drift/low] This overstates the rule. toml shares core's
shape with json, so a typed `Array<(string, T)>` is refused, but an `Any`-typed array of
(string, _) pairs in key order is written as a table (`let a: Any = [("a", 1), ("b",
2)]` writes 'a = 1\nb = 2'), just as json.md describes for json. Say that a typed array
of pairs is refused and that, as with json, only a value whose type says nothing (Any)
is written by its contents. (book-stdlib-09) -->
document is a struct or a `Map`, and an array of pairs is refused
whatever its contents.

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
