# toml

The `toml` module provides TOML serialization and deserialization.
Like `json::read`, `toml::read` uses type-directed deserialization, and
the writers write a value as its type says, as `json::write_str` does: a
document is a struct or a map keyed by strings; an `Array<(string, T)>`
is refused as a document.
As with json, only a value whose type says nothing (`Any`) is written by
its contents, where an array of `(string, _)` pairs in key order is a
table.

Every union in a type `toml::read` reads or `toml::write_str` writes is
one member beside `null`, or members each carrying a tag, as with
[json](json.md#unions-and-abstract-types). An abstract value is a table of
one entry its type's name keys (`{ Port = 80 }`), and `Transparent<T>`
writes it bare.

TOML has no null. A struct field that is null is left out, and a key the
document leaves out reads as null where the field's type admits it, so an
optional field reads back. A null anywhere else, an array's element, a
map's value or what an abstract type writes, is refused when written.
TOML cannot write a `duration` or a `decimal` either, and has a datetime of
its own.

A map is a table when its key type is `string` and an array of `[key,
value]` pairs under any other, which must be inside a document field
because TOML requires a table at the root. Bytes write as an array of byte
integers.

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
