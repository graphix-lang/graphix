# pack

The `pack` module provides native binary serialization using the netidx
Pack format. Like `json::read`, `pack::read` uses type-directed
deserialization.

```graphix
{{#include ../../../stdlib/graphix-package-pack/src/graphix/mod.gxi}}
```

The Pack format is a compact binary encoding native to netidx. It is
more space-efficient than JSON or TOML and supports the full range of
data types including bytes, datetime, and duration. Tuple, struct, variant
and list kinds are preserved, so a value in a union reads back as the same
composite kind. Abstract values still use their `Cast` implementation.

The reader also accepts older unmarked data. A kind erased by an older
writer cannot be recovered from an ambiguous union target; rewrite such
values from a source that still knows their original kind.
