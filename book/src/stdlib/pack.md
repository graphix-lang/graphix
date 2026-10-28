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
composite kind. This preserves structural kinds, not abstract identity:
abstract values are written as their `Cast` data. For example, with
`type B = Abstract<i64>` and the default `Cast` implementation, a
`[B, i64]` holding `B(1)` reads back as `i64`, and an
`[Array<B>, Array<i64>]` holding `[B(1), B(2)]` reads back as
`Array<i64>`. The array kind survives; the abstract element type does
not. Use distinct variant tags when a union must retain that distinction.

The reader also accepts older unmarked data. A kind erased by an older
writer cannot be recovered from an ambiguous union target; rewrite such
values from a source that still knows their original kind.
