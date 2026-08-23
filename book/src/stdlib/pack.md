# pack

The `pack` module provides native binary serialization using the netidx
Pack format. Like `json::read`, `pack::read` uses type-directed
deserialization.

```graphix
{{#include ../../../stdlib/graphix-package-pack/src/graphix/mod.gxi}}
```

The Pack format is a compact binary encoding native to netidx. It is
more space-efficient than JSON or TOML and supports the full range of
Graphix types including bytes, datetime, and duration.
