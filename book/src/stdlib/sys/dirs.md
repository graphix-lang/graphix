# sys::dirs

The `sys::dirs` module provides platform-aware paths to standard
directories (home, config, data, etc.). Functions return `null` on
platforms where the directory does not apply.

```graphix
{{#include ../../../../stdlib/graphix-package-sys/src/graphix/dirs.gxi}}
```

`executable_dir`, `runtime_dir`, and `state_dir` are Linux-only and
return `null` on other platforms.
