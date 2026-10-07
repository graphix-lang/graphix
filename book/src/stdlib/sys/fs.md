# sys::fs - Filesystem Operations

The `sys::fs` module provides functions for reading, writing, and watching files and directories.

## Interface

```graphix
{{#include ../../../../stdlib/graphix-package-sys/src/graphix/fs.gxi}}
```

An open `File` reads, writes and closes through the [sys::io](io.md)
traits, and seeks through `Seek`:

```graphix
use sys::{fs::{Seek, open}, io::{Close, Read, Write}};

let f = open(`Create, path)?;
let written = Write::write_exact(f, buffer::from_string("hello"))?;
let rewound = Seek::seek(f, written ~ `Start(u64:0))?;
let text = buffer::to_string(Read::read_all(rewound ~ f)?)?;
Close::close(text ~ f)?
```

## sys::fs::watch

```graphix
{{#include ../../../../stdlib/graphix-package-sys/src/graphix/fs/watch.gxi}}
```

## sys::fs::tempdir

```graphix
{{#include ../../../../stdlib/graphix-package-sys/src/graphix/fs/tempdir.gxi}}
```
