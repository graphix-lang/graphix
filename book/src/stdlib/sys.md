# sys

The `sys` module provides access to operating system level functionality:
files, sockets, processes, timers, and the netidx publish/subscribe
system. Each kind of stream (a file, a TCP or TLS connection, stdio, a
child process's pipe) is its own type, and the `Read`, `Lines`, `Write`,
`Close` and `Seek` traits give them the same operations; see
[sys::io](sys/io.md).

| Module | Purpose |
|--------|---------|
| `sys::io` | The stream traits, stdin/stdout/stderr |
| `sys::fs` | Open files, directory operations, filesystem watching |
| `sys::tcp` | TCP connect, listen, accept |
| `sys::tls` | Upgrade TCP streams to TLS |
| `sys::process` | Spawn child processes, their pipes and exit status |
| `sys::net` | Netidx publish/subscribe and RPC |
| `sys::time` | Timers and current time |
| `sys::dirs` | Platform-aware standard directory paths |

```graphix
{{#include ../../../stdlib/graphix-package-sys/src/graphix/mod.gxi}}
```
