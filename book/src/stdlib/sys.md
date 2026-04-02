# sys

The `sys` module provides access to operating system level functionality:
files, sockets, timers, and the netidx publish/subscribe system. All I/O
goes through a unified `Stream` type defined in `sys::io`, so the same
`read`/`write` functions work on files, TCP connections, TLS streams,
and stdio.

| Module | Purpose |
|--------|---------|
| `sys::io` | Unified `Stream` type, read/write/flush, stdin/stdout/stderr |
| `sys::fs` | Open files, directory operations, filesystem watching |
| `sys::tcp` | TCP connect, listen, accept |
| `sys::tls` | Upgrade TCP streams to TLS |
| `sys::net` | Netidx publish/subscribe and RPC |
| `sys::time` | Timers and current time |
| `sys::dirs` | Platform-aware standard directory paths |

```graphix
/// the command line arguments. argv[0] is the script file.
val args: fn() -> Array<string>;

/// join parts to path using the OS specific path separator
val join_path: fn(string, @args: [string, Array<string>]) -> string;
```
