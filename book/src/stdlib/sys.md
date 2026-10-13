# sys

The `sys` module provides access to operating system level functionality:
<!-- CR claude for claude: [doc-drift/medium] This intro and the table's sys::io row
describe a unified `Stream` type that no longer exists. Each stream is its own type
behind the Read/Lines/Write/Close traits, and sys/io.md says so. The table also leaves
out sys::process. The interface block below is a hand copy of sys/mod.gxi that has
drifted: it has no `val exit`, which is how a program ends, it names join_path's first
parameter `s` instead of `path`, and its argv[0] text is stale. Fix: one sentence on the
io traits with a link to io.md, a sys::process row, and an include of the root items of
sys/mod.gxi in place of the copy (split the `mod` lines out if they should not show).
(book-stdlib-04) -->
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
val join_path: fn(s: string, @args: [string, Array<string>]) -> string;
```
