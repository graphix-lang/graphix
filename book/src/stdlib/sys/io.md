# sys::io

`sys::io` is where the io capabilities live. A stream is not one type
with a tag saying what it is — it is a type of its own (`sys::fs::File`,
`sys::tcp::TcpStream`, `sys::tls::TlsStream`, a type per direction for
a child's pipes, `sys::process::ChildStdin`, `ChildStdout` and
`ChildStderr`, and for the process's own, `sys::io::Stdin`, `Stdout`
and `Stderr`), and the traits it implements say what you can do with
it: a write to `Stdin` is a type error. Anything else that implements them — including a stream
written in Graphix — works with the same code.

```graphix
use sys::io::{Read, Close};

let f = sys::fs::open(`Read, "/etc/hostname")?;
let text = buffer::to_string(Read::read_all(f)?)?;
Close::close(text ~ f)?
```

`Read` is the one to implement if you are writing a stream of your own:
`read` is the only required method, and `read_exact` and `read_all` are
written in terms of it, so they come for free. The system streams
override `read_exact` with a single call into the operating system,
which reads under one lock instead of looping.

```graphix
{{#include ../../../../stdlib/graphix-package-sys/src/graphix/io.gxi}}
```

## Which stream implements what

| type | Read | Lines | Write | Close | other |
|------|------|-------|-------|-------|-------|
| `sys::fs::File` | ✓ | ✓ | ✓ | ✓ | `sys::fs::Seek` |
| `sys::tcp::TcpStream` | ✓ | ✓ | ✓ | ✓ | `sys::tcp::Socket` |
| `sys::tls::TlsStream` | ✓ | ✓ | ✓ | ✓ | `sys::tcp::Socket` |
| `sys::process::ChildStdin` | | | ✓ | ✓ | |
| `sys::process::ChildStdout` | ✓ | ✓ | | ✓ | |
| `sys::process::ChildStderr` | ✓ | ✓ | | ✓ | |
| `sys::io::Stdin` | ✓ | ✓ | | ✓ | |
| `sys::io::Stdout` | | | ✓ | ✓ | |
| `sys::io::Stderr` | | | ✓ | ✓ | |

A call in the wrong direction — writing to `stdin`, reading from a
child's stdin pipe — does not compile: the check refuses it with
`Stdin does not implement Write`.

## Parsing and writing formats

`json` and `toml` parse from `bytes` or a `string` and serialize to
either; `pack` reads and writes `bytes`; `xls` reads `bytes` and has no
writer. Reading a document from a stream is therefore just
reading the stream:

```graphix
use sys::io::{Read, Write};

let f = sys::fs::open(`Read, path)?;
let config: Config = toml::read(Read::read_all(f)?)?;

let out = sys::fs::open(`Create, out_path)?;
Write::write_exact(out, json::write_bytes(#pretty: true, config)?)?
```
