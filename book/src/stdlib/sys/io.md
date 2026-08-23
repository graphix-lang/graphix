# sys::io

`sys::io` is where the io capabilities live. A stream is not one type
with a tag saying what it is — it is a type of its own (`sys::fs::File`,
`sys::tcp::TcpStream`, `sys::tls::TlsStream`, `sys::process::Pipe`,
`sys::io::Stdio`), and the traits it implements say what you can do
with it. Anything else that implements them — including a stream
written in Graphix — works with the same code.

```graphix
use sys::io::{Read, Write, Close};

let f = sys::fs::open(`Read, "/etc/hostname")?;
let text = buffer::to_string(Read::read_all(f)?)?;
Close::close(f)?
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
| `sys::process::Pipe` | ✓ | ✓ | ✓ | ✓ | |
| `sys::io::Stdio` | ✓ | ✓ | ✓ | ✓ | |

A handle whose direction is wrong for the call — writing to `stdin`,
reading from a child's stdin pipe — returns an `IOError` rather than
failing to compile: the trait says the operation exists, the operating
system says which end of the pipe you are holding.

## Parsing and writing formats

`json`, `toml`, `pack` and `xls` parse from `bytes` (or a `string`) and
serialize to them. Reading a document from a stream is therefore just
reading the stream:

```graphix
use sys::io::{Read, Write};

let f = sys::fs::open(`Read, path)?;
let config: Config = toml::read(Read::read_all(f)?)?;

let out = sys::fs::open(`Create, out_path)?;
Write::write_exact(out, json::write_bytes(#pretty: true, config)?)?
```
