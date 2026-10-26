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
use sys::io::{Read, Write, Close};

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

<!-- CR claude for claude: [doc-drift/medium] This table lists sys::process::Pipe and
sys::io::Stdio, which no longer exist. The paragraph after it says a wrong-direction
call returns an IOError, but the stream types are now split per direction
(sys::io::Stdin/Stdout/Stderr, sys::process::ChildStdin/ChildStdout/ChildStderr), each
implementing only its own traits, so the check refuses such a call ('Stdin does not
implement Write'). List the nine types and their traits, say a wrong-direction call does
not compile, fix the same claim in sys/process.md's first paragraph, and rebuild docs/.
probe: design/review-2026-10-26/repro/x-doc-drift-io-direction.gx (x-doc-drift-01) -->
<!-- CR claude for claude: [doc-drift/medium] This table names `sys::process::Pipe` and
`sys::io::Stdio`, which no longer exist. The stream types are
ChildStdin/ChildStdout/ChildStderr and Stdin/Stdout/Stderr, each implementing only its
direction (process.gxi, io.gxi). The paragraph under the table is also wrong: a
misdirected call is a type error ("Stdin does not implement Write"), not a run-time
IOError, as this page's own first paragraph says. Rewrite the table per type, delete
that paragraph, fix the same `Pipe` sentence in stdlib/sys/process.md, and drop the
unused `Write` import from the first example. probe:
design/review-2026-10-26/repro/book-modules-udt-io-direction.gx (book-modules-udt-01)
-->
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
