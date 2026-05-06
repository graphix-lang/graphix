# sys::io

The `sys::io` module provides a unified `Stream` type for all I/O
operations. The phantom type parameter constrains which stream kind is
accepted — `sys::fs::open` returns `Stream<\`File>`, `sys::tcp::connect`
returns `Stream<\`Tcp>`, `sys::tls::connect` returns `Stream<\`Tls>`,
and `stdin`/`stdout`/`stderr` return `Stream<\`Stdio>`.

```graphix
/// An opaque handle to an I/O stream. The phantom type parameter indicates
/// the underlying stream kind, constraining which operations are valid.
/// Stream<`File> for files, Stream<`Tcp> for TCP, Stream<`Tls> for TLS,
/// Stream<`Stdio> for stdin/stdout/stderr.
type Stream<'a: [`File, `Tcp, `Tls, `Stdio]>;

/// Read up to n bytes from the stream. May return fewer bytes than
/// requested if fewer are available.
val read: fn(stream: Stream<'a>, n: u64) -> Result<bytes, `IOError(string)>;

/// Read exactly n bytes from the stream. Returns fewer bytes only
/// if EOF is reached before n bytes have been read.
val read_exact: fn(stream: Stream<'a>, n: u64) -> Result<bytes, `IOError(string)>;

/// Write bytes to the stream. Returns the number of bytes written,
/// which may be less than the full length of data.
val write: fn(stream: Stream<'a>, data: bytes) -> Result<u64, `IOError(string)>;

/// Write all bytes to the stream, looping until complete.
val write_exact: fn(stream: Stream<'a>, data: bytes) -> Result<null, `IOError(string)>;

/// Flush any buffered writes.
val flush: fn(stream: Stream<'a>) -> Result<null, `IOError(string)>;

/// Return a handle to standard input.
val stdin: fn(trigger: Any) -> Stream<`Stdio>;

/// Return a handle to standard output.
val stdout: fn(trigger: Any) -> Stream<`Stdio>;

/// Return a handle to standard error.
val stderr: fn(trigger: Any) -> Stream<`Stdio>;
```
