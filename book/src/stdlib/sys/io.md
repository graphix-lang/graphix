# sys::io

The `sys::io` module provides a unified `Stream` type for all I/O
operations. The phantom type parameter constrains which stream kind is
accepted — `sys::fs::open` returns `Stream<\`File>`, `sys::tcp::connect`
returns `Stream<\`Tcp>`, and `sys::tls::connect` returns `Stream<\`Tls>`.

```graphix
/// An opaque handle to an I/O stream.
/// Stream<`File> for files, Stream<`Tcp> for TCP, Stream<`Tls> for TLS.
type Stream<'a: [`File, `Tcp, `Tls]>;

/// Read up to n bytes from the stream. May return fewer bytes than
/// requested if fewer are available.
val read: fn(Stream<'a>, u64) -> Result<bytes, `IOError(string)>;

/// Read exactly n bytes from the stream. Returns fewer bytes only
/// if EOF is reached before n bytes have been read.
val read_exact: fn(Stream<'a>, u64) -> Result<bytes, `IOError(string)>;

/// Write bytes to the stream. Returns the number of bytes written,
/// which may be less than the full length of data.
val write: fn(Stream<'a>, bytes) -> Result<u64, `IOError(string)>;

/// Write all bytes to the stream, looping until complete.
val write_exact: fn(Stream<'a>, bytes) -> Result<null, `IOError(string)>;

/// Flush any buffered writes.
val flush: fn(Stream<'a>) -> Result<null, `IOError(string)>;
```
