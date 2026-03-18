# sys::tls

TLS upgrades for TCP streams. After upgrading, use `sys::io::read`/`write`
as usual — the encryption is transparent.

```graphix
/// Upgrade a TCP stream to a TLS client connection. The hostname is
/// used for SNI and certificate verification. When ca_cert is null,
/// Mozilla root certificates are used; when provided, only that CA
/// is trusted.
val connect: fn(?#ca_cert:[bytes, null], string, io::Stream<`Tcp>)
    -> Result<io::Stream<`Tls>, `TLSError(string)>;

/// Upgrade a TCP stream to a TLS server connection using the given
/// PEM-encoded certificate chain and private key.
val accept: fn(#cert:bytes, #key:bytes, io::Stream<`Tcp>)
    -> Result<io::Stream<`Tls>, `TLSError(string)>;
```
