# sys::tcp

```graphix
{{#include ../../../../stdlib/graphix-package-sys/src/graphix/tcp.gxi}}
```

`TcpStream` implements the `sys::io` traits, so reading and writing a
socket is the same code as reading and writing a file:

```graphix
use sys::io::{Read, Write};
use sys::tcp::Socket;

let s = sys::tcp::connect("example.com:80")?;
Write::write_exact(s, buffer::from_string("GET / HTTP/1.0\r\n\r\n"))?;
let reply = buffer::to_string(Read::read_all(s)?)?;
Socket::peer_addr(s)?
```
