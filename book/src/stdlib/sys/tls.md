# sys::tls

TLS upgrades for TCP streams. A `TlsStream` implements the same io
traits as a `TcpStream` — and `sys::tcp::Socket` as well, since a TLS
session is still a socket — so nothing above the upgrade changes.

```graphix
{{#include ../../../../stdlib/graphix-package-sys/src/graphix/tls.gxi}}
```
