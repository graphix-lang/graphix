# sys::net - Netidx Operations

The `sys::net` module provides publish/subscribe and RPC operations via
[netidx](https://netidx.github.io/netidx-book).

`sys::net::call` waits only for the latest call's reply. Starting another
call, or leaving the select arm containing the call, abandons the pending
reply. The server may already be running the request: abandoning its
reply does not cancel or undo the server's effects. The last completed
result survives a sleep, and waking alone does not send another request.
When the result must be observed, keep the call outside the sleeping arm
or in a seq step that waits for it.

```graphix
{{#include ../../../../stdlib/graphix-package-sys/src/graphix/net.gxi}}
```
