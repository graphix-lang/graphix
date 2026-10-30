# sys::net - Netidx Operations

The `sys::net` module provides publish/subscribe and RPC operations via
[netidx](https://netidx.github.io/netidx-book).

`sys::net::call` runs calls side by side. Each fire of its arguments
starts a call, named by `#id` (an id `call_id` minted) or by a fresh id,
and every call answers `(id, result)` once: its reply, an error,
or `` `Aborted ``. The answers come in the order they arrive, one a
cycle, so a fast call is never held behind a slow one. A new call cancels
nothing, except that a call under an id still in flight replaces that
call, which answers `` `Aborted ``. One id for a search box keeps only
the latest query in flight:

```graphix
let query = "graphix";
let search = sys::net::call_id(null);
let (_, reply) = sys::net::call(#id: search, "/local/search", {q: query});
let hits: Array<string> = reply$;
```

`sys::net::abort_call(id)` aborts the call in flight under `id` from
anywhere in the program: it answers `` `Aborted `` and its reply is
ignored. Aborting, replacing or abandoning a call stops the wait, not the
server: netidx has no cancel, so the handler still runs and its effects
still happen. When the select arm holding the call sleeps, its calls in
flight are abandoned and answer nothing. The last completed result
survives the sleep, and waking alone sends no request. When a result must
be observed, keep the call outside the sleeping arm or in a seq step that
waits for it.

`sys::net::callq(path, args)` is the common case: no id goes in or comes
out, and the answers come in the order the calls were made. Each fire of
an argument still starts a call at once, so any number run side by side,
but an answer that comes before an earlier call's waits for it, so a
call the server never answers holds every later answer. Each call
answers once, one a cycle: its reply or an error. Nothing can replace or
abort a `callq` call, so its result has no `` `Aborted ``. When its arm
sleeps, its calls in flight are abandoned and the answers waiting behind
them are dropped.

```graphix
let sku = "graphix-mug";
let price: f64 = sys::net::callq("/local/price", {sku})$;
```

`callq` shares nothing with other call sites, so a fan-out over it (an
`array::map` over requests, a server's handlers) can run in parallel.
`call` cannot: any site may name a call's id, so the calls of a fan-out
over `call` run in program order.

```graphix
{{#include ../../../../stdlib/graphix-package-sys/src/graphix/net.gxi}}
```
