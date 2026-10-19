# Async outputs across sleep

Status: built 2026-09-05; rewritten for "no partial rebirth" 2026-10-18
Pins: `stdlib/graphix-tests/src/lang/async_restart.rs`; `stdlib/graphix-tests/src/lib_tests/bottom.rs`

A wake is a resume, never a rebirth (`design/wake_catchup.md`, "No
partial rebirth"). What a builtin does across a sleep follows from what
its output is.

- **A call** (`CachedArgsAsync`: file, stream, process, http, db, sqlite
  operations; rpc `call`): sleep aborts the call in flight (it cannot be
  paused) and the wake restarts nothing, since a restart would perform
  an action no trigger asked for; only an argument's fire, a caught-up
  one included, starts a call. The result is a level and survives the
  sleep.
- **An event source** (timers, `after_idle`, `throttle`'s pending
  emission, line readers, net and db subscriptions, watches, servers,
  `iter`/`range` deliveries): it pauses. A timer's run and a pending
  idle wait resume at the wake; a standing interest (a subscription, a
  publication, a listing, a watch, a served path) is detached at sleep
  and re-attached at the wake over the arguments as they stand; a
  `net::write` re-attaches to its path but writes only on a value fire;
  a delivery stream in flight is aborted. Its output is the last event,
  which does not survive the sleep: a select emits its arm's value at a
  re-entry, so a kept event would be re-raised as if it were new.
- **State** (the restart builtins, `queue`, `iterq`, `filter`, `hold`):
  kept. A queue takes back what it sent through its reply variable and
  had not received (`InFlight`), and sends it again from the wake.

A builtin starts afresh only where it is born again: `Apply::reborn`,
which a call site runs before an update that follows its sleep under a
born view, a seq step entered for its next run.

## Audit

| Family | Sleep | Wake |
| --- | --- | --- |
| `CachedArgsAsync` (files, stream I/O, processes, HTTP requests, db, sqlite, other packages on the wrapper) | aborts the call in flight, remints the reply ID; keeps args and result | starts a call only on an argument fire |
| rpc `call` | remints the reply ID; keeps the result | as above |
| `timer` | cancels the armed fire; keeps the run; clears its output | re-arms the run if fires remain |
| `after_idle` | cancels a pending wait; clears its output | resumes a pending wait |
| `throttle` | cancels the pending emission; keeps the held value | — |
| `range`, `iter` | remints the delivery ID (the stream in flight is aborted); keeps the output | — |
| `queue`, `iterq` | takes back sent-undelivered values; keeps contents and credits | sends again |
| `once`, `take`, `skip`, `uniq`, `hold`, `count`, `filter` | nothing | — |
| net subscribe, list/list_table, publish, publish_rpc, `write` | detaches; clears the output | re-attaches over standing args (`write` writes only on a value fire) |
| filesystem watch application and event/path accessors | rebuilds the watch or detaches listeners; clears the output | rebuilds over standing args |
| database subscription and event accessors | aborts the subscription or detaches listeners; clears the output | re-attaches |
| HTTP server | stops serving, aborts its connections and handler instances; clears the output | serves again |
| I/O line readers | clears the last line, preserving the reader and its delivery ID | reads on |
| netidx-admin ceremony events | detaches from the ceremony event stream (`paused`) | re-attaches |
| `sys::args`, `sys::dirs::*` | nothing | — (produce at birth only) |

## Line reader identity

`IoLines` retains its reader and its delivery ID across sleep. The
reader owns the stream position; a second reader would race it for
bytes. Unbatched lines wait in the reader's queue until the graph takes
them, one a cycle, and the reader reads no more until it has: a
sleeping listener holds the reader back and loses nothing. A partial
line stays in the stream's read half (`BufferedRead`), so an operation
on the whole stream that stops the reader (a TLS upgrade, a seek) reads
those bytes first. A reselected listener starts without an old output
and receives later lines from the same reader.

## Strict consumers and kernels

Resetting a builtin does not invalidate every enclosing expression's
resident by itself. A strict consumer must propagate a consumed bottom
before considering its cached result — `StaleBottom` included, which
invalidates the value channel without creating an event. With several
consumed inputs any bottom makes the result bottom, fresh only if at
least one consumed input triggered. The interpreter's shared strict gate
(`dense_gate!`), unary operators, casts, abstract constructors and field
projections follow this rule; a valid quiet input can refill a bottom
resident without firing, and a wake forces a quiet recomputation
without making a stale production fresh.

The kernel wrapper invokes generated code when a feeder is bottom or
its own resident is bottom. It cannot simply bottom the whole kernel: a
feeder read only by an untaken select arm must not affect the output.
Generated code propagates the tags along consumed paths and the wrapper
stores the resulting value and freshness — branch-local consumption
preserved, and the wrapper's quiet fast path cannot reuse an invalid
output.

None of this touches the history retained by `hold`, by bindings, or
by async operations that stay awake.
