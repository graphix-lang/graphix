# Async outputs across sleep

Status: built 2026-09-05
Pins: `stdlib/graphix-tests/src/lang/async_restart.rs`; `stdlib/graphix-tests/src/lib_tests/bottom.rs`

An operation restarted on wake has no completion until that activation
produces one. A builtin that discards pending work or detaches an event
source in `sleep` therefore clears its output to `TagValue::phantom()`
there; otherwise a reselected arm would surface the previous
activation's result as if the restarted operation had already
completed. Ordinary updates still retain the latest completed value
while waiting for the next result. Neither `seq` lowering nor select
history needs to know: a direct call to a restarted builtin reads as
"nothing yet" until it produces.

`Effect::Async` alone does not imply a reset: it is also the
conservative classification for some synchronous constructors, and
those keep their value.

## Audit

| Family | Sleep action that requires an output reset |
| --- | --- |
| `CachedArgsAsync` (files, stream I/O, processes, HTTP requests, other packages on the wrapper) | discards queued work and remints the reply ID |
| `after_idle`, `timer`, `throttle` | cancels timers and clears timing/input state |
| `range`, `queue`, array/map/list `iter` and `iterq` | discards the old delivery ID and buffered work |
| net subscribe, RPC call, list/list_table | detaches the subscription or reply channel |
| net write, publish, publish_rpc | tears down the target/publication; old errors are no longer current |
| filesystem watch application and event/path accessors | rebuilds the watch or detaches listeners |
| database subscription and event accessors | aborts the subscription or detaches listeners |
| HTTP server | aborts the server the previous output handle represented |
| I/O line readers | clears the last line, preserving the reader and its delivery ID |
| netidx-admin ceremony events | detaches from the ceremony event stream |

A builtin that remints its delivery ID on sleep must register the new
ID (array and list `iterq` alongside map's); otherwise the cleared
output exposes a permanent wait rather than a fresh completion.

Preserved across sleep: synchronous value/history builtins, grouping
accumulators, constant-like args/directory sources, the retained
`queuefn` callable, the filesystem watcher constructor, and the
netidx-admin ceremony handle constructors. These keep their semantic
value or refresh it without starting a new asynchronous wait.

## Line reader identity

`IoLines` retains its reader and its delivery ID across sleep. The
reader owns the stream position and the partial-line buffer; a second
reader would race it for bytes. A reselected listener starts without an
old output and receives later lines from the same reader.

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
