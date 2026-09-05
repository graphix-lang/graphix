# Async outputs across sleep

An operation restarted on wake has no completion until that activation
produces one. A builtin that discards pending work or detaches an event
source therefore clears its output to `TagValue::phantom()` in `sleep`.
Ordinary updates still retain the latest completed value while waiting
for another result. Neither `seq` lowering nor ordinary select history
needs to change for a direct call to a restarted builtin.

## Audit

| Family | Sleep action requiring an output reset |
| --- | --- |
| `CachedArgsAsync` (files, stream I/O, processes, HTTP requests, and other packages using the wrapper) | Discards queued work and remints the reply ID |
| `after_idle`, `timer`, `throttle` | Cancels timers and clears timing/input state |
| `range`, `queue`, array/map/list `iter` and `iterq` | Discards the old delivery ID and buffered work |
| Net subscribe, RPC call, list/list_table | Detaches the subscription or reply channel |
| Net write, publish, publish_rpc | Tears down the target/publication; old errors are no longer current |
| Filesystem watch application and event/path accessors | Rebuilds the watch or detaches event listeners |
| Database subscription and event accessors | Aborts the subscription or detaches listeners |
| HTTP server | Aborts the server represented by the previous output handle |
| I/O line readers | Clears the last line while preserving the reader and its delivery ID |
| netidx-admin ceremony events | Detaches from the ceremony event stream |

Array and list `iterq` also needed to register their new delivery IDs,
as map `iterq` already does. Otherwise clearing the output exposes a
permanent wait rather than a fresh completion.

Preserved: synchronous value/history builtins, grouping accumulators,
constant-like args/directory sources, the retained `queuefn` callable,
the filesystem watcher constructor, and netidx-admin ceremony handle
constructors. These retain their semantic value or refresh it without
starting a new asynchronous wait. `Effect::Async` alone does not imply
reset: it is also the conservative classification for some synchronous
constructors.

## Line reader identity

`IoLines` retains its reader and its delivery ID across sleep. The reader
owns the stream position and partial-line buffer; starting another reader
would race for bytes. Previously sleep reminted the ID without redirecting
the reader, permanently disconnecting subsequent lines. The existing
reader's progress is unchanged, but a reselected listener starts without
an old output and can receive later lines from that reader.

## Remaining F2 case

Resetting a builtin does not invalidate every enclosing expression's
resident. `seq go { let reply = (map::iter({go => go})).1; reply }`
still consumes the previous result on the second run: the tuple field
access rides its own old output when its child returns a standing bottom.
`seq_projected_iterator` preserves this failing reproducer as an explicitly
ignored test. Awaiting the tuple in its own step verifies the iterator's
reset independently of field-access retention. F2 is therefore not fully
resolved by the builtin audit.

## Validation

`stdlib/graphix-tests/src/lang/async_restart.rs` checks repeated seq runs
with timers (including identical payloads), file reads, network subscribe
and RPC, queues and iterators, plus ordinary select reentry, live
debounce value retention, and line-reader delivery after reentry. The
passing cases run with fusion enabled and disabled.

The Graphix suite passes 2496 tests, with the remaining F2 reproducer
explicitly ignored. The netidx-admin suite passed 28 tests with its two
existing ignored tests. The Windows GNU all-targets check for netidx-tools
and netidx-admin passes, with the existing unused `sh_quote` import warning.
