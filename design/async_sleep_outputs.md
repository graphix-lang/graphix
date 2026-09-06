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

## Strict consumers and kernels

Resetting a builtin does not invalidate every enclosing expression's
resident. A strict consumer must propagate a consumed bottom before
considering its cached result. This applies even to `StaleBottom`: it
invalidates the value channel without creating an event. With multiple
consumed inputs, any bottom makes the result bottom, fresh only if at
least one consumed input triggered.

The interpreter's shared strict gate, unary operators, casts, abstract
constructors, and field projections follow this rule. A valid quiet
input can refill a bottom resident without firing. Sleep forces quiet
recomputation when needed; it does not make a stale production fresh.

The kernel wrapper also invokes generated code when a feeder is bottom
or its own resident is bottom. It cannot simply bottom the whole kernel:
a feeder used only by an untaken select arm must not affect the output.
Generated code propagates the tags along consumed paths and the wrapper
stores the resulting value and freshness. This preserves branch-local
consumption while preventing the wrapper's quiet fast path from reusing
an invalid output.

This does not change the history retained by `hold`, bindings, or async
operations that remain awake.

## Validation

`stdlib/graphix-tests/src/lang/async_restart.rs` checks repeated seq runs
with timers (including identical payloads), file reads, network subscribe
and RPC, queues and iterators, plus ordinary select reentry, live
debounce value retention, and line-reader delivery after reentry. The
cases run with fusion enabled and disabled. The projected-iterator
regression is enabled, with additional repeated-run cases covering
composed projections, tuple/struct construction, array/map lookup,
casts, and arithmetic.

`stdlib/graphix-tests/src/lib_tests/bottom.rs` drives all four production
tags directly, both with and without sleep. It checks exact freshness,
quiet invalidation and recovery, and an unused bottom feeder. Native
cases require `#[native]` compilation, so interpreter fallback cannot
mask a kernel discrepancy.

The compiler suite passes 168 tests. The Graphix suite passes 2566 tests;
its only two failures are the pre-existing `seq_shadow::sampled_closure`
interpreter/JIT regressions described in the review's F7 follow-up.
All 64 fuzzer tests pass, including scheduled-input comparisons and the
120-program generated sweep (690 fused regions, zero budget skips).
The netidx-admin package passes 28 tests, with its two existing
measurement tests ignored.
The Windows GNU all-targets check passes for `netidx-tools` and
`netidx-admin`, with the existing unused `sh_quote` import warning.
