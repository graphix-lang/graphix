# `queuefn` — queued function invocation

Status: built 2026-04-27 (per-cycle delta semantics 2026-04-29)
Pins: `stdlib/graphix-tests/src/lib_tests/core.rs` `queuefn_delta_per_cycle`, `queuefn_trigger_arg`,
`queuefn_trigger_before_fn`

Lives in `graphix-package-core::queuefn`; the builtin is
`core_queuefn`, bound in core's `mod.gx`.

## Motivation

`queue(#clock, v)` queues a single value stream against a clock. To
queue a multi-argument function invocation you would replicate the
queueing per argument and wire a feedback loop from the function's
output back to the clock by hand. The boilerplate pushes users toward
fire-and-forget semantics even when strict input/output pairing
matters, and when a `filter`-style pipeline stalls they have no way to
observe it. `queuefn` packages the pattern as one transformation: wrap
a function, get back a function with the same signature whose
invocations are queued and released by an external trigger.

## API

```graphix
val queuefn: fn(?#count: &[i64, null], #trigger: Any, f: 'a) -> 'a;
```

- `f: 'a` — the function to wrap. `'a` is structurally a fn type,
  checked at the CallSite phase; there is no `Fn` kind constraint.
- `#trigger: Any` — each update releases one queued invocation, or
  banks a pop.
- `#count: &[i64, null]` — optional writable ref; when non-null the
  builtin writes the queue depth whenever it changes. Pure
  observability, and the stall-visibility tool: a forgotten trigger
  grows the queue without bound, and `#count` is how a program renders
  or asserts on that.
- Returns a fn value of type `'a`. There is no handle type; the queue
  is internal state of the returned wrapper, and each `queuefn` call
  site owns a fresh queue.

```graphix
let trig = sys::time::timer(duration:1.s, true);
let depth = 0;
let slow = queuefn(#trigger: trig, #count: &depth, real_slow_fn);
let result = slow(x, y);   // at most one release per tick
```

## Semantics

State per `queuefn` call site: `queue` (pending invocations, oldest
first) and `pop_count` (banked permission slots, initialized to **1**
so the first invocation runs immediately and may emit in the same
cycle). `pop_count > 0` and a non-empty queue are mutually exclusive.

- **Wrapper invocation**: if `pop_count > 0`, decrement and invoke `f`;
  otherwise push the invocation and emit nothing this cycle.
- **`#trigger` update**: if the queue is non-empty, dequeue the oldest
  invocation and invoke `f` with it; otherwise `pop_count += 1`.
- **Depth change**: write the new depth through `#count` when non-null.
- **`f` re-emits**: the queue and `pop_count` are NOT reset; later pops
  run queued args through the new `f`. A user who needs a fresh queue
  per generation builds it on `queue`.
- **Concurrent in-flight invocations**: if `#trigger` fires before `f`
  has finished emitting for a previous invocation, the outputs
  interleave. The wrapper's value is "everything `f` emits from any
  in-flight invocation" — accepted semantics, the price of a manual
  release edge.

**Queueing is a per-cycle delta, not a snapshot.** Each queue entry
holds only the `(BindId, Value)` pairs that actually fired in the
originating cycle, and a pop re-fires exactly those binds. Args that
did not fire are not re-fired, so the wrapped fn sees the same
per-cycle arg pattern it would see without queueing: a wrapped fn with
a trigger-style arg (`tick ~ x`) emits only when that arg fires, and a
partial update does not become a spurious all-args event. The wrapped
fn cannot observe the queueing.

## Why a manual trigger

Auto-release on output is seductive but misbehaves when the wrapped fn
emits more than once per invocation (reactive inner updates, animation
loops, accumulators): new args are released mid-invocation, or nothing
is released when the fn settles, and neither is diagnosable from
source. Making the release edge explicit costs one reference per use
site and buys predictability.

## Why a wrapped value rather than a handle

An earlier shape had an opaque `QueuedFn<'f>` handle plus `queue_fn`,
`qfn_depth`, `qfn_trigger` and a magic `qfn_invoke` compile-time form;
the form was needed because a builtin signature
`fn(QueuedFn<'f>, @args: Any) -> Any` poisoned inference with `Any`.
Returning a value of type `'a` avoids the problem: call sites of the
wrapper are plain applications typed exactly like calls to `f`, there
is no abstract type to expose, one builtin replaces four, and each call
site naturally owns its queue. `let q = queuefn(..); array::map(a, q);
array::map(b, q)` shares one queue across both maps, which is what the
user asked for.

## Implementation

- `ExecCtx::wrap_lambda` is the public path for a builtin to mint a
  `LambdaDef` and emit it as a first-class fn `Value`; `queuefn` is its
  first user. One `LambdaDef` per `queuefn` call site, built lazily on
  the first `f` value; the wrapper's `init` captures a
  `triomphe::Arc<Mutex<QueueState>>` shared with the `QueueFn` node.
- The node owns a shared `fid` bind holding the current `f`, updated
  whenever `f` re-emits, so every live wrapper calls the latest `f`.
- Per call site of the wrapper, `WrapperApply` allocates fresh arg
  `BindId`s and builds `pred = genn::apply(reference(fid), arg_bids)`.
  A push writes `#count` directly (the node's update is not re-entered
  by a push); a pop calls `ctx.rt.set_var(arg_bid, v)` for each
  recorded bind, so the call-site `pred` fires next cycle.
- `#count` resolves the outer ByRef through `byref_chain` to its target
  bind and writes through that.
- `sleep` clears the queue and resets `pop_count` to 1 (the arm-rewake
  restart, not a frame reset — `reset_replay` leaves the queue alone,
  because queued calls are semantic buffering). The wrapper's `sleep`
  and `delete` forward to its `pred`; the node's `delete` unrefs `fid`.
- Containers: `QueueEntry.updates` and the per-cycle delta are
  `LPooled<Vec<(BindId, Value)>>`; `arg_bids` is `Arc<[BindId]>`, set
  once at wrapper construction. `std::sync::Arc` remains only where the
  `InitFn` trait alias demands it.
- The builtin is `Effect::Async` (a pop lands on a later cycle), so it
  never fuses.
