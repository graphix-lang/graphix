# `queuefn` — queued function invocation

Status: built.
Pins: `stdlib/graphix-tests/src/lib_tests/core.rs` `queuefn_delta_per_cycle`, `queuefn_trigger_arg`,
`queuefn_trigger_before_fn`, `queuefn_sleeping_site_keeps_its_calls`, `queuefn_birth_fire_is_no_credit`,
`queuefn_wake_releases_one_call_a_cycle`, `queuefn_count_follows_a_moving_place`;
`tests/queue_credits.rs` `queuefn_banks_idle_permits`

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
val queuefn: fn<'a: Function>(?#count: [&mut i64, null], #trigger: Any, f: 'a) -> 'a;
```

- `f: 'a` — the function to wrap; the `Function` bound refuses
  anything else at the call, a function with a variadic argument or a
  defaulted label included (wrap a lambda that calls it).
- `#trigger: Any` — each update releases one queued invocation, or
  banks a pop.
- `#count: [&mut i64, null]` — optional writable ref; when non-null the
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
let slow = queuefn(#trigger: trig, #count: &mut depth, real_slow_fn);
let result = slow(x, y);   // spends a permit, or waits for one
```

The trigger supplies permits to a counting semaphore. It starts with one
permit and accumulates unused permits: several later invocations can run
between two timer ticks. It therefore does not impose a rate limit. Calls
released into the same site's bindings remain limited to one per runtime
cycle, preserving each invocation's argument delta.

## Semantics

State per `queuefn` call site: `queue` (pending invocations, oldest
first) and `pop_count` (banked permission slots, initialized to **1**
so the first invocation runs immediately and may emit in the same
cycle). `pop_count > 0` and a queue holding a call of a site awake are
mutually exclusive.

- **Wrapper invocation**: if `pop_count > 0`, decrement and invoke `f`;
  otherwise push the invocation and emit nothing this cycle.
<!-- XCR claude for claude: [doc-drift/low] Credit banks without bound: after N idle
trigger fires, N later calls dispatch at once. This contradicts the API example above
('at most one release per tick'), and queuefn is no rate limit. Decide: cap `pop_count`
(at 1, if a trigger is meant to pace calls) or fix the example and say credit
accumulates. probe:
design/review-2026-10-26/repro/x-async-io-queuefn-unbounded-credit.gx (x-async-io-06)
-->
<!-- 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q19, queuefn's
banked credit): yes, accumulate, fix the doc (the recommendation: accumulate (a
semaphore), fix the doc.) -->
<!-- 2026-10-28 codex: The implementation is graphix-package-core/src/queuefn.rs:
QueueState::pop_count already implements the ruled counting semaphore, initially one
permit, with trigger adding unused permits and wrapper calls consuming them. Kept that
accumulating behaviour. Fixed the rate-limit claim and writable-reference example in
queue_fn.md; documented banking and per-site cycle pacing in CLAUDE/AGENTS, the
interface, book and language skill; built the book. The original probe reproduced all
five answers at tick 6. Pin: graphix-tests --test queue_credits
queuefn_banks_idle_permits, all four modes: five post-birth idle triggers plus the
initial permit release exactly six calls without another trigger; the seventh stays
queued and #count is one. The as-built implementation passes. Temporarily capping
credit at one makes all four modes fail (one answer, six queued); restoring
accumulation passes all four. cargo test -p graphix-tests --offline --test
queue_credits: 4 passed; cargo test -p graphix-package-core --offline --lib: 7 passed.
cargo fmt --all now passes again, as does git diff --check. No runtime semaphore
replacement was needed: the existing synchronous counter under QueueState's mutex
already has the requested semantics. -->
- **`#trigger` update**: dequeue the oldest invocation of a site awake
  and invoke `f` with it; with none, `pop_count += 1`. A fire born with
  the queuefn (its first cycle, or a seq step's next run) is no credit:
  only fires after it exists release calls.
- **A site's sleep** is a pause: its calls keep their place in the queue
  and no release takes one while it sleeps, since a sleeping site's
  bindings would see only the last of several releases. At its wake the
  credit pays its own calls, oldest first, while it remains; they are
  released one a cycle, and a call it makes meanwhile queues behind them.
- **One release a cycle per site**: writes to a site's argument bindings
  made in one cycle land together, so two released calls would reach `f`
  as one. A site's release is in flight until it lands; a call the
  trigger or a credit pays meanwhile waits, paid, and the site releases
  it when the one before lands.
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
- Per call site of the wrapper, `WrapperApply` holds a
  `genn::Callback::through` the `fid` binding: a call of `f` over one
  fresh binding per argument of the site. An invocation with credit
  delivers its fired arguments to those bindings this cycle
  (`ExecCtx::deliver`), unless a release landed in them this cycle:
  then it writes them with `set_var`, so the release runs first and this
  call lands next cycle; with a release in flight it queues, paid. A push writes `#count` directly (the
  node's update is not re-entered by a push); a pop writes each recorded
  bind with `ctx.rt.set_var`, so the site's call fires next cycle.
- `#count` is written through `ExecCtx::write_target`: a place patches
  its root (`patch_level`), a binding is set (`set_level`). The node
  keeps the target the reference resolved to, and a target that arrives
  or moves (a place's key) is told the depth.
- Sleep: the node's `sleep` is a pause, keeping the queue and
  `pop_count`; its `reborn` (a seq step's next run) clears the queue,
  resets `pop_count` to 1 and writes `#count`. A wrapper site's `sleep`
  marks it asleep in `QueueState::asleep`, and its next update is its
  wake; its `delete` and `reborn` drop its queued entries, writing
  `#count`. The node's `delete` releases `fid`.
- A bottom argument at a wrapper site bottoms that invocation: it is
  neither queued nor dispatched.
- Containers: `QueueEntry.updates` and the per-cycle delta are
  `LPooled<Vec<(BindId, Value)>>`. `std::sync::Arc` remains only where
  the `InitFn` trait alias demands it.
- The builtin is `Effect::Async` (a pop lands on a later cycle), so it
  never fuses, and `ORDERED`: two forked branches never both reach it.
