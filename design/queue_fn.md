# `queuefn` — queued function invocation

Status: implemented v1 (2026-04-27). Lives in `graphix-package-core::queuefn`.
Working: pop_count=1 immediate, queue+trigger pop, multi-arg, closure
capture, count ref writes (through-the-ref via `byref_chain`).
Not yet implemented: per-generation wrappers when `f` re-emits — current
impl shares one queue across all generations, which is simpler and
sufficient for the common case. See "Implementation status" below.

## Motivation

The existing `queue(#clock, v)` builtin queues a single value stream against
a clock. To queue a multi-argument function invocation you have to replicate
the queueing per argument and wire a feedback loop from the function's
output back to the clock by hand. The boilerplate pushes users toward
fire-and-forget semantics even when strict input/output pairing matters —
and then when the predicate of a `filter`-style pipeline stalls, they have
no way to observe it.

`queuefn` packages the pattern as a single transformation: wrap a function,
get back a function with the same signature whose invocations are queued
and released by an external trigger.

## API shape

```graphix
val queuefn: fn(?#count: &[i64, null], #trigger: Any, f: 'a) -> 'a;
```

- `f: 'a` — the function to wrap. `'a` is structurally a fn type; checked
  in the CallSite phase. No `Fn` kind constraint is added — graphix
  doesn't have one yet, and the CallSite check is sufficient.
- `#trigger: Any` — each update releases one queued invocation (or banks a
  pop). Reactive idiom.
- `#count: &[i64, null]` — optional writable ref. When non-null, the
  builtin writes the queue depth whenever it changes. Pure observability.
- Return: a fn value of type `'a` — call it like any other function.

There is **no abstract handle type**. The queue is internal state of the
returned wrapper. Each call to `queuefn` produces a fresh wrapper bound to
a fresh queue.

Typical use:

```graphix
let trig = sys::time::timer(duration:1.s, true);
let depth = 0;
let slow = queuefn(#trigger:trig, #count:&depth, real_slow_fn);

// Now slow has the same signature as real_slow_fn, but invocations are
// rate-limited by trig — at most one release per timer tick.
let result = slow(x, y);
```

## Semantics

State per wrapper (internal to one `queuefn` call site):

- `queue: VecDeque<args-tuple>` — pending invocations, oldest first.
- `pop_count: i64` — banked permission slots. Initialized to **1**, so the
  first invocation can run immediately and may emit in the same cycle.

**Wrapper invocation** (user calls the returned fn):
- If `pop_count > 0`: decrement, invoke f with the args. Output flows
  through normally.
- Else: push args onto the queue. The call site emits nothing this cycle.

**`#trigger` update**:
- If queue non-empty: dequeue oldest args, invoke f with them.
- Else: increment `pop_count`.

`pop_count > 0` and `queue.len() > 0` are mutually exclusive — a non-empty
queue would have already drained any banked pops.

**Whenever queue depth changes**: if `#count` is non-null, write the new
depth through the ref.

**When `f` updates (re-emits a new fn value)**: produce a brand-new wrapper
with a fresh queue and `pop_count = 1`. Previously-returned wrappers stay
alive with their own state. They share `#trigger` and `#count` with the
new wrapper — every trigger pops every live wrapper, and any wrapper's
depth change writes `*count`. Acceptable: the common case is the user
discards stale wrappers.

**Concurrent invocations**: if `#trigger` fires before f finishes emitting
from a previous invocation, invocation N+1 starts and you get interleaved
output streams. The wrapper's emitted value is "everything f emits from
any in-flight invocation." Documented gotcha; the price of not having
auto-pop-on-output.

## Why a manual trigger

Auto-release on output is seductive for ergonomics but misbehaves when the
wrapped fn emits more than once per invocation (reactive inner updates,
animation loops, accumulators). The symptom — new args released mid-
invocation, or no release when the fn settles — is hard to diagnose from
source. Making the release edge explicit costs one extra reference per
use site and buys predictability.

## Why this shape (vs. the earlier QueuedFn handle)

The earlier design had an opaque `QueuedFn<'f>` handle type plus
`queue_fn`, `qfn_depth`, `qfn_trigger`, and a special `qfn_invoke`
compile-time form. `qfn_invoke` had to be a magic compile-time construct
because a plain builtin signature `fn(QueuedFn<'f>, @args: Any) -> Any`
poisoned type inference with `Any` before NEEDS_CALLSITE could fix it.

Returning a value of type `'a` directly avoids the entire problem:

- Call sites of the wrapper are plain function applications — no special
  form. Type inference works exactly like calling f.
- No abstract type to expose. The queue is implementation-internal.
- One builtin instead of four.
- Each `queuefn` call site naturally has its own queue, no handle
  juggling. The corner case `let q = queuefn(...); array::map(a, q);
  array::map(b, q);` shares the queue across both maps — that's "you
  asked for it" and matches user intent.

## Implementation sketch

### Constructing the wrapper

The wrapper is a `LambdaDef` constructed at runtime by the `queuefn`
builtin's `init` (or its first update, when `f` is first known). This
project does not currently have a builtin that returns a runtime-built
LambdaDef — `queuefn` will be the first. The wrapper LambdaDef:

- Has the same signature as `f` (extracted from the resolved fn type during
  the CallSite typecheck phase).
- Body is a synthesized expression that calls a private dispatch builtin,
  passing the args plus the (captured) reference to this `queuefn`'s
  internal queue state.
- "Captured" here means the dispatch builtin has access to per-wrapper
  state via a side-channel — likely a handle held in the queuefn node's
  state plus a paired registration so the lambda body's call resolves to
  the right state. Exact wiring TBD until we look at how lambda bodies
  reach builtin args today.

### Queue state

Held in the `queuefn` node:

```rust
struct QueueState<R: Rt, E: UserEvent> {
    queue: VecDeque<ValArray>,   // one entry per queued invocation
    pop_count: i64,              // initialized to 1
    pred: Node<R, E>,            // compiled call to the wrapped fn
    fid: BindId,                 // ref to the wrapped fn value
    arg_bids: SmallVec<[BindId; 4]>, // one bind per fn arg
    out_bid: BindId,             // output broadcast bind
}
```

When `f` updates: tear down old `pred`, build a new one against the new f
value, reset queue and pop_count. Old wrappers (if anyone still holds
them) keep their old state.

### Builtin update behavior

Each cycle, the `queuefn` node:

1. If `f` updated, build a new wrapper (and emit it as queuefn's output).
2. If `#trigger` updated:
   - Queue non-empty: dequeue, invoke pred with those args.
   - Queue empty: `pop_count += 1`.
3. Wrapper invocations from user code arrive via the dispatch path:
   - `pop_count > 0`: decrement, invoke pred.
   - Else: push onto queue. Emit nothing on the call site.
4. If queue depth changed and `#count` non-null: write `*count = depth`.

### Type-checking

The CallSite phase resolves `'a`. The `queuefn` builtin verifies that the
resolved `'a` is a fn type; if not, error. It uses the resolved arg/return
types to construct the wrapper LambdaDef's signature and the inner
`pred` application.

## Known risks

1. **Constructing a LambdaDef from a builtin.** Novel work in the compiler.
   The user expects this to be tractable because we're "just wrapping an
   existing one" — f is already a fn value with known structure. Spike this
   first to confirm.

2. **Stall visibility.** If the user forgets to wire `#trigger`, the queue
   grows unbounded. `#count` being a writable ref helps (the user can
   render or assert on depth); consider an opt-in warn-on-threshold log if
   silent growth becomes a footgun.

3. **Handle lifetime, sleep, delete.** Wrapper lambdas might outlive the
   `queuefn` node if pasted into longer-lived contexts. The dispatch
   builtin must fail gracefully (emit nothing) when the underlying state
   is gone. Sleep should clear the queue and reset pop_count.

4. **Ordering across multi-arg updates.** Queueing is per-cycle delta:
   each cycle, the wrapper captures only the args that actually fired
   that cycle as a `Vec<(BindId, Value)>`, queues that delta, and on
   pop re-fires only those bids. Args that did not fire in the
   originating cycle are not re-fired on pop. This preserves callsite
   semantics — the wrapped fn is unable to observe queueing. In
   particular, a wrapped fn with a trigger/clock arg (`tick ~ x`) will
   only emit when its trigger arg actually fires, both with and
   without queueing, and a partial update (one arg firing alone) does
   not produce a spurious "all args fired" event for the wrapped fn.
   See `queuefn_delta_per_cycle` and `queuefn_trigger_arg` in
   `graphix-tests`.

5. **Concurrent in-flight invocations.** Already noted — interleaved
   outputs are accepted semantics, not a bug.

## Implementation status (2026-04-27)

### What's wired
- `ExecCtx::wrap_lambda` is the public path for builtins to mint a
  `LambdaDef` and emit it as a Value (`graphix-compiler/src/lib.rs`).
  `queuefn` is the first user.
- Builtin `core_queuefn` in `graphix-package-core::queuefn`. Holds a
  shared `Arc<Mutex<QueueState>>`. The wrapper LambdaDef's `init` captures
  a clone of that Arc; per-call-site Apply impls push args / dispatch via
  the shared state.
- Per call site, the wrapper Apply allocates fresh arg `BindId`s and
  builds a `pred = genn::apply(reference(fid), [arg_bids], ftyp)`. The
  shared `fid` is owned by the queuefn node and updated whenever `f`
  re-emits, so all live preds see the latest `f`.
- Push path (no pop available): wrapper writes count ref directly, since
  queuefn's update isn't re-entered when a wrapper pushes.
- Pop path: queuefn pops oldest entry, calls `ctx.rt.set_var(arg_bid,
  v)` to schedule the bind for the next cycle; the call-site pred fires
  next cycle and emits.
- `#count` ref handling resolves the outer ByRef via `byref_chain` to
  the underlying target BindId, then writes through that.

### Final shape (deviation from earlier draft)
The earlier draft said: "When `f` updates, produce a brand-new wrapper
with a fresh queue; previously-returned wrappers stay alive with their
own state."

Final shape: one `LambdaDef` per `queuefn` call site, built lazily on
first `f` value, one shared QueueState. When `f` re-emits, the shared
`fid` updates so all wrappers start calling the latest `f`. No new
LambdaDef is minted, no per-generation state.

Resolved out-of-scope: this covers ~all real use cases, and a user who
genuinely needs per-generation wrapper isolation can build one by hand
on top of the existing `queue` builtin.

### Tests run manually
- pop_count=1 immediate: `qf(7)` → emits `70` same cycle ✓
- Queue + trigger: `qf(1) qf(2)` → both emit, `r2` after a tick ✓
- Multi-arg: `qf(x, y)` with two-arg fn ✓
- Closure capture: `qf` wrapping `|x| x * multiplier` where `multiplier`
  is a captured binding ✓
- Count ref writes: `depth` ramps up on push, down on pop ✓

### Open follow-ups
- [ ] Sleep/delete cleanup for wrappers — current impl only cleans the
  queuefn node's `fid`. Wrapper apply impls clean their own `pred`.
- [ ] Book chapter — likely under core queueing docs alongside `queue`.

### Resolved (2026-04-29 review)
- Per-cycle delta semantics: replaced the original "snapshot all
  cached args on each push" approach with a delta model where each
  queue entry holds only the `(BindId, Value)` pairs that actually
  fired that cycle. Pops now re-fire only the originating bids, so
  the wrapped fn sees exactly the same per-cycle arg pattern it
  would see without queueing. Added `queuefn_trigger_arg`,
  `queuefn_trigger_before_fn`, and `queuefn_delta_per_cycle`
  language tests covering trigger-style args, banking pop_count,
  and the asymmetric-fires case.
- Container types: `QueueEntry.updates` and the per-cycle delta in
  `WrapperApply` are `LPooled<Vec<(BindId, Value)>>`; arg_bids is
  `Arc<[BindId]>` (set once at wrapper construction).
- Shared state: `triomphe::Arc<Mutex<QueueState>>` (no Weak needed,
  no cycles). `std::sync::Arc` is still required for the `InitFn`
  closure since that's what the trait alias demands.
