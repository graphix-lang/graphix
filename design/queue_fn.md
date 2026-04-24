# `queue_fn` — queued function invocation

Status: design agreed, not yet implemented. Paused to finish the data-table
review; return to this afterward.

## Motivation

The existing `queue(#clock, v)` builtin queues a single value stream against
a clock. To queue a multi-argument function invocation you have to replicate
the queueing per argument and wire a feedback loop from the function's
output back to the clock by hand. The boilerplate pushes users toward
fire-and-forget semantics even when strict input/output pairing matters —
and then when the predicate of a `filter`-style pipeline stalls, they have
no way to observe it.

`queue_fn` packages the pattern as a first-class operation: wrap a function,
get back a handle, enqueue invocations against it, trigger releases
explicitly.

## API shape

```graphix
// Opaque handle parameterized by the wrapped fn type.
type QueuedFn<'f: fn>;

// Wrap a function. Returns a handle.
val queue_fn: fn<'f: fn>('f) -> QueuedFn<'f>;

// Read current queue depth. Reactive — updates whenever the queue changes.
val qfn_depth: fn<'f: fn>(QueuedFn<'f>) -> u64;

// Trigger release of one queued invocation. Fire-and-forget on the Any
// trigger arg; the result of the released invocation flows out of the
// `qfn_invoke` call site, not this one.
val qfn_trigger: fn<'f: fn>(QueuedFn<'f>, Any) -> null;

// Special: not a plain builtin. See "qfn_invoke" section below. The call
// shape at the source level is `qfn_invoke(q, a, b, c)` where (a, b, c)
// must type-match 'f's args; the return has 'f's return type.
val qfn_invoke: <magic>;
```

Typical wire-up for a self-driven queue (release-on-output):

```graphix
let q = queue_fn(my_fn);
let out = qfn_invoke(q, a, b, c);
qfn_trigger(q, out);
```

For an externally-driven queue, replace `out` with any other reactive
trigger (a timer, a user action, a frame boundary, etc.). No implicit
release mode — all releases are explicit.

## Key decisions

### Explicit trigger only

Manual trigger everywhere. No "auto release on output" default.

Rationale: auto-release is seductive for ergonomics but misbehaves when the
wrapped fn emits more than once per invocation (reactive inner updates,
animation loops, accumulators). The symptom — new args released mid-
invocation, or no release when the fn settles to a steady value — is nearly
impossible to diagnose from source. Making the trigger edge explicit costs
one extra line per use site and buys predictability.

### `qfn_invoke` as a compile-time construct, not a builtin

The core problem: graphix's type system can't express "the arg types are
'f's arg types and the return type is 'f's return type". Declaring
`qfn_invoke` as a plain builtin forces a signature like
`fn(QueuedFn<'f>, @args: Any) -> Any`.

`Any` in the return type flows into normal inference before NEEDS_CALLSITE
runs. Downstream TVars get unified with `Any`, and even when CallSite phase
later learns the correct types, the earlier inference decisions aren't
revisited. `json::read` sidesteps this by requiring a contextual type
annotation that pins the return before deferred checks fire; we can't ask
users to annotate every `qfn_invoke(q, ...)` call that way.

Instead: the compiler recognizes `qfn_invoke` at CallSite construction
time, reads the `QueuedFn<'f>` type off the first arg, and emits a node
whose signature is exactly 'f's signature. No `Any` ever enters inference.

`qfn_depth` and `qfn_trigger` stay as ordinary builtins — their signatures
are clean and don't need the special handling. Only `qfn_invoke` is
weird.

## Implementation sketch

### Abstract type `QueuedFn<'f>`

Declared in `graphix-package-core` (sibling of `queue`). Parameterized by
the fn type. Runtime representation: a `u64` handle indexing a per-`ExecCtx`
registry of queue states. Clones are refcount-free (plain value).

A `Fn` constraint on `'f` is new — today the documented constraints are
numeric (`Number`, `Int`, `Float`). Two options:
- Add an `Fn` constraint to the type system (cleanest).
- Leave `'f` unconstrained and rely on use sites (`queue_fn(f: 'f)` unifies
  `'f` with whatever was passed; only function values can reach it).

The unconstrained form may be enough; revisit if it lets ill-formed programs
typecheck.

### Per-queue state

Held in `ExecCtx` keyed by handle:

```rust
struct QueueState<R: Rt, E: UserEvent> {
    queue: VecDeque<ValArray>,   // one entry per queued invocation
    pred: Node<R, E>,            // compiled call to the wrapped fn
    fid: BindId,                 // ref to the wrapped fn value
    arg_bids: SmallVec<[BindId; 4]>, // one bind per fn arg
    out_bid: BindId,             // output value broadcast bind
    ref_count: usize,            // # of invoke sites referencing this handle
}
```

Clean up when all references drop.

### Builtins

- `queue_fn(f)`: allocate a `QueueState`, return `Value::U64(handle)`.
- `qfn_depth(q)`: read the state's `queue.len()`, emit as u64. Reactive on
  queue mutations.
- `qfn_trigger(q, trigger)`: on `trigger.update() == Some`, pop one arg
  tuple from the queue (if any), write each arg into `arg_bids`, let
  `pred.update()` produce the result into `out_bid`.

### `qfn_invoke` — compile-time specialization

In `graphix-compiler/src/node/compiler.rs`, when building a `CallSite` node:
- Detect the call whose head resolves to `qfn_invoke`.
- Extract `'f` from the first arg's resolved type (unify with `QueuedFn<'f>`).
- Type-check remaining args against 'f's arg types.
- Emit a node that:
  - On each arg update, pushes the new arg tuple onto the handle's queue.
  - Watches `out_bid` from the handle's state; emits whatever the wrapped
    fn produces.
  - Has return type = 'f's return type.

Pattern mirrors `MapQ`/`FoldQ`'s deferred-typecheck trick for their
predicate args, but produces a single call-application node rather than a
HOF wrapper.

## Known risks

1. **Stall visibility.** The whole point of queueing is explicit
   back-pressure; if the user forgets to wire `qfn_trigger`, the queue grows
   unbounded silently. `qfn_depth` being reactive helps (users can render
   it, assert on it, or gate with `>`), but consider a warn-on-threshold
   log for extreme growth.

2. **Handle lifetime and sleep/delete.** If the `queue_fn` node is deleted
   while an `invoke` site still holds the handle, reads must fail gracefully
   (emit null / stop emitting) rather than panic. Sleep needs to clear the
   queue and park the wrapped fn.

3. **Ordering across multi-arg updates.** When several of `qfn_invoke(q,
   a, b, c)`'s arg streams fire in the same cycle, we enqueue one tuple
   capturing that cycle's values. When they fire across cycles, each partial
   update produces a fresh tuple snapshotting the currently-cached values
   of the other args. Document clearly: queueing is keyed per-update-cycle,
   not per-arg-stream.

4. **`Fn` constraint.** If we don't add one, malformed programs like
   `queue_fn(5)` might slip past earlier type stages and surface only at
   the `qfn_invoke` callsite. Acceptable if the error message there is
   clear; consider adding the constraint if not.

## Return-to-work checklist

When picking this back up:

- [ ] Decide on `Fn` constraint vs unconstrained `'f`.
- [ ] Add `QueuedFn<'f>` abstract type to `graphix-package-core`.
- [ ] Implement `queue_fn`, `qfn_depth`, `qfn_trigger` builtins.
- [ ] Add `qfn_invoke` specialization in `graphix-compiler::node::compiler`.
- [ ] Write .gxi declarations; register in the core package.
- [ ] Tests: basic queue/release, depth reactivity, stall with no trigger,
  handle lifetime across sleep/delete, multi-arg update-cycle semantics.
- [ ] Book chapter in `book/src/stdlib/` — or under core queueing docs.
