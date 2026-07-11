# reset_replay and evaluation frames

> Built 2026-07-11 (Eric approved the ruling that morning; it settles
> what `fuzz/soak-jul10c`'s class-2 residuals and the old tail-arg
> stale-cache pending ruling were both waiting on). This documents the
> semantics as landed — the mechanism, the classification rule, and
> the two channels that make the interpreter agree with the kernel.

## The problem

The node-walk keeps REPLAY MEMORY everywhere: `Cached` operand
wrappers, select's cached scrutinee (and its arm selection), builtin
arg slots (`CachedVals`), a call site's published arg values in
`rt.cached`. All of it exists so REACTIVE evaluation can combine one
fresh input with a quiet other (combineLatest).

Sequential evaluation re-runs the same shared node tree many times
per cycle — a `For` sync-loop iteration, a tail-loop jump. Replay
memory then leaks one iteration's sub-results into the next whenever
the next iteration's producer bottoms: `filter(a, |x| 10/x > 2)` over
zeros KEPT the errored elements (the select re-polled its stale
selection with fresh arm-body inputs); `fold(.., |v0,_| v0 % v0)`
emitted where 0 % 0 must bottom. The kernel never had the disease —
its temps are SSA, recomputed per iteration, with a taint bit for
bottoms.

## The mechanism

Three pieces, all landed:

1. **`reset_replay`** — a REQUIRED method on `Update` and `Apply`
   (no default: the replay-vs-semantic classification is a per-node
   decision the compiler must force). Clears replay caches, never
   semantic state, never wake registrations. It is NOT sleep: sleep
   is arm-rewake RESTART semantics (`once`'s flag and `count`'s tally
   reset on sleep; they survive reset_replay).

2. **Evaluation frames** — the For sync loop and the tail loop's
   re-entered passes run the body against a PRIVATE variables map
   (`mem::swap` of `event.variables`): externals seeded from
   `rt.cached` (delivery-fresh per cached-into-Rt), acc/elem (or the
   jump's rebound formals) inserted fired. A body publish — a shadow
   bind, a callsite arg, a callee formal — dies with the frame, so
   nothing needs to be enumerated for removal (the enumeration
   approach broke twice before). `ctx.frame_depth` marks frame
   execution; per-iteration `reset_replay` runs between frames.

3. **`ctx.frame_bottom`** — the interpreter's taint bit. The single
   `Option<Value>` channel conflates GENUINE bottom (a swallowed
   error: unchecked arith, handler-less `?`, `$`) with legitimate
   quiet (a stale input bridged by a value cache). Error sites mark
   the flag (a no-op outside frames); the frame driver treats the
   iteration as bottomed even when a downstream cache produced a
   value. Sticky, like the kernel's taint.

## The two channels (why some caches PERSIST)

The kernel separates VALUE from FIRING: a value can be present in a
slot while its disc says STALE. The interpreter's corpus-pinned
equivalents:

- **Builtin arg slots (`CachedVals`) persist across frames.** They
  are the value channel — the kernel's own per-arg last-value slots
  do exactly this. A const-result feeder (`f(v)` with a constant
  body) fires once ever; its slot value is what lets
  `push(res, f(v))` keep emitting per fired `res`
  (hof_const_body_prev_len). The fold dual (`|a,b| 100`) stays quiet
  because its value rides the DIRECT chain, which is the firing
  channel (hof-lift-firing pin). Value-level staleness from an
  in-frame ERROR is caught by `frame_bottom`, not by clearing slots.

- **Closed (refs-free) `Cached` subtrees keep their cache** — a
  constant expression's value is identical in every frame and cannot
  re-fire without an init view; the cache IS the kernel's immediate.
  Same rule for a call site's closed args: their `rt.cached` entries
  survive reset, and inside a frame the call site re-delivers them
  into the private map (the DynCall side-channel twin; gated on
  `frame_depth` — in reactive land that re-delivery re-ran effectful
  callees every cycle).

- **Select ties selection to the scrutinee cache**: a frame-varying
  scrutinee clears both (the filter bug); a closed scrutinee keeps
  both (a `select 1 { 1 => x, _ => y }` body keeps emitting per
  fired x — the coarse-firing pins).

## Init priming

No per-iteration init view (it made const-callback folds fire —
constants must be value-present, not firing). Instead the loop's
FIRST ITERATING run is init-forced (`For::primed`) so constants and
defaults materialize into their frame-surviving caches — the loop
mirror of a call site's first-dispatch priming. The tail loop keeps
its pre-existing per-jump init (a tail call is a CALL; its emission
is the return value, not a slots-word).

## What this settled

- The jul10c replay-leak class: 17 of 20 artifacts now agree
  (d06/d36/g89 are other classes — async queue/group machinery and
  pacing).
- The old tail-arg stale-cache pending ruling (fuzz/pending-ruling/):
  the tail frame + per-jump reset is the fix.
- The double-emission class (dynamic-module programs on the async
  per-index path) is NOT addressed — `update_async` is untouched.

## Costs and invariants

- Iteration cost: one `reset_replay` walk + frame map rebuild per
  iteration. The interp loop was already O(body) per iteration.
- `sleep` and `reset_replay` are independent contracts; neither
  calls the other. Every new node must implement both — the compiler
  enforces it.
- reset_replay must NEVER touch wake registrations (`ref_var`/
  `unref_var`) — that is sleep's business.
