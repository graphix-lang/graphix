# Sync blocks, `await`, and `~!` (force)

> Split out of `design/interfaces.md` (2026-07-10) — that doc keeps the
> Iter/objects material and points here. This one pins the semantics of
> sequential code's two boundaries with the reactive graph: how
> sequential code CONSUMES reactive values (`await`), and how reactive
> code CONTROLS when sequential evaluation runs (`~!`).

## The two axes

A sync block interacts with the reactive graph on two independent axes
that earlier drafts conflated:

1. **Suspension** — what happens when a value the block needs has not
   ARRIVED yet. This is `await`'s domain. The async elaboration engine
   (per-index instantiation + re-evaluation, never-until-complete)
   already implements suspension; it currently INFERS the suspension
   points from effect analysis. `await` declares them.

2. **Re-triggering** — when a COMPLETED evaluation re-runs. This is
   `~!`'s domain, and it lives at the USE site, not the definition
   (per-callsite has won every design battle in this language).

Under this split there is no `sync<trigger>` syntax at all — a block
that needs precise triggering is gated where it is used:

```graphix
let f = || sync { ... };
both(a, b) ~! f()
```

## `await` — declared suspension points

`await e` is legal ONLY inside a sync block. It marks a read whose
value arrives asynchronously: an async function call, or a capture
whose defining expression has an async effect.

- **Mandatory**: an async call or async-derived capture read inside a
  sync block WITHOUT `await` is a compile error.
- **Bidirectional**: `await` on a provably-sync expression is also a
  compile error — the annotations must stay honest or they rot into
  noise.
- **Semantics**: `await` is the declared form of what effect analysis
  (passes 2/4) currently infers. The analysis becomes a VERIFIER of
  the programmer's claim instead of a silent oracle. Evaluation
  reaching an `await` whose value has not arrived parks there
  (never-until-complete); the block produces nothing this cycle and
  resumes as values arrive.
- **Not a trigger**: awaits do not define when the block re-runs.
  Two awaits in a block have no and/or structure to define — they are
  path-order suspension points. (An await-as-trigger reading was
  considered and rejected: it can't express and/or without new
  syntax, and it moves triggering back to the definition.)
- **The cost signal**: `for v in a { await f(v) }` puts the per-index
  instantiation + re-evaluation cost in the source text where the
  user agreed to it. An un-awaited async call silently flipping a
  loop to the expensive path was the old world; the annotation is the
  new one.
- Open question (revisit with real code): transitively, most bindings
  in a reactive program are async-derived, so mandatory await on
  CAPTURES may wallpaper blocks. Start strict (calls AND captures);
  if it's wallpaper in practice, fall back to calls-only, where the
  marker is unambiguous and carries the loop-cost signal.

## `~!` — the force operator

`t ~! e` is `~`'s gating twin: where `t ~ e` samples e's OUTPUT when t
fires (e still free-runs), `t ~! e` puts e's whole subtree to SLEEP
between triggers and evaluates it against the runtime cache when t
fires.

- **Dormant RHS**: between triggers the RHS receives no updates and
  holds no wake interest (the unselected-select-arm sleep machinery).
- **Cached-pull per trigger**: a trigger fire evaluates the RHS with
  its inputs' latest values PULLED FROM the runtime cache (the same
  delivery a select arm wake uses, per cached-into-Rt) — enough to
  compute and emit, but a cached delivery is not a fresh event: a
  stateful Sync builtin in the RHS (`count`) does not tick on
  replayed values. The first-ever trigger primes the dormant subtree
  the way any first dispatch does; it is NOT a per-trigger init view.
- **Arg presence still required**: `t` firing does not weaken call
  semantics. If an arg of a forced call has never arrived, the
  dispatch produces nothing this cycle (normal bottom rules). Force
  gates WHEN a call fires, not WHETHER it can.
- **Zero-arg calls**: `t ~! f()` finally gives no-data-input calls a
  firing schedule — the trigger IS the data dependency. (Today a
  sync call with no positional args is a compile error because it
  could never fire.)
- **Reactive-land only**: `~!` inside a sync block is a compile
  error. Sequential code has no trigger semantics — statements run in
  order. This gives the teaching symmetry: `await` is legal only
  inside sync; `~!` only outside.
- **Precedence**: same level as `~` (lowest binary), left-associative.
- **RHS is any expression**, not just a call — `t ~! sync { ... }`
  gates a whole block, which is what makes `sync<trigger>` syntax
  unnecessary.

## Trigger combinators — plain values, not syntax

And/or over triggers needs no trigger DSL; the trigger is an ordinary
value built with ordinary combinators:

```graphix
either(a, b) ~! f()        // fires when a or b fires (≈ Any)
both(a, b) ~! f()          // fires only when a and b fire in the SAME cycle
both(either(a, b), c) ~! f()
```

`either` is essentially the existing `Any`; `both` is a small new Sync
core builtin (fires iff all inputs fired this cycle — mostly arises
from correlated sources, e.g. a `set_many` batch fanning out).
Variadic `all(@xs)`/`any(@xs)` for arity. Because triggers are values
they can be let-bound, inspected, and tested — a debugging property no
trigger syntax has.

## Defaults

Bare `sync { ... }` re-runs on any capture update (unchanged — right
for the common case, zero ceremony). `await` marks arrival semantics
inside; `~!` adds use-site precision outside. Nothing else.

## Implementation notes

- `~!` is close to a one-armed select: arm sleep/wake + the arm-wake
  cached-replay machinery (settled after soak jul08l), with the
  trigger as the sole wake source. The per-call-site first-dispatch
  state word (built for the fold-callback priming, 2026-07-10) is the
  first-trigger priming in kernel land.
- `await` enforcement rides the existing effect analysis: declared
  awaits are CHECKED against inferred effects in both directions.
- Kernel-side, a forced region's trigger disc gates the region's
  firing; the RHS emits with the init flag wired to the trigger fire.
