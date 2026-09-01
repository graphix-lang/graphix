# Strict fusion — retreat to pure computation?

Status: PROPOSED, measured, awaiting Eric's ruling (2026-09-01). The
question, in Eric's words: "Are we trying to fuse too much? How much
simpler would both our lives be if we defused everything but stateless
non-async code that contains only fastcall builtins? … the user can
still get performance by structuring their code to arrange pure
computation together … the code we did fuse would probably be faster
because it would strip out all the mechanisms we're using to deal
with the above."

## The case

Every soak class since July has lived at one boundary: fused code
with interior cross-invocation state. Site identity (per-site inner
Applies, site words, per-slot chains, per-activation block trees and
their reclaim), the stale/taint mask delivery protocol, the
SLEEP_RESTARTS and has_stateful_reach gates, selection-memory words,
the quiet-frame trio, wire bit 2 / wake hints / the birth view — all
of it exists to make stateful kernel interiors mimic the interp, and
the default-arg-birth finding showed the mimicry can reproduce the
interp's bugs so faithfully that BOTH engines break identically and
the differential oracle goes blind.

Under a pure+fastcall-only regime, kernels are pure functions of
(inputs, discs) with exactly one kind of cross-invocation memory: the
firing boundary (output discs, taint propagation, one prev-length
word per HOF loop for resize detection). The user model collapses to
one sentence — "pure code over fastcall builtins fuses" — and
`#[native]` marks every cliff.

## The prototype

`GRAPHIX_STRICT_FUSE=1` (dbgenv; zero default-path change): refuses
emission of (a) any DynCall-path builtin dispatch — everything that
is not a direct FASTCALL, which covers every stateful, effectful,
seam-gated, and defaulted-label site in one predicate; (b) fused
`connect`; (c) handler-ful `?`. Lambda callees are covered
transitively (their bodies hit the same gates). Refusal = node-walk =
the canonical semantics by construction.

Known prototype over-refusals a real implementation would admit: the
`cast<>`/qop pseudo-sites (pure, compiler-internal, `fastcall: None`
only as an artifact). The numbers below are therefore a FLOOR on what
strict keeps.

## The numbers (2026-09-01, washu-chan, release build, quiet box)

**Semantic safety**: all 464 findings-corpus pins agree under strict.
The one flag raised was `dyncall-tagblind-print-aug2026/04`, whose
`#[native]` on an effectful loop correctly became a compile error —
the cliff signal working as designed.

**Coverage census** (per-program fused-kernel counts, both modes,
whole findings corpus): 94% of kernels survive (821/877); 401 of 464
programs fuse identically; 12 lose all fusion, 51 lose some — and
the losers are overwhelmingly the stateful-fusion BUG WITNESSES
(dyncall-stale-arg, sleep-restart-gate, tagblind-print,
recursive-activation-blocks, …), which this corpus over-represents
by construction.

**Bench corpus** (bench/run.sh, then variance-checked with repeated
runs — the min-of-N harness was noise-skewed on first pass):

| bench | verdict |
|---|---|
| fold_sum, fold_floatmath, filter_fold, map_fold, list_fold_sum, mandelbrot, leibniz_pi, symbolic, stream_stats | flat |
| list_map_fold, netidx_stream | flat (runs overlap; strict's best faster) |
| composite_seams | strict consistently FASTER (~37 vs ~42ms) |
| tail_sum | **+6.5%** (31.75ms stable vs ~33.8ms) — the only real regression |

tail_sum's loop kernel is IDENTICAL under strict (same back-edged
CLIF); the delta is call periphery (the fused result-statement region
vs an interp call site entering the loop kernel), which admitting the
pure cast pseudo-site would likely recover — no user modification
involved. The stream benches were the predicted risk and came out
flat because their stateful pieces (array::window in a `<-` RHS,
count in control flow) already sit at seams fusion never crosses:
**the bench corpus already writes hot code in the strict idiom
without having been told to** — pure folds in fused position,
stateful/async machinery at the boundaries. That is the strongest
form of the "user can restructure" argument: no restructuring was
needed even once.

## What a real strict regime would delete (sketch)

DynCall site identity end to end (slots' instances/recipes/site
blocks, SelfBlock trees + reclaim, SiteAnchor chains,
slot_state_sites), the dyncall stale/taint side-channel and the
dispatcher's inner-Apply path, the P7/stateful/key-0 gates
(replaced by the one admission predicate), selection-memory words +
arm-wake init overrides + wake hints + wire bit 2 + the birth view,
kernel replay words (audit — likely reduces to prev-len only),
arm-lift machinery in kernels. Kept: the firing/taint boundary
discs, the ABI, prev-len words, the scaffold loops, tail collapse,
native recursion (now stateless), cross-kernel calls.

What survives regardless: the entire interp semantics built this
arc (wake catch-up, the birth rule, sys::net reconcile) — that is
language semantics, engine-independent, and all 464 pins hold under
both regimes.

## Open questions for the ruling

- Fuse pure SELECTS (guards/arms pure, scalar merges) or retreat to
  straight-line + loops + recursion only? The measurement kept them
  and symbolic (guard-heavy, 12s) stayed flat — but its win over
  node-walk is already ~1x, so the coverage is cheap either way.
- Grow the FASTCALL set deliberately (re:: via a global pattern
  cache; the opt:: partial-delivery family stays out on semantics).
- Whether `#[native]`'s contract should be advertised as THE
  performance model in the book once the cliff is this legible.
