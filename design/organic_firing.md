# Organic Firing

Status: built 2026-08-14
Pins: `stdlib/graphix-tests/src/lang/organic_deltas.rs` (one fixture per ruled delta below); `graphix-fuzz/findings/quiet-frame-init-view-aug2026/`, `fold-midchain-fired-aug2026/`, `standing-bottom-refire-sep2026/`

## The rule

**A node fires iff a consumed input fires.** Sources fire when they
produce. Constants fire once, at init. Cadence is shaped explicitly —
`uniq`, `filter`, `~`, `once`/`take`/`skip` — and the compiler never
stores a previous value or a previous selection to decide a tag.

Consequences:

- **Select** emits whenever a consumed input fires — a scrutinee
  delivery, a CONSULTED guard's production, or the taken arm's own
  production (`own_sound`/`own_anyfire` in `node/select.rs`; the kernel
  folds the scrutinee and prologue-guard bits at every merge). The
  emission is the taken arm's current production: Fired if it holds a
  value, FreshBottom if it is bottom. Same-arm re-matches emit.
  Becoming-selected is not a separate event: a selection change implies
  a fired input, so the wake emission is the ordinary emission.
  Selection memory survives only for sleep/wake routing.
- **Calls**, recursive or not, fire organically: the body's selects
  fire per delivery, so the body produces per delivery, so the call
  fires per delivery. Recursion fires exactly like the hand-inlined
  chain, with no extra machinery; the tail and non-tail forms of one
  function have the same cadence.
- **Collection HOFs** fire by `scaffold::SlotFlags` — iff resized, or a
  slot fired, or the source fired empty — which is already
  delivery-based.

## Why

The project is one constraint-satisfaction problem: the simplest,
fastest implementation with the simplest user-facing semantics we can
defend (Eric, 2026-08-14). Gating a fire on value identity is what
`uniq` is for; the compiler must never do it implicitly. The
implementation difficulty of the previous rules was evidence against
them: reproducing "fire only when the selection or value changed" in a
kernel required mirroring the interpreter's retained-instance tree into
kernel state just to compute one tag bit. Under this rule no node in
the language compares a stored previous value or selection, and the
tail/non-tail cadence asymmetry (`f(n/2)` firing four times where
`0 + f(n/2)` fired once) disappears because the scrutinee fold IS the
general rule.

Rejected:

- **Fire on selection change only** (the strict-select rule): a
  same-arm re-match on a fired scrutinee was quiet, which needed a
  stored selection to decide the tag and made recursion fire
  differently from its inlined chain.
- **A pure function re-applied to unchanged inputs is not an event**
  (memoised call arguments): an implicit `uniq` at every call site, and
  the memo had to be reproduced per activation in kernels.
- **Guard fires do not count as inputs**: a guard-driven re-selection
  is a consumed input like any other; excluding it left the select's
  tag disagreeing with its own routing.

## The ruled deltas

Observable consequences ruled intended in advance; the numbers are
what `organic_deltas.rs` cites.

1. A scrutinee re-fire that re-matches the same arm EMITS.
2. A consulted guard's dependency firing with the selection unchanged
   EMITS.
3. The gating idiom `select enabled { true => data, _ => never() }`
   samples its arm on every `enabled` delivery; `uniq(enabled)` is the
   remedy when the level is what is wanted.
4. A select whose taken arm is `never()`/bottom emits FreshBottom per
   fired input (consistent with an op whose operand is a standing
   bottom).
5. A recursive call whose arguments fire at their previous value fires.
6. A tail re-dispatch with unchanged arguments fires at any iteration
   count, including zero.
7. A constant-terminal recursion fires per delivery on both engines.
8. The downstream cadence of `count`/`once`/effects over selects rises
   correspondingly — they observe honest deliveries.
9. `~` remains the sampling construct; `select t { _ => v }` is also a
   sampler, redundant rather than wrong.
