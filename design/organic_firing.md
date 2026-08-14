# Organic Firing — the fired-plane simplification

Status: RULED by Eric, 2026-08-14 (conversation: the const-terminal
witness → "option 4 is change the semantics" → fired-args recursion →
fire-on-discriminant selects → this doc). Not yet built; this is the
as-ruled record and the migration plan.

Supersedes (fired plane only — the bottom/ride axis is untouched):
- THE STRICT SELECT RULE (2026-08-06) and its scrutinee/guard-quiet
  clauses
- The per-slot loop-select firing rule (2026-07-15, "an arm fires once
  when it becomes selected...")
- The guard-disc non-folding rule (2026-08-07 arc, item 1)
- The recursion ruling's "a pure function re-applied to unchanged
  inputs is not an event" clause (2026-08-13); its structure rulings
  (unconditional retention, no park) stand
- tail-zero-iteration quiet (cceb0809, 2026-08-13)
- The derivation-changed machinery as semantics (kernel memo + wire
  slot 3 damping + the interp's aug13h entry-args memo)

## The rule

**A node fires iff a consumed input fires.** Sources fire when they
produce. Constants fire once. Cadence is shaped explicitly — `uniq`,
`filter`, `~`, `once`/`take`/`skip` — and the compiler NEVER stores a
previous value or selection to decide a tag.

Consequences, spelled out:

- **Select**: emits whenever any of its inputs fire — the scrutinee, a
  guard dependency, or the taken arm's production. The emission is the
  taken arm's current resident: Fired if it holds a value, FreshBottom
  if it is bottom (op-consistency: an op with a standing-bottom operand
  also mints FreshBottom when triggered). Same-arm re-matches emit.
  Selection memory survives ONLY for sleep/wake routing (a resource
  concern, no longer a firing concern). Becoming-selected is subsumed:
  a selection change implies a fired input, so the wake emission is
  just the ordinary emission.
- **Calls** (recursive and not): fire organically — the body's selects
  fire per delivery, so the body produces per delivery, so the call
  fires per delivery. No force-fire, no memo. Recursion fires like the
  hand-inlined chain, and BOTH fire per delivery: chain-equivalence
  holds again, with no machinery.
- **Tail spine**: the scrutinee fold IS the general rule now. The
  tail/non-tail cadence asymmetry (`f(n/2)` fired 4×, `0 + f(n/2)`
  fired 1×) dies.
- **Collection HOFs**: unchanged — the SlotFlags rule (fires iff
  resized ∨ a slot fired ∨ source fired empty) is already
  delivery-based and consistent.

## The philosophy (Eric, 2026-08-14)

The project is one big constraint satisfaction problem: the simplest,
fastest implementation with the simplest user-facing semantics we can
defend. `uniq` is the combinator for gating firing on value
uniqueness; the compiler must never do it implicitly. The
implementation difficulty of the old rules was evidence against them —
the exact fix for the old semantics required mirroring the interp's
retained-instance tree into kernel state just to compute a tag bit.
Under the new rule, no node in the language compares a stored previous
value or selection: firing is fully organic. Test churn and soak-clock
resets are accepted costs ("long term we're still at day 0") — but
every flipped expectation must be ENUMERATED and mapped to the ruled
delta list below; unaudited churn is the cost we refuse.

## What dies (deletion inventory)

Kernel:
- The select claims on the identity algebra: per-select state words,
  the per-slot directory chains (`graphix_slot_state_table`,
  `own_levels`, anchor/leaf recursive frees,
  `WrappedKernel::slot_table_words`) — the chains exist only for
  select memory; loop DynCall sites use the key-0 bucket. Site blocks
  survive for DynCall identity only.
- Becoming-selected detection and dampening, woke-forced-FIRED,
  wake-without-refill, the null-site-block quiet read.
- The "no selection memory available → de-fuse" class (coverage goes
  UP).
- `tail_scrut_stale`/`tail_scrut_fired` as special channels; the
  cceb0809 damp.
- The recursion machinery: the per-site scalar-formal memo, wire slot
  3 forwarding/damping.
- The guard-disc non-folding discipline: guard input discs now fold
  into the emission tag. (Implementation option: keep lazy guard VALUE
  evaluation and fold only the guard-input DISCS — the tag needs "did
  a guard input fire", an OR over discs, not the guard bodies. If that
  keeps the schedule-free carve-out observationally equivalent, it
  stays as pure optimization; if not, prologue everywhere and delete
  the carve-out.)

Interp:
- Select's emission logic collapses to: tick guards, match (cached or
  fresh scrutinee), swap sleep/wake on selection change, emit iff any
  input triggered or the arm produced.
- The aug13h entry-args memo on GXLambda (the kernel memo's twin).
- The becoming-selected/wake-refill special emission path (merges into
  the ordinary path).

## What stays (the bottom/ride axis — untouched)

- The scrutinee ride (aug06ghz0), the guard ride, `emit_scrut_ride`,
  the select-miss STALE inheritance (a627b13d): forced by selection
  continuity — you cannot re-match against a bottom.
- The standing-bottoms-never-refire genus, Q1 bottom-propagates, the
  fired×bottom tag algebra, dense delivery R1–R3 wholesale.
- Sleep-is-pause, the SLEEP_RESTARTS interior-sleep gate (about the
  interp's arm sleep, not firing).
- The recursion STRUCTURE rulings: unconditional transient retention,
  no park. Depth trips deliver FreshBottom.
- DynCall site identity (jul23f) — the state channel survives for it.

## Ruled deltas (each becomes a red→green fixture; the re-bless key)

1. Select scrutinee re-fire, same arm → EMITS (was quiet).
2. Guard-dep fire, selection unchanged → EMITS (was quiet).
3. The gating-select idiom (`select enabled {true => data, _ =>
   never()}`) becomes a sampler of its arm per `enabled` delivery;
   `uniq(enabled)` is the documented remedy. Book/examples need a
   chattiness pass.
4. A select whose taken arm is `never()`/bottom emits FreshBottom per
   fired input (was quiet; consistent with ops).
5. Recursive call on fired-same-value args fires:
   `recursion-fires-like-chain/00` flips ([1,2,2] → 3 fires).
6. Tail same-args re-dispatch fires at any iteration count:
   `tail-zero-iteration-quiet/00` and `/01` flip (the aug13h count
   becomes 4, not 2).
7. The const-terminal witnesses (`fuzz/known-kernel-gaps/`): the
   kernel was right; both move to `findings/` as agreement pins and
   the known-kernel-gaps dir empties.
8. Downstream cadence of `count`/`once`/effects over selects increases
   correspondingly (they observe honest deliveries).
9. `~` remains the sampling construct; `select t { _ => v }` is again
   ALSO a sampler — no longer an error of intent, just redundant.

## Migration protocol (one combined change, 5b/5c discipline)

- P0: red fixtures for every delta above (verified to diverge/flip as
  predicted on the pre-change tree); stop the running soak lanes when
  implementation starts (old-semantics findings become noise; the
  bottom-axis coverage resumes with the new tree).
- P1: interp flip (select emission + tail memo deletion + lambda.rs
  simplification). Gate: the differential oracle runs interp-new vs
  kernel-old EXPECTING divergences — every one must map to the delta
  list; off-list = stop the line.
- P2: kernel flip (select emitter deletions, state-channel pruning,
  rec memo/damp deletion, tail channel deletion). Gate: divergence
  diff returns to ∅.
- P3: re-bless — regress corpus + graphix-tests + FuseExpect churn
  (the dead de-fuse class flips None→Jit) + CLAUDE.md rules rewrite +
  the examples chattiness pass. Every changed expectation cites a
  delta number.
- P4: fleet soak on the new tree; the clean-days clock restarts.
