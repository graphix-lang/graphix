# Wake catch-up — tracked fires + forced recompute at arm wake

Status: RULED by Eric 2026-09-01, BUILT the same day (both engines;
as-built record at the end — one design deviation: the kernel carries
NO mask words, see §as-built). Gates at build time: the 8-pin
`findings/wake-catchup-sep2026/` corpus + the re-adjudicated
`dyncall-arm-init-stale-aug2026` pin, full findings regress,
graphix-tests. FLEET SOAK REQUIRED before this is called landed.

Originally: RULED by Eric 2026-09-01, NOT BUILT. The conversation arc: the
aug31e corpus regression (`dyncall-arm-init-stale-aug2026/00` diverging
after the wake-delivers-present-but-stale commits) → stateless-re-eval
proposal → the `subscribe` hole → value-diff proposal → rejected
("the same value fired can carry just as much meaning as a differing
value") → "reselection is the clock" → the modal collision → per-input
tracked fire bits with once-per-select consumption → the `publish`
probe (forced recompute is required, tracking alone is not enough) →
the 43/2/21/62 table → the mechanics (the op-node ride-skip, the
`CachedArgs` wrapper, node-local slept bits). Supersedes the aug08b
arm-init fired view as semantics; REFINES, does not revert, the
present-but-stale ruling (9b2e7231/a4f69e8e).

## The hole

**Sleep is what breaks the ride-skip's invariant.** Under dense
delivery a node skips recomputation when no consumed input triggers
(`op.rs` R1: `if !(trig || resident_bottom || framed) { ride }`),
which is sound because an AWAKE node's stale input value cannot differ
from what its resident was computed from — values change only by
firing, and an awake node saw every fire. A sleeping arm does not.
While it sleeps, its inputs' values drift behind its back; its first
update after wake delivers present values under stale tags, the skip
takes, and the arm surfaces a product computed from a world that no
longer exists.

This is why the seam flip-flopped for a month, each fix moving which
engine was wrong because the semantics was genuinely unruled:

- `dyncall-loop-stale-ride`, then aug08b (`dyncall-arm-init-stale-
  aug2026`): the JIT rode a pre-sleep product, the interp recomputed →
  the kernel's arm-init was forced to deliver everything FIRED.
- 9b2e7231 (the admin-TUI phantom submit): a fired wake delivery
  re-raised a consumed Enter into a freshly woken modal arm → wake
  reads went present-but-stale. Correct for edges — but now the
  INTERP rode (nothing fired, `CachedArgs` never re-evals) while the
  kernel still recomputed: the same pin diverged with the engines
  swapped. From the user's point of view the ride is wrong (Eric):
  the arm displays data derived from values the whole graph has moved
  past.

The two eras were each half right. Fired-at-wake has correct values
and re-raises events; stale-at-wake re-raises nothing and shows stale
values. The rule below separates the two channels.

## The rule (Eric, 2026-09-01)

1. **Every select tracks one fire bit per arm input** — the union of
   its arm BODIES' free refs (binds defined outside the arm; pattern
   binds and arm locals are internal). Bits are set when a tracked
   input fires, and the tracking runs whatever the selection state,
   including no-arm-selected windows (bottom scrutinee, init-phantom
   guards). Bits OR-accumulate: there is no event queue — a bit plus
   the bind's current standing value, so N fires during a sleep
   conflate to ONE catch-up fire at the latest value (`queue` stays
   the lossless tool).
2. **Cached fire state is consumed by evaluation.** Whichever arm
   evaluation reads an input clears its bit — the live selected arm
   consuming same-cycle as the degenerate case (awake behavior is
   unchanged by construction), a woken arm consuming at catch-up.
   Delivery is consumption whether or not the arm's interior does
   anything with it (a `~` gate dropping it does not un-consume it).
   An arm that doesn't read an input leaves its bit standing for a
   future waker; two sleeping readers wake in sequence → the first
   consumes, the second reads stale. **At most once per select.**
3. **A woken node's first update recomputes from present values.**
   `sleep()` already reaches every node in the arm (through
   `CallSite` into instantiated callee bodies), so each node sets a
   local `slept` bit there and its next update refuses the ride-skip,
   recomputing from its children's present values and clearing the
   bit. No global flag, no event plumbing — the bit is definitionally
   co-extensive with the soundness hole: a node that never sleeps
   never drifts and never pays.
4. **Tags stay honest.** A tracked bit delivers FIRED (conflated to
   the current standing value); everything else delivers
   present-but-stale; recomputed products tag by the OR of their
   inputs' fired bits. So edge consumers (`~`, connects,
   accumulators, callback dispatch) tick exactly for the events no
   selected reader saw — once — and nothing consumed is ever
   re-raised. Eval re-runs at wake only where it is a pure function
   of its argument slots (§ mechanics).

The invariant, user-facing: **a select's output is always a function
of the present world; events influence it exactly once.**

Out of scope, unchanged: the scrutinee and guards (their own consult
machinery — the aug03 guard-flip FIRED, the stale-first-consult
routing, a4f69e8e); the bottom-scrutinee bottom-out rule; SLEEP_RESTARTS
builtins (restart on sleep as today). Nesting needs no extra
machinery: an arm's ref set includes everything under it, so a select
sleeping inside an outer arm has its missed window covered by the
OUTER select's bits; the wake evaluation carries the tags down, the
inner select routes and consumes with them, and its own tracking
resumes.

## Worked examples

**Eric's table** — `select cond { true => in0 + 1, false => in0 + 42 }`,
`in0` init 1, `cond` init false:

| event | woken arm sees | output |
|---|---|---|
| init | everything fires (genuine init) | 43 |
| cond→true | `in0` bit consumed at init by false arm → stale 1; recompute | 2 |
| in0→20 | live fired delivery | 21 |
| cond→false | bit consumed by true arm → stale 20; recompute | **62, never 43** |

43 at the last step is the ride — the answer this rule exists to
forbid. Note step 2: nothing in the arm fired, yet the select emits —
organically, because the consulted scrutinee fired.

**The modal** (`lib_tests/callable.rs`
`arm_wake_delivers_standing_args_stale`): `e = "x"` fired while `` `A ``
was selected and `` `A `` reads `e` (`e ~ 1`) — bit consumed. `` `B ``
wakes with `e` stale, `t ~ (submitted + 1)` does not tick, no phantom
submit. The 9b2e7231 ruling stands.

**The fork witness** (`dyncall-arm-init-stale-aug2026/00`): `in1`
fired while the OTHER arm (`[0, 1]`, which does not read `in1`) was
selected — bit survives — so the woken map arm catches up with a
genuine fire: `1, 0, 1, 1` on BOTH engines. The pin re-points from
divergence to agreement.

**Shared-input effect** — `select cond { true => publish(p1, v),
false => publish(p2, v) }`: `v` is read by both arms, so its fires
are always consumed by whichever arm is awake; the woken arm gets
ZERO catch-up fires. Value correctness cannot come from fires here —
this is the probe that proved tracking alone insufficient. Forced
recompute republishes `p2` at the present `v`; a pure Graphix
function in place of `publish` recomputes identically (tag-blindness
stops being a lucky accident — a pure function's value derivation
depends on values, not tags, by construction). The sleeping arm's
STANDING publication (sleep is pause, not teardown, so `p1` stays
published) is a separate publish/sleep-contract question, noted, not
ruled here.

## Rejected alternatives

- **Ride is the rule** (uniform stale, no recompute — the interp's
  post-9b2e7231 accident): user-visible staleness; 43 instead of 62;
  `subscribe` stuck on a path the graph moved past.
- **Fire everything at wake** (aug08b): the fired bit IS the edge
  semantics — this re-raises consumed events (the modal), or demands
  a distinguishable "soft fire" that every edge consumer and every
  future builtin author must learn to ignore, the tag-blind ones by
  definition can't, an open-set tax versus the closed set of skip
  sites we own.
- **Value-diff gating** (re-eval iff a stale value differs from the
  cached copy): an invisible `uniq` at every wake seam; "the same
  value fired can carry just as much meaning as a differing value."
- **Stateless-only re-eval**: `subscribe` is stateful and stays on
  the wrong path; `print` is STATELESS and would be mis-framed as a
  flaw. Statelessness is the wrong axis — the right question is which
  fires the arm missed, and that cannot be reconstructed at wake, so
  it must be tracked.
- **Per-arm independent tracking** (each arm catches up on everything
  it missed): re-phantoms the modal — `e` fired during `` `B ``'s
  sleep, so `` `B `` would catch it up even though `` `A `` consumed it.
  Consumption must be per select, not per arm.
- **Traditional always-computing dataflow** (no firing at all — the
  graph always computes, consumers pull): the compiler gets much
  simpler, and input is tolerable (read bottom until something
  happens), but OUTPUT is a nightmare — the naive `print(42)` prints
  forever, and the whole graph is a busy loop doing useless work.
  Edge-triggered semantics with this one bounded repair at the sleep
  seam is the better trade (Eric, 2026-09-01).

## Mechanics

### Interp

- **`node/select.rs`**: the tracked set derives from the arms'
  `Refs` at compile time (referenced minus bound-within-arm). The
  select registers wake interest in the tracked BindIds so fires
  reach its tracker even while every reading arm sleeps — sleep's "no
  computation" is preserved (a bit set is O(1), no arm node runs);
  "subscriptions paused" now excludes this one standing interest per
  tracked bind. Bits live beside selection memory: semantic state,
  survives sleep, cleared only by consumption. After each cycle's arm
  evaluation, clear bits ∩ the taken arm's read set. Note the bits
  only ever record fires that HAPPENED in the awake graph: a sleeping
  async producer (a paused `subscribe`) produces nothing, so there is
  nothing to catch up — it resumes producing on wake as today.
- **Wake delivery**: per-input tags replace the uniform stale view —
  bit set → FIRED at the current standing value; clear → STALE.
- **The ride-skip** (`op.rs` and the other resident skips): a fourth
  disjunct, `self.slept`, set in `sleep()`, cleared on the recompute.
  The recompute path already reads present child values
  (`op.rs:130`); frames prove the pattern — `frame_depth > 0` has
  forced recomputation through stale values since dense delivery.
- **The builtin wrapper** (`CachedArgs::update_inner`,
  package-core): `Apply::sleep` sets the bit. `CachedVals` already
  refreshes slot VALUES on stale deliveries (lib.rs:404); on the
  first post-sleep update with an all-stale production:
  `STATELESS = true` → re-run eval from the refreshed slots, result
  STALE — extending the existing phantom rule
  (`arm-local-bind-aug2026/03`, "a value rule and not a firing one")
  from first-production to wake. `STATELESS = false` → today's
  retag: an accumulator's resident IS its state, already the correct
  present value; its edge catch-up arrives separately as a tracked
  FIRED through the normal eval path, added exactly once. This is
  STATELESS's real meaning here: "eval is a pure function of the
  slots, safe to re-run for value derivation" — audit the ~90
  declarations against that reading (FASTCALL already demanded a
  compatible one). The async wrapper gets the same wake dispatch
  (tag-blind level reconcile: `publish`, `subscribe`).

### Kernel

- **Mask words in select state**: OR-in the invocation's fired discs
  for tracked inputs before the chain runs; clear the taken arm's
  read set after its evaluation. Semantic state like selection
  memory — survives sleep, never reset by frames.
- **Arm wake**: per-input discs from the mask words replace the
  aug08b blanket stale-mask suppression, which dies. Pure emitted ops
  compute-always — the forced recompute is free.
- **DynCall**: the site's state block takes the slept treatment keyed
  on selection change; the inner `Apply` behind the dispatcher shares
  the wrapper implementation above — one implementation, both
  engines. This also closes the standing kernel hole where a fused
  stateful builtin (`sum` in an arm) re-accumulates at wake off the
  fired arm-init view.
- A shape that cannot carry its mask words de-fuses. A fusion bug may
  lose fusion, never produce a wrong answer.
- The uncommitted seam A/B patches (the `emit_ref_node` honest disc,
  the raw-init stale-mask keying) are partially superseded —
  re-derive both under this doc during implementation.

### What stays

Present-but-stale for untracked standing reads; genuine init
(`init && !wake_init`) still upgrades; constants fire at init only;
the guard rules; `ByRef`'s stale seed; sleep-is-pause value retention
(residents survive sleep — they are now REFRESHED at wake rather than
surfaced); shrink-is-delete for recursion (a fresh activation has
phantom residents and takes the ordinary first-production path, no
interaction with slept bits).

## Testing and rollout

Pins (red→green, `run!` + schedule format): Eric's table verbatim
(43/2/21/62); the modal pin stays green untouched; the fork witness
re-pointed to agreement at `1,0,1,1`; the shared-input pure-function
shape; sequential wakers (first consumes, second stale); the
no-arm-selected accumulation window; `sum`-in-arm catch-up (added
exactly once); `~`-in-arm catch-up tick. Then the full gates (bare
`cargo test` at the root, release `regress` + `selfcheck` +
`detcheck`), leakcheck if the select grows owned state (mask words
are scalars — should not), and a FLEET SOAK before this is called
landed — gates are not the fuzzer, and this is exactly the class of
change the differential oracle exists for. The soak folds into the
blocked aug31f deploy: the fleet goes out on the tree that implements
this and hammers it.

## As built (2026-09-01, same day)

The rule stack landed as ruled; the pins live in
`findings/wake-catchup-sep2026/00–06` (the table, shared-input spent,
sequential wakers, conflation, `~` catch-up, the no-arm window,
nested composition), all AGREE with the ruled traces, and
`dyncall-arm-init-stale-aug2026/00` is re-adjudicated in place
(header updated). Mechanism deltas discovered during the build:

- **Interp, as designed**: the `Node` funnel carries the slept bit
  (`sleep()` sets, the next `update` scopes it into `ExecCtx::woke`);
  `ExecCtx::recompute_forced()` (= framed ∨ woke) is the one skip
  condition — `dense_gate!`, the three op.rs macros, StringInterp.
  `Not`/`Neg`/`TypeCast`/`Block`/`StructRef`/`Construct` already
  recompute unconditionally; `Any` and `~` ride correctly (they ARE
  edge state); `Constant` keeps its fires-at-wake behavior (both
  engines agreed on it before and after). The select tracker is one
  `pending` set + per-arm free-ref read sets (`Refs` minus pattern
  binds), REFRESHED AT EACH DESELECT — compile-time refs cannot see
  through a lambda literal into an instantiated body, deselect-time
  refs can. Catch-up delivery is an `event.variables` injection
  scoped to exactly the arm's evaluation.
- **Value-channel refresh seams the ruling implied but the doc
  didn't name**: a wake's recomputed STALE values must re-reach the
  store, or readers downstream of a publish seam still ride —
  `Bind::update` re-publishes a quiet wake production (`<-` targets
  still held back), `CallSite::update_call` refreshes its arg ids'
  standing entries, `GXLambda::update` re-seeds formals, and MapQ
  rebuilds its collection from the refreshed slots (FoldQ's acc
  chain already flowed through the stale channel).
- **Kernel: NO mask words** (the one design deviation). Wire slot 0
  grew bit 2 (WAKE = `event.wake_init` ∨ the kernel node's own woke,
  depth 0 only); the DynCall stale-mask suppression keys on
  `bit0 & !bit2` (genuine init); `DynCallSlot::dispatch`'s
  first-dispatch arrival upgrade is likewise genuine-only (a first
  dispatch AT A WAKE reads standing args stale — R2's rule at the
  site seam; pin 02's kernel `count` re-counted through both of
  these); an in-kernel becoming-selected arm hints the dispatcher
  (`graphix_wake_hint` → scoped `ctx.woke`) so the shared
  `CachedArgs` wrapper is the ONE implementation of the STATELESS
  wake re-eval on both engines. What replaces the mask words: pure
  arms recompute anyway (kernels compute-always), and every
  edge-consuming arm interior either de-fuses or sits under an
  interp select whose tracker injects THROUGH the kernel boundary
  (an arm-position kernel's params read the injected fires — pin 06
  proves the composition). The enforcement is the WIDENED interior
  gate: `has_restart_reach` (sleep-restarting, refused in ANY arm
  extent — P7 as before) now has a companion `has_stateful_reach`
  (stateful non-restart, refused in VALUE-POSITION arm extents
  only, transitively through callees and self back-edges).
  Tail-position arms are exempt — they wake only through
  frames/activations, where the mechanism is excluded (frames
  guard every wake predicate: `wake_recompute()` is depth-0 only,
  which is also what keeps the frame-formal-init-view FIRED overlay
  seed intact) and per-activation site blocks are the correct twin
  (`tail_stateful_scalar` keeps fusing).
- **Coverage residue**: stateful (non-restart) builtins —
  `sum`/`max`/`min`/`mean`/`product`-class — inside VALUE-position
  select arms de-fuse the region. In-kernel mask words (this doc's
  original mechanism) remain the path to reclaiming that coverage
  if it ever matters; the semantics would be unchanged.
