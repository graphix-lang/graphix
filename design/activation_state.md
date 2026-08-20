# Activation state and the bottom-out rule

> **RULED 2026-08-20 (Eric), NOT YET BUILT.** Two rulings derived
> together from the aug18a class-5 finding; they jointly settle it and
> replace its parked fix. This doc holds the derivation, the precise
> rules, the compliance inventory (what each engine already gets
> right, what changes), and the build plan.

## Provenance

aug18a class 5 (katana divergence_000001, minimized in
`fuzz/pending-triage/class5_tail_entry_ride.gx`):

```graphix
{let x = array::iter([i64:1, i64:2, i64:3, i64:0]);
 let m = x / x / i64:3;                       // 0, 0, 0, ⊥ (0/0)
 let rec f = |n: i64| -> i64 select n {
   i64:0 => select i64:0 {i64:0 if m == i64:0 => i64:1, _ => i64:2},
   _ => f(n)};
 f(m)}
```

interp `[1,1,1,1]`, jit `[1,1,1]`. The interp's 4th value comes from
the ride machinery: the standing entry select serves its cached
scrutinee, the tainted guard reads its held selection, and the arm
RE-EMITS 1 on the cycle whose only fresh input was a bottom. The
parked fix direction was to give the kernel matching per-instance
entry-history storage.

Probing the recursion boundary killed that direction. The twin probe
(guard moved to depth 1; tail self-call vs `+ i64:0` forcing native
recursion) showed the interp DISAGREES WITH ITSELF:

```
standing entry (class witness):        [1, 1, 1, 1]   rides
native recursion, depth-1 guard:       [1, 1, 1, 1]   retained instance rides
tail recursion, depth-1 guard:         [1, 1, 1, 2]   fresh frame, tainted guard
                                                      falls through to _
```

Three behaviors for one function, selected by execution strategy.
Each twin's JIT agrees with its own interp, so the differential
oracle is structurally blind to the family (the symmetric-blind-spot
genus). Nobody ruled any of this — it is all machinery accident:
whether you get cross-cycle ride state depends on whether your select
node happened to be a never-framed standing body, a retained
per-depth instance, or a per-pass-reset frame.

## The adjudication

Eric's reading, as a normal programmer: x updates 4 times, m updates
3 times and bottoms on cycle 4 — why should f produce 4 values? And:
the fact that the inline interpretation of recursion is UNBUILDABLE
on the value plane (per-depth cross-cycle history forfeits
constant-space tail loops, guaranteed by `atomic_recursion.md`) is
evidence the interpretation is wrong there.

Checking the ride's two motivating findings confirms neither ever
demanded value manufacture:

- `findings/select-quiet-scrutinee-aug2026` (aug06ghz0, the scrutinee
  ride): the demanded emissions are "the FRESH values (0 then 7 —
  **body production, not a ride**)". The ride exists so the selection
  survives a bottomed delivery and the arm body keeps firing on its
  OWN deps against cached operands.
- `findings/guard-bottom-ride-aug2026` (aug13b, the guard ride): the
  ruling verb is "hold its standing selection **QUIET**" — the pinned
  bug was a phantom selection FLIP, not a missing re-emission.

The 4th value is an emergent COMPOSITION nobody ruled: organic firing
(fire per guard production) × ride substitution (held guard replaces
the bottomed one) = a delivered bottom laundered into a sound
emission.

Method note (recurring — same shape as the sync-subset unwind): when
the implementation fights a semantic model this hard, suspect the
model before building the machinery.

## Ruling 1 — THE BOTTOM-OUT RULE (value plane)

**Held/ride state serves selection survival, re-matching, and operand
service — NEVER the cycle's output bottomness. A node's production
OR-joins the bottomness of every delivery it consumed this cycle.
Bottom in, bottom out.**

Precisely:

- The FIRED plane is untouched. Organic firing stands: the select
  still fires on a bottomed guard/scrutinee delivery — its production
  is FreshBottom (consistent with organic delta 4). The interp's
  class-5 trace loses its 4th VALUE, not its 4th fire.
- The value channel this cycle is what was DELIVERED this cycle
  (fresh or stale, including bottoms). Reading a standing SOUND stale
  value is legitimate — that is what stale means, and what `~` does.
  What is banned is a ride cache OVERWRITING a delivered bottom for
  emission purposes.
- Bottoms are therefore STICKY on the value plane: a standing bottom
  (StaleBottom re-delivery) keeps the channel valueless until a sound
  delivery replaces it — consistent with `Rt::store_value`'s
  bottom ⇒ None and the dense model. A function of a valueless input
  is valueless. **`hold` is the explicit recovery tool** — riding
  over bottoms is what `hold` is FOR, exactly as `uniq`/`filter`/`~`
  are the explicit tools on the fired plane. The compiler never does
  it silently. (This is the value-plane twin of the organic-firing
  philosophy.)
- "Consumed" for a select: the scrutinee delivery, every guard the
  match chain evaluated (arms above and including the taken one), and
  the taken arm's production. Deliberate harsh case: a bottomed guard
  on a NON-taken arm above the taken one poisons the cycle's emission
  even though the selection (via held re-match) and the taken arm's
  value are sound — the selection DECISION consumed a bottom.
  Sleeping untaken arms below are not consumed.
- A tainted guard is UNKNOWN, not false. The probe's tail twin taking
  `_` on a tainted guard (`[1,1,1,2]`) invented a selection flip from
  a bottom — wrong under this rule. Selection HOLDS (or, with no
  held selection, no arm becomes selected); the production bottoms
  either way.
- What SURVIVES of the ride rulings: everything their pins actually
  demand. Selection survival (no teardown, no phantom flip on a
  bottomed delivery), cached-operand service (pattern binds serve the
  cached scrutinee so the arm body's OWN fresh fires emit — the
  aug06ghz0 emissions are body productions and are unchanged), and
  the sleep/wake routing the held selection drives. aug06ghz0/aug13b
  are NARROWED to this, not repealed.

Convergence evidence: under this rule all four probe shapes — the
class witness, the drop-the-`rec` twin, and both depth-1 twins —
produce the identical trace `[1,1,1]` + FreshBottom, on both engines.
No shape-dependence remains.

## Ruling 2 — STATE MULTIPLICITY = ACTIVATION MULTIPLICITY

**State has the multiplicity of activations. Non-tail recursion
creates an activation per level — full inlining, lazily materialized;
each level maintains its own state, standing across cycles. A tail
loop is ONE activation whose iterations reuse its one state — a tail
call does not create an activation, it REPLACES the current one.
Collection slots are each an activation.**

This is the Scheme move extended from space to state, and it is
FORCED, not chosen: inlining semantics for general recursion plus
constant-space tail loops are jointly incompatible with per-depth
history, so tail position must be semantically meaningful for state.
The programmer can see syntactically which they wrote.

Corollaries, previously ruled piecemeal:

- The connect identity law (aug18a class 3, e05a6c8b): "a connect
  target's identity has the multiplicity of its interp binds" —
  per-slot in collection callbacks, per-activation in recursion, one
  reused cell for a tail loop's lifted counter.
- MapQ/FoldQ per-slot live instances (`collection_intrinsics.md`).
- The retention ruling (2026-08-13): retained per-depth instances ARE
  the inlining's standing nodes, materialized on demand.

Consequences:

- Tail and non-tail twins of a STATE-carrying body legitimately
  differ (a `count()` in a tail body counts iterations across the
  loop's whole life; in a native body each depth has its own
  counter). This is ruled semantics, by design — do not read
  tail-vs-native agreement as an invariant for state-carrying bodies.
  (The class-5 probe divergence was on the ride/VALUE face, which
  Ruling 1 removes; post-amendment the bottom cases converge.)
- SCOPE: the tail clause covers SELF tail calls — what the loop
  machinery catches. Mutual tail recursion (`f`→`g`→`f`, all in tail
  position) falls to the non-tail clause: per-level, retained, NOT
  constant-space. A narrower guarantee than Scheme's all-tail-calls;
  deliberate, and predictable from the source.
- Selection/held caches under Ruling 1 only do routing and operand
  service, so per-pass frame clearing vs "one reused state" is nearly
  unobservable; the residue (sleep/wake routing off the held
  selection) takes the rule's answer directly: the loop's one
  selection is whatever the last iteration left.

## Compliance inventory

Already correct (no change):

- interp non-tail builtin/connect state: per-depth retained
  instances. ✓ Ruling 2.
- interp tail semantic state: standing body nodes shared across
  passes (`reset_replay` clears only replay caches). ✓ Ruling 2.
- kernel tail: per-site DynCall state words, one per instance; the
  lifted counter's one cell. ✓ Ruling 2.
- kernel tail-position select storage refusals (the jul10h severing
  rule): produce bottom on tainted entry — under Ruling 1 this is now
  simply CORRECT, not an approximation. The class-5 kernel trace
  `[1,1,1]` is the ruled trace.

Changes required:

1. **interp: delete the ride re-emission face.** A select whose
   consumed delivery bottomed produces FreshBottom; selection holds;
   cached-operand service and body-driven fires unchanged. Sites:
   the `ride` path in `node/select.rs` (emission), the guard held
   read in `node/pattern.rs` (`is_match`'s Held serve — keep for
   re-match, stop it determining emission soundness).
2. **interp: tainted-guard-no-history must not take the wildcard**
   (the `[1,1,1,2]` fall-through in framed passes). Unknown ≠ false:
   no selection flip, production bottoms.
3. **kernel: value-position ride sites** (`emit_scrut_ride`, the
   guard-prologue taint cache in `fusion/emit/select.rs`): KEEP the
   substitution for re-match/routing, change the emission fold so a
   consumed bottomed delivery taints the production. Same fix as (2)
   where the kernel's masked-to-false guard path invents `_`.
4. **kernel: the key-0 shared legacy bucket for recursive back-edges**
   (`fusion/emit/call.rs`) violates Ruling 2 for native recursion —
   a stateful builtin below a recursive back-edge shares one state
   across all depths where the interp keeps per-depth state. Fix:
   per-depth state or de-fuse. Gets its own finding dir when built.
5. **Pin re-adjudication.** Expected traces containing MANUFACTURED
   values (a value emitted on a cycle whose only fresh input was a
   bottom) flip to one-fewer-value; expected traces whose emissions
   are body productions survive untouched. Review at minimum:
   `select-quiet-scrutinee-aug2026` (survives — body productions),
   `guard-bottom-ride-aug2026` (survives — already "quiet"),
   `dyncall-loop-stale-ride-aug2026`,
   `select-merge-taint-ride-aug2026`, the jul30a re-woken-arm ride
   delta, `array::window`-on-absent pins, and the dense/organic
   ruled-delta lists in `dense_delivery.md`/`organic_firing.md`.
6. **Docs.** Narrow the SCRUTINEE RIDE and guard-ride paragraphs in
   CLAUDE.md; annotate the superseded deltas in the two design docs;
   fold the class-5 dossier (`fuzz/pending-triage/`) into the
   findings dirs as red→green pins when built.

Dead: the parked class-5 fix (per-instance entry-history storage,
first-pass ride flags, guard-held words in tail bodies). The kernel
was right; the interp sheds machinery instead.

Untouched: aug18a class 4 (slot-state chain ensures — a different
chapter), class 6 (diagnostic-only).

## Build plan

- P0: land this doc; commit the probe twins beside the class-5
  dossier as red fixtures with expected ruled traces.
- P1: interp amendment (changes 1+2), red→green on the probes; full
  corpus run, re-adjudicate flips (change 5).
- P2: kernel value-position emission fold (change 3), differential
  green.
- P3: key-0 back-edge finding + fix-or-defuse (change 4).
- P4: doc narrowing (change 6); soak on the amended binary.
