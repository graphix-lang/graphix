# Activation state and the bottom-out rule

> **RULED 2026-08-20 (Eric); Ruling 1 BUILT same day (interp + kernel,
> P0–P2).** Two rulings derived together from the aug18a class-5
> finding; they jointly settle it and replace its parked fix. This doc
> holds the derivation, the precise rules, the compliance inventory,
> and the as-built record. Open: the key-0 back-edge chapter (P3) and
> the named mid-loop residue at the end.

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
- **The per-fire formulation** (the buildable statement, refined at
  build time — the first draft's unconditional "any consumed bottom
  poisons" contradicted aug06ghz0's survival by treating guards and
  scrutinees asymmetrically): a select's emission is the taken arm's
  current production, re-tagged fresh, whenever any consumed input
  fires SOUND (a sound scrutinee delivery, a sound guard production,
  or the arm's own sound body fire — sound beats bottom within one
  select's scope); when EVERY fired consumed input this cycle is a
  bottom, the emission is FreshBottom regardless of the arm's
  standing value. "Consumed" = the scrutinee delivery, every armed
  guard's production (both engines tick ALL guards every cycle —
  jul19b / the kernel prologue — so a below-the-taken-arm guard's
  fire is a consumed fire too, sound or bottom), and the taken arm's
  production. Sleeping arm BODIES are not consumed. Neither are the
  guards of a NO-HISTORY bottom select: with no value view there is
  no chain to consult, so only the scrutinee delivery is consumed
  and the settled bottom stays QUIET on unrelated guard fires — the
  aug13l select-miss-standing-fresh ruling holds (the first build
  fired that early return on guard fires and re-created aug13l on
  the interp side; the corpus caught it).
- **THE INIT-PHANTOM GUARD** (surfaced by the fixture gate): a guard
  that has NEVER produced — its deps deliver after init — is the
  same knowledge state as a bottomed guard, so it is UNKNOWN too.
  The old engines read it false (`unwrap_or(false)` / the kernel
  mask) and took the wildcard at init — an invented selection. Now
  the select bottoms until the guard first becomes evaluable: a
  guarded select loses its init emission when the guard's deps
  deliver post-init (organic delta 2's counts drop by one; 16
  fixtures re-blessed). The explicit tool for a startup default is
  initializing the guard's source (`let enabled = false;
  enabled <- …`), not a silent invented false.
- A tainted guard is UNKNOWN, not false. The probe's tail twin taking
  `_` on a tainted guard (`[1,1,1,2]`) invented a selection flip from
  a bottom — wrong under this rule. With held history the selection
  rides (routing only); with NO history the chain is UNDETERMINED at
  that arm: it stops, no selection is recorded, no arm body runs, and
  the production bottoms (fresh iff anything fired this cycle).
- **Nesting composes through arm productions**: an inner select whose
  only fires were bottoms emits FreshBottom, which the outer select
  consumes as a fired-bottom arm production — the outer's own sound
  fires do NOT resurrect it (its value chain IS the bottom). In the
  kernel's flattened tail spine this required a compile-time scope
  stack (`LowerCtx::sel_fires`, applied innermost-first at every
  `emit_kernel_return`) because the single loop-carried accumulator
  conflated the two selects' scopes — the outer scrutinee's sound
  fire upgraded a result the inner guard's bottom should have
  poisoned. Bottom fires are per-CURRENT-iteration by construction
  (SSA values recompute each loop pass), never loop-carried; the
  cross-iteration sound accumulator (jul21g) is untouched.
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

## Ruling 1a — THE CONSULTED-GUARD RULE (Eric, 2026-08-20, built same day)

**A select consults arms top-down: structure/type first, guard
second. A consulted guard whose CURRENT channel is bottom makes the
selection UNDECIDABLE — the chain stops (no flip, no wake, no arm
body; selection state holds) and the select bottoms, whatever else
fired. Guards of structure-failed arms and of arms below the stop or
take point are IRRELEVANT — they neither fire nor bottom the select.**

Eric's framing: `select a { [x, y] => x + y, [x, y, tail..] if
x / y == 0 => f(tail), _ => 42 }` — with y = 0 the select bottoms
UNLESS the array is a pair; a normal programmer expects arm 1's match
to make arm 2's guard irrelevant. "If the programmer has guards, and
they bottom, it makes the select undecidable when it happens — we
have to bottom."

Refinements over the morning's build:

- **"Consumed" is chain-scoped**, restoring the first draft's reading
  and correcting the all-armed-guards over-breadth: guards tick every
  cycle (jul19b — evaluation), but only CONSULTED productions are
  consumed, on both planes (organic delta 2's guard-fire emission
  narrows to consulted guards; the existing fixtures' guards are all
  always-consulted, so nothing re-blessed).
- **Sound-beats-bottom no longer applies to guards.** A sound
  scrutinee fire cannot rescue a consulted bottom guard by riding a
  held verdict — a previous delivery's verdict cannot route this one
  (the same argument that killed the mid-loop cross-iteration ride,
  applied uniformly). The scrutinee axis is untouched: aug06ghz0's
  selection survival + body-driven fires under a bottomed scrutinee
  stand, and a ridden fresh-bottom scrutinee still emits FreshBottom
  when nothing sound fired.
- **The guard-ride machinery is DELETED, both engines** — the interp's
  held-bool serve in `arm_match` (né `is_match`) and the kernel's
  guard-prologue taint cache. Selection survival across a guard
  bottom is the chain-stop itself (selection state untouched), not a
  ridden verdict. This supersedes the aug13b ride MECHANISM; the
  pin's observables (no phantom wildcard flip, no manufactured
  count) are preserved exactly. The depth-trip `guard_ride_blocked`
  bit went with it (a bottom-channel guard now stops the chain —
  strictly more aligned with whole-derivation-bottoms than the old
  local false-read).
- **A standing consulted bottom keeps the select bottom** until the
  guard recovers — the interp stores the consulted mask from the last
  re-match (`Select::consulted`) so quiet cycles honor it; the kernel
  re-runs its chain every invocation and needs no memory. The taken
  path needs no bottom override at all: a taken arm's consulted
  guards are all sound by construction.

**Item 2 (the mid-loop guard-bottom residue) DISSOLVES**: with no
verdict-serving anywhere, a mid-loop iteration's bottom guard bottoms
the derivation on both engines — no cross-iteration ride to sever, no
loop-head cache zeroing. The tail and native twins of the witness
shape now agree at `[101, 1]` (w14/w15), and Eric's example behaves
verbatim: the pair emits 3, the triple bottoms — both engines.

Kernel shape (fusion/emit/select.rs): the prologue keeps per-guard
evaluation and stores `(eff, gbot, gs_sound, gfire)`; the chain folds
the sound/fired planes into cranelift Variables AT each consultation
point (control flow scopes them for free) and branches
bottom-channel guards to the shared undet block, whose freshness
reads the fired-plane accumulator. `SelFires.bfired` carries only the
scrutinee axis now.

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

Changes (1–3 BUILT 2026-08-20; 4 open):

1. **interp: the ride re-emission face deleted** (node/select.rs):
   the fired plane split into `own_sound`/`own_bottom`
   (`PatternNode::update` returns the guard's production tag), the
   `emit!` fold emits FreshBottom on all-bottom fires, the
   no-history early return fires on guard fires too, and
   `tail_scrut_fired` folds SOUND fires only.
2. **interp: three-valued `is_match`** (node/pattern.rs):
   `Option<bool>` — a bottomed guard with no held bool is `None`
   (undetermined); the select's chain stops there with no flip, no
   wake, no sleep, and a bottom production (fresh iff anything
   fired). The depth-trip `guard_ride_blocked` read stays the ruled
   FALSE.
3. **kernel: the emission folds mirror it**
   (fusion/emit/{select,flow,body,lower}.rs): the guard prologue
   splits sound-plane stales (a tainted production reads quiet —
   `TAINT >> 1 == STALE`) from fresh-bottom fires; `SelFires`
   carries both to every arm emitter; the pre-ride scrutinee
   bottomness is captured before `emit_scrut_ride`; a
   post-ride-still-tainted guard routes the chain to a shared
   UNDETERMINED block (per-shape bottom via
   `emit_select_bottom_value`, no record, no arm body); the
   value-position merge fold and `emit_kernel_return` (via the
   `sel_fires` scope stack, innermost-first) turn a still-stale
   result with a fresh-bottom consumed fire into TAINT fresh, the
   payload untouched (valid under TAINT, ownership exact).
4. **kernel: recursive back-edge state — AUDITED 2026-08-20; the tree
   was already built, and the audit found + fixed a REAL adjacent
   hole.** The per-depth mechanism landed 2026-08-16 (003fa7d6, "give
   a recursive activation its own memory"): a self-call roots a
   lazily-grown per-ACTIVATION block tree (`graphix_site_child_block`;
   size read at runtime from the callee's `site_desc` cell — a
   self-call's own layout isn't final mid-emission; one root per
   self-call SITE so sibling calls get separate trees;
   `free_self_block_tree`/`reset_self_block_tree` walk it; honor
   headers ride down). The doc's original "key-0 bucket" framing was
   stale comments plus a probe misread (the depth-1 substitution was
   depth-1's OWN child-block history — correct behavior).

   Audit results:
   - RIDE/routing state per depth: real and pinned (003fa7d6's own
     pins 00–02 + the bottom-out probe family).
   - BUILTIN (SLEEP_RESTARTS) state per depth: structurally
     UNREACHABLE in kernels — recursion dispatches through a select,
     so every stateful builtin in a rec body sits under an arm, and
     the P7 interior-sleep gate (direct `arm_depth` check + the
     deferred `self_backedge_in_arm && saw_restart_reach` check)
     de-fuses every such shape LOUDLY. Safe by construction; coverage
     cost only. Pins 03/04 carry the interp multiplicity contract and
     become kernel pins if the gate is ever relaxed (their first
     drafts claimed kernel evidence — vacuous, headers corrected).
   - Every degrade door is closed: a callback-mediated in-loop
     self-call is a MUTUAL static edge (loud de-fuse at the
     CallSite), passing the rec fn as its own callback is an
     occurs-check type error, aliased self-calls (`let g = f`)
     de-fuse undiscovered, mutual `let rec` is inexpressible, and
     `emit_site_block`'s silent-0 fallbacks now Err (a future shape
     that reaches them loses fusion, never correctness).
   - **The REAL hole the audit flushed out (via the loud Err failing
     the dyncall_seed_backedge fixture): kernel DEFINITION order.**
     Reverse-declaration-order approximated callees-first and broke
     on SIBLING discovery — a callee discovered before its caller at
     the same scan level (`g(..) ..; f calls g`) defined AFTER it,
     the caller found no `SiteLayout`, and the old silent 0 ran the
     callee's activations below the recursion with no interior
     memory. Red witness: interp `[101,1,1,1]` vs jit `[101,1]` (the
     callee's guard ride never stored; bottoms where the interp's
     retained instance rode). Fixed with a TOPOLOGICAL definition
     order over the recorded static call edges (emit/jit.rs,
     deterministic: declaration-position-sorted edges, DFS
     postorder). Pin: recursive-activation-blocks-aug2026/05.
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

## Build record / remaining

- P0 ✅ doc + probe twins committed (7a74fe00).
- P1 ✅ interp amendment — all four probe shapes converge on the
  ruled `[1,1,1]` + FreshBottom.
- P2 ✅ kernel emission folds — differential AGREE across the probe
  family. Two build-time findings folded back into Ruling 1's fine
  print: the per-fire formulation (sound beats bottom within one
  scope) and the nesting/scope-stack composition.
- P3 ✅ back-edge audit (change 4): the tree was already built
  (003fa7d6); degrade doors verified closed and made loud; the audit
  flushed out and fixed the forward-edge DEFINITION-ORDER hole
  (topological order, pin 05); corpus 417.
- P3a ✅ THE CONSULTED-GUARD RULE (Ruling 1a) built both engines —
  guard rides deleted, chain-scoped consumption, undecidable-bottoms;
  item 2 dissolved; pin 05 re-blessed to [101,1]; gates green with
  zero fixture flips.
- P3b ✅ THE SHRINK-TO-ZERO RULE (aug18a class 4 — Ruling 2's
  slots-are-activations enforced in kernels): every in-loop state
  chain (DynCall cache pairs, per-slot callee site blocks,
  select-table directory levels) re-ensures in each enclosing loop's
  ALWAYS-EXECUTED exit block (`TruncRec` records, close-time stash →
  `emit_slot_truncates`, propagating outward), so a shrink-to-zero
  truncates exactly when the interp deletes the slot activations. No
  prewalk — the exit block is as always-executed as the preheader the
  parked plan wanted, and close time has every record naturally.
  Pins: findings/slot-shrink-truncate-aug2026/ (three faces, all
  red→green verified).
- P4 ⬜ soak on the amended binary (fleet deploy next).

## Former open residue (mid-loop guard bottoms) — RESOLVED

Resolved by Ruling 1a: with the guard ride deleted there is no
verdict to serve across iterations (or across cycles), so the
tail-loop and native twins agree — the iteration where the guard
bottoms bottoms the derivation. The "likely verdict" this section
used to carry (kernel one-state) was wrong; Eric's undecidability
framing settles it the other way, and simpler.
