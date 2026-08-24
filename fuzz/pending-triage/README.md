# pending-triage

## The aug18a arc (2026-08-20) — 3 fixed, 3 open

The fleet round on campaign aug18a (hz0/aieka/katana/ryouko, campaign
binary 1b1778b3, the organic-firing P4 soak): 12 divergences, 6
classes. Fixed and committed same day:

1. **framed-formal-seed** (c4fa9407) — a framed dispatch seeds its
   quiet args every pass; the fold-in-rec-arm that never published.
2. **trip-poison-extent** (6df2ec60) — one shared depth-trip poison
   bit with pop-to-zero extent; the kernel rode across trips (and
   refused legal rides past the root).
3. **connect-instance-identity** (e05a6c8b) — connect-target liveness
   in dead-elim, per-instance minted lifted ids, loop/rec/arm lift
   gates; the write-only-let spinner family.

Open, each parked here with mechanism located and a design question:

4. **class 4 — CLOSED** (2026-08-20): THE SHRINK-TO-ZERO RULE —
   always-executed loop-EXIT re-ensures (TruncRec records propagating
   outward per frame) truncate every in-loop chain when its level
   shrinks; no prewalk needed. Pins:
   findings/slot-shrink-truncate-aug2026/ (three faces: DynCall
   pairs, nested levels, callee site blocks).
5. **class 5 — CLOSED** (2026-08-20): ruled AND built same day —
   THE BOTTOM-OUT RULE (design/activation_state.md; ruled with
   state-multiplicity=activation-multiplicity). The finding INVERTED
   (the kernel's tail refusal was right; the interp's ride
   re-emission face is deleted, the kernel's value-position folds
   and undetermined-guard chain match). Pins moved to
   findings/bottom-out-aug2026/. Open follow-ons live in the design
   doc: the mid-loop guard-bottom residue (the back-edge bucket
   AUDITED CLOSED 2026-08-20 — covered by 003fa7d6's per-activation
   trees; degrade doors unreachable and now loud).
6. **class 6 — CLOSED** (2026-08-20): NOT a fusion leak — both modes
   flapped identically in isolation; `constrain_known` (+2 sibling
   walks) drained a name-keyed AHashMap in per-process hash order.
   Fixed by (name, TVarId) sorts; pin:
   findings/constrain-order-diag-aug2026/.

## The aug20a round (2026-08-22) — 5 findings, 1 class, CLOSED

Campaign aug20a (hz0/aieka/katana/ryouko on ad091e65, the
activation-state soak): 5 divergences — ryouko 4, hz0 1 — pulled at
the module-system redeploy (aug22a); all five reproduced on merged
main (bdd013b0). Triaged and fixed 2026-08-22; pins moved to
`graphix-fuzz/findings/quiet-frame-init-view-aug2026/` (00–04 the
campaign witnesses, 05–07 the two further faces found while
isolating).

**Shape:** an `array::iter`-driven binding `m` read ONLY by the guard
of a structure-failed arm, inside a `let rec` tail chain. Every `m`
delivery re-derives the chain (quiet framed passes in the interp), and
on each pass the `0 =>` arm is re-woken after sleeping on the n≠0
pass — loop plumbing, not a trigger. The interp emits once; the JIT
re-emitted per delivery.

**Root cause — NOT the guard fold** (the suspect above was wrong; the
consulted-guard chain is fine): a re-derivation inside a QUIET FRAME
(`frame_depth > 0 && !frame_init`) is not an init view — the interp's
Constant/Ref/Bind/lambda-priming sites all gate on `frame_init` there
— but three kernel mechanisms manufactured one anyway:

1. `DynCallSlot::sleep` reset `fired`, so every post-wake dispatch
   was a FIRST dispatch — forced `event.init`, every arg delivered
   fired, STALE mask ignored — and the arm-body DynCall fired on
   constant args the interp delivers stale. The interp's
   `CallSite::sleep` keeps `first_update`: a re-woken site is resumed,
   not re-primed (sleep is pause); only a site's first-ever dispatch
   is the `bound` init-view dispatch, and THAT one keeps its forced
   view at any frame depth (43e6af90's FIRED seeds — pins
   frame-formal-init-view-aug2026, which a frame-gated first dispatch
   broke on the first try). (Faces 00–04.)
2. A fused select's selection-changed word (`woke`) granted the
   re-selected arm an init view on every NATIVE tail-loop iteration
   (05), and in a callee kernel that cannot know statically it runs
   per iteration (06).
3. The same word inside a fused sub-region of an INTERP frame (07).

**Fix:** `DynCallSlot::sleep` no longer resets `fired`; wire slot 0
gains bit 1, THE QUIET FLAG — set by the wrapper from the interp
frame, by a tail-loop body for itself when `!init`, inherited by
callees through the context word — under which becoming-selected
grants no init view (a first-ever call/dispatch still does, as the
interp's `bound` dispatch does). See CLAUDE.md "Fusion / JIT
subsystem" (the QUIET FLAG entry).


## The aug22c round (2026-08-24) — 11 findings, 5 classes, parked

Campaign aug22c (hz0/aieka/katana/ryouko on e9791a6a, the quiet-frame
soak): ~2 days, 11 divergences — hz0 7, aieka 2, ryouko 2, katana 0 —
pulled at the traits redeploy (aug24a). **All 11 reproduce on merged
main (f60bbf2d)** under the release binary. None is traits-related:
the campaign predates that merge and every witness is core language.
Every one came from the corpus-mutation source (~236M subjects across
the fleet); generate (~260M) and reactive (~281M) found nothing.

### Class A — a `<-` target bound inside a literal loses its spin (5)

`aug22c_selfconnect_spin_{00..04}.gx` (hz0 x4, ryouko x1). Shape: a
`let` bound INSIDE a container literal that stands as a statement,
then a self-connect.

    {[i64:2, let x = true]; x <- x; true}
    interp: [0:true …capped]      jit: [0:true]

The values agree; the interp spins forever (the trace cap fires every
epoch) and the JIT goes quiet after the first. The control says the
interp is right: `{let x = true; x <- x; true}` — the same program with
the bind in statement position — AGREES, and both engines cap (its
region de-fuses, so "both" is really the node-walk twice). `x <- x` is
a self-feeding connect: x fires, the RHS reads x and fires, the connect
schedules the next cycle. Direction: in the fused form the lifted
connect target does not re-trigger itself. Neighbours: the
connect-instance-identity fix (e05a6c8b) and its write-only-let
spinner family.

### Class B — the same bind never publishes at all (2)

`aug22c_connect_no_publish_{00,01}.gx` (hz0). One step past class A:
the tail READS the connect target, and the JIT emits nothing at all —
not even the initial value.

    // schedule-v1: cap=16 events=128; in0=i64:1
    {select let x = i64:1 {i64:0 => i64:0, _ => ["a", "b"]}; x <- x + i64:1; x - (in0 * i64:0)}
    interp: [0:i64:1 1:i64:2 … 15:i64:16 …capped]; [ …capped]      jit: []; []

Here the bind sits in a select SCRUTINEE (00) or in an outer array
literal (01) — expression position again, so probably one class with A
in two faces: A's tail is a constant and still emits at init, B's
tail depends on x and emits nothing, which reads as "the kernel binding
never publishes".

### Class C — a labeled callback parameter eats the element (2)

`aug22c_labeled_callback_{00,01}.gx` (aieka, hz0). The 45-character
witness:

    array::map([i64:7], |#foo: i64 = i64:42| foo)
    interp: [0:[i64:42]]      jit: [0:[i64:7]]

Two bugs stacked. First, the TYPECHECKER accepts a map callback with
ZERO positional parameters when it has a labeled one with a default —
`|a, b|` and `||` are both rejected at that same call site, so the
arity check is counting the labeled parameter as filling the positional
slot. Then the engines disagree about what the callback receives: the
node-walk binds `foo` to its default and drops the element, the JIT
marshals the element into the callback's first slot. 01 is the same
shape with two labeled parameters and a struct result (`foo: i64:42`
interp vs `foo: i64:0` jit) plus an extra epoch fire. The arity hole
is the root worth fixing; the value divergence is downstream of a
program that should not compile.

### Class D — count over a nested map (1)

`aug22c_nested_map_count_00.gx` (ryouko).

    {let x = array::iter([i64:1, i64:2, i64:3, i64:4]);
     let f = |n| array::map([n], |i| buffer::from_string("hello"));
     let a = array::map([i64:-1, i64:1], |j| f(x));
     let c = count(a);
     select count(x) {i64:4 => c, _ => never()}}
    interp: [4:i64:1]      jit: [4:i64:4]

`count` of a value produced by a map whose callback calls a map-bodied
lambda over an `array::iter` source: the JIT counts a production per
element delivery, the interp one. Which cadence organic firing demands
here is not derived yet — do that before touching either engine.

### Class E — `?` in a map callback, and the node-walk is the suspect (1)

`aug22c_qop_callback_00.gx` (aieka).

    {let y = array::iter([i64:0, i64:2, i64:3, i64:4]);
     let m = array::map([i64:1], |x| select i64:1 {i64:1 => str::parse("42")?, _ => y});
     let c = m; c}
    interp: []      jit: [0:[i64:42]]

The scrutinee is a constant that matches the first arm, `str::parse`
succeeds, and `?` on a non-error is the bare value — so the JIT's
`[42]` is what the program says and the INTERP producing nothing is the
side that needs explaining. Node-walk-is-canonical is a claim about the
model, not an oracle: adjudicate this one against the intended
semantics before assuming the kernel is wrong.
