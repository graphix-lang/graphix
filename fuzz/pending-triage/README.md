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

## The aug20a round (2026-08-22) — 5 findings, 1 apparent class, PARKED

Campaign aug20a (hz0/aieka/katana/ryouko on ad091e65, the
activation-state soak): 5 divergences — ryouko 4, hz0 1 — pulled at
the module-system redeploy (aug22a). Parked per Eric before root-cause
work; all five REPRODUCE on merged main (bdd013b0), so they are not
module-system artifacts and not fixed by it.

**Shape (all five, `aug20a_epoch_refire_*.gx`):** an
`array::iter([1,2,3,4])`-driven binding `m` is read ONLY by the guard
of a structure-failed arm of an inner select over a constant
scrutinee, inside a `let rec` callee:
`select <const≠0> { 0 if m == 0 => A, _ => B }`. The interp emits B
once (the guard is never consulted — structure fails first, so `m` is
not a consumed input); the JIT re-emits B on EVERY `m` delivery
(interp `[0:v]` vs jit `[0:v 1:v 2:v 3:v 4:v]`; the `array::group`
variant turns the extra fires into a phantom `[2,2,2]` group).

**Suspect** (UNCONFIRMED): the guard PROLOGUE's `guard_stale` fold in
`emit_select_arms` counts a non-consulted guard's freshness as an
own-fire — the consulted-guard rule says a structure-failed arm's
guard is irrelevant on both planes. BUT the bare skeleton does NOT
reproduce: probes of the naked select, a plain callee wrapper, and a
minimal `let rec` wrapper all AGREE — the embedded minimized forms
(which still diverge) each retain extra ingredients (inner
`count(f(x))` rec-shadow body, `math::abs` cmp, `array::group`
shell), so the real trigger is narrower and still unisolated. Each
file carries its campaign-minimized form under `// minimized:`.
