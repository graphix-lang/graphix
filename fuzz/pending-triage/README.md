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

