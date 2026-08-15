# pending-triage — EMPTY

Nothing is parked. The aug14f round (~240M subjects, five boxes, 25
findings, 2026-08-15) closed with six engine bugs found and fixed, and
its one remaining question ruled the same day:

1. kernel-result bottom persist (835542d2)
2. kernel frame-init const fire (c8794f0f)
3. in-frame formal init view (43e6af90)
4. key-0 dyncall fired-plane leak (45c5a4fb)
5. MapQ standing-bottom remint (561fb39a)
6. FoldQ standing-init acc poison + resize seed (f439b849 — which also
   reverted a wrong kernel-side "fix" for the same witness; see the pin
   headers for the post-mortem)

Ruled, not fixed: **atomic recursion** (`design/atomic_recursion.md`) —
evaluation is atomic within a cycle, so a program may legally spin
forever inside one; containment is the cooperative interrupt, which the
shell now arms on Ctrl-C. The occasioning witness
(`connect_in_call_arg_nontermination`) is a legally non-terminating
program under seed-applies-once — both engines agree by timing out —
and lives in that design doc rather than the corpus, where it would
burn a budget every gate run.

## Working notes for the next batch

* Gate on a CHECK-based sweep of `graphix-fuzz/findings/`, not on
  regress alone, when a change touches bottom/firing semantics. Regress
  false-greened over three broken pins on 2026-08-15 (fixed in
  78b9003e: non-ran agreements now retry sequentially at 4x budget),
  and the sweep is what caught it.
* Before matching one engine to the other, grep `findings/` for a
  ruling covering the seam — `fold-tainted-init-aug2026/00`'s header
  carried option A verbatim, and a fix that violated it shipped anyway
  because I read the header after breaking it.
* Refuted leads from this batch (do not re-chase): dynamic modules;
  "struct/array just de-fuse"; one cause for the extra-fire symptom (it
  was two); `array::window` in the fold class; the ref-write-wake
  reading of the MapQ remint; the depth-limit reading of the
  non-termination witness (tail loops accrue no depth).
* DynCall site-identity v2 (claimed per-position sites in scaffold
  loops taking the honest stale MASK) is the deeper design item behind
  fix 4 — it would also end the effect-refire and cross-position-cache
  residuals.
