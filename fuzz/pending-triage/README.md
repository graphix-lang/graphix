# pending-triage — EMPTY

Nothing is parked, and nothing awaits a ruling.

## The aug14f arc, closed 2026-08-15

The round itself (~240M subjects, five boxes, 25 findings) plus an
audit of every historical finding still sitting on the boxes and in
`graphix-fuzz/fuzz/crashes/`.

**Nine engine bugs fixed:**

1. kernel-result bottom persist (835542d2)
2. kernel frame-init const fire (c8794f0f)
3. in-frame formal init view (43e6af90)
4. key-0 dyncall fired-plane leak (45c5a4fb)
5. MapQ standing-bottom remint (561fb39a)
6. FoldQ standing-init acc poison + resize seed (f439b849)
7. collection `merge_tag` fired-bit loss (c84e573b)
8. the shell never armed the cooperative interrupt, so a wedged
   program made the process unkillable by Ctrl-C (11db44b0)
9. array slice patterns matched TUPLE values — a type error, and the
   interp's unsoundest bug of the round: it bound leaves at the wrong
   type and emitted a value that violated the arm's own type
   (`findings/slice-pattern-tuple-type-aug2026/`)

**Two rulings:** atomic recursion (`design/atomic_recursion.md`) and
seed-applies-once inside a call argument — both closing witnesses
rather than opening work.

**Three gates repaired**, all the same disease — a budget artifact
treated as a semantic verdict:

* `regress` false-greened over broken pins under load; non-ran
  agreements now retry sequentially at 4x (78b9003e).
* `selfcheck` reported load-induced Timeout/Trace flips as
  nondeterminism; the confirm pair now runs at 4x and a Timeout is
  counted as INCONCLUSIVE, reported, never failed on.
* `jit_generated_sweep` read a Timeout in one mode as a divergence
  (only inside the full workspace run, which is ~13x slower than
  solo); it re-checks at 4x and reports what it skipped.

**Two backlogs audited to zero:** the 22 crash artifacts (none
reproduces — see `graphix-fuzz/fuzz/crashes/README.md`) and 2363
unique historical findings from all five boxes (2 still diverged; both
are fixed above, and 7 more are the known-benign fib
asymmetric-timeout class).

## Working notes for the next batch

* Gate on a CHECK-based sweep of `graphix-fuzz/findings/` when a change
  touches bottom/firing semantics — regress alone false-greened once.
* Before matching one engine to the other, grep `findings/` for a
  ruling covering the seam.
* A Timeout is not a value. Any gate that compares one against a value
  is measuring its own budget; escalate and report, never fail.
* "pending-triage is empty" is not "nothing is open" — the boxes and
  the crashes directory are separate backlogs.
* Tools: `GXDBG_SLOT` (collection per-slot productions + fold
  decision), `GXDBG_CS`/`GXDBG_REF` (dispatch/read pair),
  `GXDBG_LETBIND` (bind publication).
* Refuted leads: dynamic modules; "struct/array just de-fuse"; one
  cause for the extra-fire symptom (it was two); `array::window` in the
  fold class; the ref-write-wake reading of the MapQ remint; the
  depth-limit reading of the non-termination witness.
