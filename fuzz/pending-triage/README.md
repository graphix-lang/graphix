# pending-triage — one item, awaiting a ruling

## Open

| file | why it is here |
|---|---|
| `slice_pattern_matches_tuple.gx` | A slice pattern `[x, y]` matching a TUPLE value. Both engines are wrong and the interp is unsound (it emits a value that violates its own static type, and says so in the log). No runtime discriminator exists — a tuple and an array are the same `Value::Array` — so the fix is a language decision: reject the ambiguous union at compile time (recommended), type-directed matching (incomplete by construction), or rule that slice patterns match tuples (contradicts the typechecker). Full analysis in the file header. |

## Closed 2026-08-15

The aug14f round (~240M subjects, 25 findings) plus a full audit of
every historical finding on every box:

* **Seven engine bugs fixed:** kernel-result bottom persist
  (835542d2), kernel frame-init const fire (c8794f0f), in-frame formal
  init view (43e6af90), key-0 dyncall fired-plane leak (45c5a4fb),
  MapQ standing-bottom remint (561fb39a), FoldQ standing-init acc
  poison + resize seed (f439b849), and the collection `merge_tag`
  fired-bit loss (found in the backlog audit).
* **Two rulings:** atomic recursion (`design/atomic_recursion.md`) and
  seed-applies-once inside a call argument — both closing witnesses
  rather than opening work.
* **One gate fixed:** regress false-greened over broken pins under
  load (78b9003e); non-ran agreements now retry sequentially.
* **The crash backlog audited to zero** — 22 artifacts, none
  reproduces (`graphix-fuzz/fuzz/crashes/README.md`).
* **2363 unique historical findings re-checked** across all five
  boxes: 2 still diverged (one fixed above, one parked here), 7 are
  the known-benign fib asymmetric-timeout class, the rest agree.

## Working notes

* Gate on a CHECK-based sweep of `graphix-fuzz/findings/` when a change
  touches bottom/firing semantics — regress alone false-greened once.
* Before matching one engine to the other, grep `findings/` for a
  ruling covering the seam.
* `GXDBG_SLOT=1` is the collection tool (per-slot productions + fold
  decision); `GXDBG_CS`/`GXDBG_REF` are the dispatch/read pair.
* Refuted leads: dynamic modules; "struct/array just de-fuse"; one
  cause for the extra-fire symptom (it was two); `array::window` in the
  fold class; the ref-write-wake reading of the MapQ remint; the
  depth-limit reading of the non-termination witness.
