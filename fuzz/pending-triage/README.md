# pending-triage — open classes, 2026-08-15 (post aug14f triage, both fixes)

The aug14f overnight round (~240M subjects, five boxes) landed 25
findings; triage resolved them into TWO kernel bugs, both fixed:

1. **kernel-result bottom persist** — `Kernel::update`'s bottom-result
   arm kept the pre-bottom value in the resident; quiet rides served it
   to de-fused consumers as real. 11 witnesses + 2 previously-parked
   classes. Pins: `findings/kernel-result-bottom-persists-aug2026/`.
2. **kernel frame-init const fire** — the kernel's init wire slot
   carried raw `event.init`, which frames force, so fused regions
   invoked from ordinary framed re-derivations (retained recursions
   re-dispatching on a delivery) minted FIRED consts and re-emitted
   stale results. This was the WHOLE rec-const-args cluster (the old
   "Family B", 5 witnesses), BOTH recursion members of the ref family,
   and what made `connect_in_call_arg` diverge. Pins:
   `findings/kernel-frame-init-const-fire-aug2026/`.

What remains below survives both fixes.

## Open divergences

| file | note |
|---|---|
| `refwrite_guard_extra_fire.gx` | the ref family's surviving member — NO recursion (so not the frame-init class): ref-write flips a select scrutinee, guard read through a second ref, jit extra fire. Campaign original = hz0 aug14f generate 000001 |
| `iter_rec_guard_wrong_value.gx` | jit emits an untaken arm's VALUE (a bool through an [i64,bool] union) on deliveries where the interp is silent; sibling pure-extra-fire witness via `count` in the header (ryouko aug14f fuzz 000000/000001) |
| `window_div0_missing_fire.gx` | the round's one MISSING-fire survivor (window value-arg bottoms; jit silent where interp fires). Campaign original = aieka aug14f reactive 000000. Not yet matrixed |
| `generated_missing_fires.gx` | MISSING fires, large generated subject. Not yet minimized — do that first, then check whether it is `window_div0_missing_fire` |

## Awaiting a ruling (no engine divergence)

| file | note |
|---|---|
| `connect_in_call_arg_nontermination.gx` | both engines now agree (both Timeout — the program is non-terminating under the current reading). Questions for Eric stand: (1) is a `<-` inside a call ARGUMENT meant to advance once (non-terminating here) or re-evaluate fresh per call? (2) should an infinite recursion settle on the depth-trip bottom instead of grinding? |

## Refuted leads (do not re-chase)

* **dynamic modules** — minimal dynmod AGREES; witnesses minimized to
  programs with none.
* **"struct/array just de-fuse"** (the old tuple-ref matrix row) — all
  three composite rows reported identical fusion. The real split was
  recursion (frame-init) vs not.
* **one cause for the whole extra-fire symptom** — it was TWO causes
  (bottom-persist, frame-init), and the residue above is neither.
