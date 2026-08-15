# pending-triage — open classes, 2026-08-15 (post aug14f triage)

The aug14f overnight round (~240M subjects, five boxes) landed 25
findings. Eleven of the seventeen live divergences, plus two classes
parked earlier (`reactive_guard_arm_epoch3`, `reactive_catch_qop_select`),
were ONE seam: `Kernel::update`'s bottom-result arm did not persist the
bottom into the resident, so quiet cycles re-served the pre-bottom value
to de-fused consumers. Fixed (the interp-op twin now holds at the kernel
result seam); pinned as `findings/kernel-result-bottom-persists-aug2026/`.
What remains here survives that fix.

## Family A — refs (3 witnesses, 3 distinct sub-triggers, unchanged)

Every witness uses a value ref (`let r = &x`) AND a ref write
(`*r <- v`). No new witnesses overnight.

| file | trigger |
|---|---|
| `refintuple_recursion_extra_fire.gx` | ref carried in a **tuple**, deref'd through `.0`, inside a **recursive** fn |
| `refwrite_guard_extra_fire.gx` | ref-write flips a select **scrutinee**, guard read through a **second ref** |
| `crossmodule_refwrite_extra_fire.gx` | ref-write to **another module's** binding, recursive callee in that module reads it |

Read `findings/refwrite-guard-flip-aug2026` and
`findings/module-state-callee-reactivity-aug2026` first.

## Family B refined — rec-const-args stale refire (5 witnesses)

The old "extra event at output 0" signature was really "extra fire at
CYCLE OFFSET 0 (the injection cycle) carrying the previous epoch's
result" — the offsets in traces are cycle offsets, not output ids. The
surviving cluster shares one shape: a RECURSIVE fn called with CONSTANT
args whose terminal select reads a schedule input in an arm the taken
path never takes, plus a `<-` connect elsewhere. The interp is quiet at
offset 0 (the input is consumed only by an untaken arm); the jit
re-emits the stale result.

* `rec_const_args_stale_refire.gx` — minimized core + full sibling list
* `reactive_extra_stale_fire.gx` — the original hz0 witness (header
  updated; hz0 reactive 000001 is its on-box near-dup)

## New this round (each survives the bottom-persist fix)

| file | note |
|---|---|
| `iter_rec_guard_wrong_value.gx` | jit emits an untaken arm's VALUE (a bool through an [i64,bool] union) on deliveries where the interp is silent; sibling pure-extra-fire witness via `count` recorded in the header. Possibly the guard-read face of Family B — matrix before assuming |
| `window_div0_missing_fire.gx` | the round's one MISSING-fire survivor (window value-arg bottoms; jit silent where interp fires). Not yet matrixed; compare with `generated_missing_fires.gx` |

## Unrelated to all of the above

| file | note |
|---|---|
| `connect_in_call_arg_nontermination.gx` | needs an Eric ruling: should a `<-` inside a call ARGUMENT re-evaluate per call? and should an infinite recursion settle on the depth-limit bottom rather than grind? |
| `generated_missing_fires.gx` | MISSING fires. Not yet minimized — do that first |

## Refuted leads (do not re-chase)

* **dynamic modules** — minimal dynmod AGREES; witnesses minimized to
  programs with none.
* **"struct/array just de-fuse"** (Family A's tuple row) — all three
  composite rows report identical fusion.
* **one cause for the whole extra-fire symptom** — the aug14f fix pass
  proved the split empirically: 13 witnesses fell to bottom-persist,
  Family A and the rec-const-args cluster survived it.
