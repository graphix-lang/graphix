# pending-triage — open classes, 2026-08-15

Seven open classes. Six of them came out of the aug14e/aug14f soaks and
share one SYMPTOM — "the jit emits an extra event carrying a prior
value" — but they split cleanly into **two families with no overlap**,
which is the useful structure here. Do not fix them as one thing until
something actually connects the families.

## Family A — refs (3 witnesses, 3 distinct sub-triggers)

Every witness uses a value ref (`let r = &x`) AND a ref write
(`*r <- v`). Each sub-trigger was reduced to a small deterministic
program with a one-ingredient-at-a-time boundary matrix.

| file | trigger |
|---|---|
| `refintuple_recursion_extra_fire.gx` | ref carried in a **tuple**, deref'd through `.0`, inside a **recursive** fn. Needs BOTH; struct- and array-carried refs agree *and fuse identically* |
| `refwrite_guard_extra_fire.gx` | ref-write flips a select **scrutinee**, guard read through a **second ref**. No recursion, no composite |
| `crossmodule_refwrite_extra_fire.gx` | ref-write to **another module's** binding, recursive callee in that module reads it |

CLAUDE.md lists ByRef/Deref as a *lower-impact* missed-fusion gap. Three
independent correctness divergences in one campaign says it is not
lower-impact — it is a systematic seam. Two existing finding dirs are on
the same ground and should be read first:
`findings/refwrite-guard-flip-aug2026` and
`findings/module-state-callee-reactivity-aug2026`.

## Family B — reactive, no refs (3 witnesses, one exact signature)

`reactive_extra_stale_fire.gx` (+ two near-duplicates recorded in its
header, incl. `reactive_guard_arm_epoch3.gx`). None of these uses a ref
at all. Signature is identical across all three:

* reactive, schedule-driven, multi-epoch; exactly two outputs 0 and 1
* epoch 0 agrees
* every later epoch, the jit emits an extra event at output **0**
  carrying the value output **1** held in the **previous** epoch

The stale value being the *other output's* prior value is the lead: a
value-ride on output 0 would re-deliver output 0's own history. This
points at output/slot identity across epochs.

## Unrelated to both

| file | note |
|---|---|
| `connect_in_call_arg_nontermination.gx` | needs an Eric ruling: should a `<-` inside a call ARGUMENT re-evaluate per call? and should an infinite recursion settle on the depth-limit bottom rather than grind? |
| `generated_missing_fires.gx` | MISSING fires, not extra. Not yet minimized — do that first |
| `reactive_catch_qop_select.gx` | parked 2026-08-14 |

## Refuted leads (recorded so they are not re-chased)

* **dynamic modules.** Three of the first four aug14f findings carried a
  `mod .. dynamic` and it looked like a common cause. A minimal dynamic
  module AGREES, and the witness that had one minimized to a program
  with none. Refuted.
* **"struct/array just de-fuse."** The obvious deflation of Family A's
  tuple row. Ruled out: all three composite rows report identical
  fusion (attempted=11 fused=2).
