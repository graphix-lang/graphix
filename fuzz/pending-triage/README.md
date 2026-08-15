# pending-triage — aug14f batch, FULLY TRIAGED 2026-08-15

The aug14f overnight round (~240M subjects, five boxes, 25 findings)
resolved into TWO kernel bugs fixed same day, four root-caused open
classes, and one standing ruling request. Every open class below is
adjudicated to mechanism level with a one-ingredient boundary matrix
in its file header.

## Fixed (committed, pinned)

1. **kernel-result bottom persist** (835542d2) — the wrapper's bottom
   result didn't persist into the resident; quiet rides served the
   pre-bottom value to de-fused consumers. 11 witnesses + 2 parked
   classes. Pins: `findings/kernel-result-bottom-persists-aug2026/`.
2. **kernel frame-init const fire** (c8794f0f) — the kernel's init
   wire carried raw `event.init`, which frames force; in-frame
   invocations minted FIRED consts and re-emitted stale results. The
   whole rec-const-args cluster (5 witnesses), both recursion members
   of the ref family, and the engine half of connect_in_call_arg.
   Pins: `findings/kernel-frame-init-const-fire-aug2026/`.

## Open — JIT wrong (interp is the ruled semantics)

| file | mechanism | fix locus |
|---|---|---|
| `refwrite_guard_extra_fire.gx` | key-0 scaffold-loop DynCall sites deliver args FIRED (documented aug08a approximation); through a cross-kernel callee's return disc this fires a select whose only fired input is an untaken arm's | DynCall site-identity v2: claimed per-position sites in scaffold loops (designed, not built) |
| `window_div0_missing_fire.gx` | fused fold poisons on a STANDING bottom init where the aug13k ruling (init poison gates on triggering delivery) rides the retained acc | emit_fold init taint: bare TAINT vs TAINT\|STALE split — the aug13k fix's kernel twin |

## Open — INTERP wrong (jit matches the ruled semantics)

| file | mechanism |
|---|---|
| `iter_rec_guard_wrong_value.gx` | a guard-flip re-selection inside a recursion frame doesn't emit the newly-selected arm's value; the non-recursive twin agrees with the jit exactly (recursion ruling: fires like the chain) |
| `generated_missing_fires.gx` | a ref-write's WAKE converts into an emission through a guard-select over a standing-bottom scrutinee — nothing the select consumes fires (organic firing says quiet; the jit is quiet) |

## Awaiting Eric

| item | question |
|---|---|
| `connect_in_call_arg_nontermination.gx` | engines now AGREE (both Timeout). (1) is a `<-` inside a call ARGUMENT meant to advance once (non-terminating here) or re-evaluate fresh per call? (2) should an infinite recursion settle on the depth-trip bottom instead of grinding? |
| `generated_missing_fires.gx` polarity | the organic ruling seems to answer it (a wake is not a fire), but confirm before the interp fix: may a ref-write wake ever produce emissions from nodes whose consumed inputs did not fire? |

## Refuted leads (do not re-chase)

* **dynamic modules** — minimal dynmod agrees; witnesses minimized to none.
* **"struct/array just de-fuse"** — identical fusion across composite
  rows; the real split was recursion (frame-init) vs not.
* **one cause for the extra-fire symptom** — it was two (bottom-persist,
  frame-init), and the four survivors are four distinct mechanisms.
* **array::window in the fold class** — probed out; plain div0 diverges
  identically.
