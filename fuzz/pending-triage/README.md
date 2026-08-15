# pending-triage — aug14f batch closed out, 2026-08-15

The aug14f round (~240M subjects, five boxes, 25 findings) is fully
resolved: FIVE engine bugs found and fixed same day — two kernel
(bottom-persist 835542d2, frame-init const fire c8794f0f), then the
three remaining root-caused classes on Eric's "fix what needs no
ruling": the fused fold's standing-bottom init poison (kernel), the
in-frame formal init view (interp), and the key-0 dyncall fired-plane
leak (kernel). Pins: `findings/kernel-result-bottom-persists-aug2026/`,
`findings/kernel-frame-init-const-fire-aug2026/`,
`findings/fold-init-bottom-ride-aug2026/`,
`findings/frame-formal-init-view-aug2026/`,
`findings/key0-dyncall-stale-fold-aug2026/`.

## Awaiting Eric — everything left in this directory

| file | question |
|---|---|
| `connect_in_call_arg_nontermination.gx` | engines AGREE (both Timeout). (1) is a `<-` inside a call ARGUMENT meant to advance once (non-terminating here) or re-evaluate fresh per call? (2) should an infinite recursion settle on the depth-trip bottom instead of grinding? |
| `generated_missing_fires.gx` | the batch's one unfixed divergence, adjudicated interp-side over-fire (a ref-write WAKE converts into an emission through a guard-select over a standing-bottom scrutinee; organic firing says quiet and the jit is quiet). Held for polarity confirmation before the interp fix: may a ref-write wake ever produce emissions from nodes whose consumed inputs did not fire? |

## Notes carried forward

* The deeper design item behind the key-0 fix: DynCall site-identity
  v2 (claimed per-position sites in scaffold loops taking the honest
  stale MASK, not just an honest result tag). The stale-fold fix
  closes the fired-plane divergence; per-position identity would also
  end the effect-refire and cross-position-cache residuals.
* Refuted leads from this batch (do not re-chase): dynamic modules;
  "struct/array just de-fuse"; one cause for the extra-fire symptom
  (it was two); array::window in the fold class.
