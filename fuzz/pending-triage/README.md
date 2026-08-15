# pending-triage — aug14f batch fully resolved, 2026-08-15

The aug14f round (~240M subjects, five boxes, 25 findings) closed with
SIX engine bugs found and fixed in one day:

1. kernel-result bottom persist (835542d2)
2. kernel frame-init const fire (c8794f0f)
3. fold standing-bottom init poison (81c610f1)
4. in-frame formal init view (43e6af90)
5. key-0 dyncall fired-plane leak (45c5a4fb)
6. MapQ standing-bottom remint — the finish minted FRESH_BOTTOM on any
   production while a slot carried its persistent taint mark; a find
   over a permanently-bottoming predicate re-fired a standing bottom
   off quiet PASS_THROUGH source rides. The "ref-write wake" polarity
   question this was briefly held on DISSOLVED: there was never a
   wake-driven emission, just the remint driving a guard — covered by
   the existing standing-bottoms ruling
   (pin: findings/mapq-standing-bottom-remint-aug2026/).

Every witness from the round — campaign findings, probes, and parked
classes — now AGREEs except the known-benign fib(30) asymmetric
timeout.

## Awaiting Eric — the only open item

| file | question |
|---|---|
| `connect_in_call_arg_nontermination.gx` | engines AGREE (both Timeout — no divergence). (1) is a `<-` inside a call ARGUMENT meant to advance once (making this program non-terminating) or re-evaluate fresh per call? (2) should an infinite recursion settle on the depth-trip bottom instead of grinding past budgets? |

## Notes carried forward

* DynCall site-identity v2 (claimed per-position sites in scaffold
  loops taking the honest stale MASK) remains the deeper design item
  behind the key-0 fix — it would also end the effect-refire and
  cross-position-cache residuals.
* Refuted leads from this batch (do not re-chase): dynamic modules;
  "struct/array just de-fuse"; one cause for the extra-fire symptom;
  array::window in the fold class; the ref-write-wake reading of the
  find remint.
