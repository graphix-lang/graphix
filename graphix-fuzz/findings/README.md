# graphix-fuzz findings

Persisted minimal repro programs for divergences found by the fuzzer
sources (see `design/graphix_fuzz.md`). Each `.gx` file is a confirmed
divergence with its bisect class + root-cause analysis in the header.

These double as a **regression corpus**: re-run any directory through the
oracle and every program should now `AGREE` (the bug is fixed):

```
for f in graphix-fuzz/findings/<dir>/*.gx; do graphix-fuzz check "$f"; done
```

## source-e-jun2026

10 confirmed divergences from the Source E adversarial-agent hunt (8
agents, found where a 400-mutant random campaign found 0). 4 root-cause
clusters — integer div/rem trap (A), GirType::Error DynCall marshalling
(B), value-shape unchecked arith error-drop / duration underflow (C),
StringInterpolate non-scalar part (D). All fixed (graphix #176 + the
netidx-value saturating duration sub). Programs that produce *bottom*
(div-by-zero) show as `Timeout` in all modes, which is agreement.
