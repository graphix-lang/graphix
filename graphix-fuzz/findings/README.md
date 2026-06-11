# graphix-fuzz findings

Persisted minimal repro programs for divergences found by the fuzzer
sources (see `design/graphix_fuzz.md`). Each `.gx` file is a confirmed
divergence with its bisect class + root-cause analysis in the header.

These double as a **regression corpus**: re-run any directory through the
oracle and every program should now `AGREE` (the bug is fixed):

```
for f in graphix-fuzz/findings/<dir>/*.gx; do graphix-fuzz check "$f"; done
```

## select-jun2026

Two classic-path select bugs found while building the direct-path C5
mirror (which deliberately did not copy either): the order-unsound
trivially-true Nullable type predicate (#200, wrong value) and the
reachable `compile_ifchain` final-arm miss trap under a possibly-bottom
scrutinee (#201, SIGILL). Both fixed in the classic path.

## composite-qop-jun2026

Composite-success `?`/`$` SIGSEGV (#199), found during the C4
direct-path mirror work. One root cause: the QopUnwrap composite-success
arm handed a Value::Array's inline ValArray payload bits to consumers
expecting the boxed `*mut ValArray` composite ABI. Fixed in both the GIR
arm and the direct-path mirror via `graphix_value_into_array[_borrowed]`.
Two programs: owned-producer inner (the slice) and borrowed Local-read
inner.

## source-e-jun2026

10 confirmed divergences from the Source E adversarial-agent hunt (8
agents, found where a 400-mutant random campaign found 0). 4 root-cause
clusters — integer div/rem trap (A), GirType::Error DynCall marshalling
(B), value-shape unchecked arith error-drop / duration underflow (C),
StringInterpolate non-scalar part (D). All fixed (graphix #176 + the
netidx-value saturating duration sub). Programs that produce *bottom*
(div-by-zero) show as `Timeout` in all modes, which is agreement.
