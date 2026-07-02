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

## lambda-jun2026

The shadowed-lambda-name self-call crash (#206), found during E3's
audit of name-based self-call matching: `finish_kernel` registers a
kernel's own name in `known_fns` before its body emits, and the
name-only resolution in `emit_known_fused_call` matched a body call to
a shadowed same-name OUTER lambda against the kernel itself — an
infinite native self-call (stack overflow under DirectJit; classic
never built these kernels). Fixed by `KnownFusedFn::self_bind` BindId
verification.

## dyncall-jun2026

The pending String DynCall sentinel-drop SIGSEGV (#214), found by the
F1 mutation soak as a whole-process crash that killed the campaign
(and motivated `GRAPHIX_FUZZ_ECHO` crash forensics). String DynCall
results rode the scalar convention — no site-level pre_pending branch
— so a pending dispatch's sentinel-zero flowed into owned-ArcStr drop
positions (`graphix_arcstr_drop(0)` → SIGSEGV). Fixed: String results
now branch at the site like composite/Value results (shared
`emit_dyncall_pending_branch`, both paths), and all five JIT drop
helpers null-check + panic instead of UB.

Fixing the crash exposed a residual pre-existing VALUE divergence —
a pending DynCall in dead position bottoms the whole fused kernel
(interp = 0, jit = Timeout), because whole-kernel pending is coarser
than the canonical per-node bottom. Resolved (#216, Eric's call): a
sync variadic builtin called with no positional arguments has no data
inputs and can never fire — now a COMPILE ERROR pointing at never()
(itself reclassified Async, which exempts it and stops it fusing into
always-pending kernels). Both programs now CompileErr in every mode —
agreement — and guard against the error ever being relaxed.

## source-e-jun2026

10 confirmed divergences from the Source E adversarial-agent hunt (8
agents, found where a 400-mutant random campaign found 0). 4 root-cause
clusters — integer div/rem trap (A), GirType::Error DynCall marshalling
(B), value-shape unchecked arith error-drop / duration underflow (C),
StringInterpolate non-scalar part (D). All fixed (graphix #176 + the
netidx-value saturating duration sub). Programs that produce *bottom*
(div-by-zero) show as `Timeout` in all modes, which is agreement.

## flip-jun2026

Three same-class divergences found by the first post-F2-flip generate
campaign: a DEAD statement containing an arithmetic bottom (div/mod by
zero inside a discarded tuple/array, or an unused let holding an
aborting array literal) poisoned the whole fused kernel via the
composite producers' bottom-abort — interp = value, jit = Timeout. The
classic planner never hit this because its prune pass removed dead
statements before emission; the direct path had no pruning. Fixed by
dead-statement elimination at the direct block-emission seam
(`emit_block_node`): a Bind is emitted iff a later sibling or the tail
references one of its bound ids; a bare expression statement is always
dead (sync-emittable code has no effects).

## audit-jul2026

Three OPEN divergences found by the 2026-07 pre-release fusion audit
(none by the generator — a coverage gap in itself: it produces neither
shadowed rebinding, multi-monomorphization regions, nor recursive-ADT
pipelines). 01/02 share one root cause: cross-kernel lambda-call
resolution is keyed by source NAME only (`discover_lambda_calls` keeps
one kernel per name; emission resolves sites via name-keyed
`funcids`/`callee_refs` without checking the site's kernel Arc) — a
regression of the #206 fix, whose BindId guard lived in the deleted
classic path's `emit_known_fused_call`. 01: a shadowed same-name outer
lambda reached transitively gives a silent WRONG ANSWER (interp 5,
jit 1). 02: two monomorphizations of one polymorphic lambda in one
region collide on the name and PANIC cranelift's FunctionBuilder at
compile time, killing the runtime worker. A BindId-only guard cannot
fix 02 (same binding, two FnTypes) — resolution must key on kernel
identity (the Arc). 03: root cause OPEN — a fold callback calling a
local lambda whose body consumes a shared recursive-ADT subtree via
TWO terms double-counts the first term and drops the second (exactly
2x checksum; needs tree width >= 16 and the call through the callback;
the ADT pipeline itself node-walks, pointing at fired/STALE glitching
of the fused scalar fragments around the fold). Found by
bench/symbolic.gx.
