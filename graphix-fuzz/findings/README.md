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

---

# The 2026-08-07 review batch (OPEN)

A directed interp-vs-JIT code review, run against `0174c8a2` while the
soak fleet was on the same commit. Every program below was reproduced by
hand with the prebuilt binaries; the ones marked ORACLE-BLIND are real
divergences that `graphix-fuzz check` reports as `AGREE`, because the
trace oracle records only the result VALUE. That blind spot is the theme
of half this batch — it is where the weeks of soaking could not look.

Full regression state at the time: `graphix-fuzz regress` = 270 programs,
0 regressions.

## select-guard-shortcircuit-aug2026 — WRONG VALUE

A fused select evaluates guards lazily (if-chain order); the node-walk
ticks EVERY arm's guard every cycle. Guards carry operand caches, so the
skipped evaluations desynchronize them and the two backends select
DIFFERENT ARMS. 00 = the jit's guard never ran (no history), 01 = it ran
on a different subset (staler history), 02 = the effect face
(ORACLE-BLIND).

## result-union-nullable-abi-aug2026 — WRONG VALUE / POINTER LEAK

`abi_kind` maps both `[T, null]` and `[T, Error<E>]` to
`AbiKind::Nullable`, and the select type-predicate lowers "is a T" as
`disc != NULL`. For a RESULT union an error is not NULL, so the success
arm is taken; a BINDING predicate then reads the error's payload word as
the scalar and an ASLR-varying heap pointer escapes as an i64. Reachable
from plain `/?` with no annotations.

## modstmt-fused-no-publish-aug2026 — SILENT DEATH

A signature-less module compiles to `Block { module: true }`, not to a
`Module` node, so it loses its module treatment twice: dead-statement
elimination classifies it effect-free (the `NodeView::Module` guard
never matches) and the ordinary block emitter env-pops its bindings at
the end of the `mod` statement. Three faces, all two-file CLI witnesses:
a trailing-expression module's exports go silently dead (this file's
program, which is also a COMPILE divergence in the oracle's module
form); a `<-`-driven module is dead-eliminated whole and prints nothing
ever; and a module that connects to its own binding has its exported
stream shifted by one, losing the seed value. 01 is the control — a
module whose last statement is a `let` works.

## init-over-limit-aug2026 — MISSING FIRE / PHANTOM FIRE

An over-limit `array::init` count means "no source update, keep state" to
the interp and "taint + clamp length to 0" to the kernel. They diverge in
both directions: 00 the kernel loses a value the interp emits, 01 the
clamp clobbers the prev-length word and the NEXT valid count over-fires.
The fix is one change on both sides.

## dyncall-stale-arg-fired-aug2026 — EFFECT DUPLICATED / WRONG VALUE

`DynCallSlot::dispatch` delivers every unmasked argument as
`TagValue::fired`, so a builtin that gates on argument PRODUCTION runs
again on every kernel invocation. 00 = `println` with a constant message
prints once per cycle instead of once (ORACLE-BLIND); 01 = `str::escape`,
which gates on a specific arg via `update_diff`, produces three values
the node-walk never produces (oracle-visible). 02 and 03 are the
value-visible cases the soak could NEVER see, because `oracle_tier`
excludes their names from value comparison: `rand::rand` re-randomizes
per invocation and `sys::time::now(trigger)` resamples the wall clock, so
fused code sees a clock advancing without its trigger. 04 is the same
effect duplication inside a collection HOF — per element per invocation.

## fusion-mutates-tvars-aug2026 — COMPILE-MODE SKEW

Merely ATTEMPTING fusion rewrites the program's static types.
`freeze_for_abi_normalized`'s rung 2 calls `Type::normalize()` on the
ORIGINAL type, and `TVar::normalize_int` writes the normalized binding
back into the shared cell — so a `select` with a `never()` arm has its
`[i64, never-tvar]` rtype collapsed to `i64` for every later consumer.
The witness is a `--check` diagnostic that differs between modes on a
program where NOTHING fuses. This is the root cause of
fusion-flips-display-backend-aug2026. The emitter's own doc comment
claims no rung rewrites TVar bindings; that is false of rung 2.

## variant-arity-tag-only-aug2026 — WRONG VALUE

A fused variant pattern tests only the TAG (`graphix_variant_tag_eq`
accepts either representation and never checks the payload count), so
two arms with the same tag at different arities compile to the SAME
condition and the first one wins: `[`A, `A(i64)]` holding `` `A(7) ``
gives 7 interpreted and -1 fused, with the missing payload slots read as
0. The node-walk checks (representation, arity, tag). No annotation is
needed — the union arises from ordinary inference — and the typechecker
requires both arms, so neither is dead. 02 is the reactive escalation
and the worst face: the wrongly-taken arm is a constant, so its
selection never changes, so the strict select rule makes the kernel go
SILENT — a wrong value degrading into a dead stream. Same family as
select-lit-leaf-union-slot-aug2026: when the kernel cannot distinguish
two arms it must de-fuse, not guess.

## dyncall-apply-unwired-aug2026 — ORACLE-BLIND, four faces

A fused DynCall's inner builtin Apply is not wired like a call site:
synthetic arg nodes with `Expr::default()` specs and ABI-FROZEN types,
the slot's scope instead of the call site's, and no `typecheck0` ever.
`dbg` loses its position and expression (00) and its type (01, renders
naked), `log` loses its scope prefix (02), and `println` leaks an
abstract type's private representation (03).

## tail-zero-iteration-fire-aug2026 — MISSING FIRE (interp side)

The residual leg of tail-scrut-fire-jul2026: a tail dispatch that takes
the base arm immediately never sets `reentered`/`framed`, so the interp's
`tail_scrut_fired` upgrade never applies and a re-delivered scrutinee is
silent where the kernel (uniformly) fires.

## rec-prev-looped-arming-aug2026 — MISSING FIRE (interp side)

`prev_looped` is assigned on every dispatch, including quiet polls that
never ran the body, so an intervening quiet cycle disarms the framed
re-derivation and a later capture fire never re-runs the recursion.

## callee-value-taint-passthrough-aug2026 — MISSING FIRE

`emit_value_taint_cache` PASSES THE TAINT THROUGH when no storage word is
available (callee bodies refuse to claim one), violating the stated bar
"NO storage → DE-FUSE, never pass through".

## sprintf-error-return-shape-aug2026 — MISSING FIRE

`str::sprintf` is declared `-> string` but its eval returns a bare
`Value::error` on a format failure. The interp passes it through, the
fused DynCall return-shape check drops it to bottom. The root cause is
the stdlib signature; the emitter's check is what prevents adopting an
Error payload as ArcStr bits and must stay. `str::sprintf("%d")` is the
whole witness.

## catch-callsite-coverage-aug2026 — MISSING FIRE

A lambda compiled to ONE shared kernel ignores per-call-site catch
COVERAGE: after an uncovered call site drives the kernel build, a later
call site that IS covered by a `catch` silently drops its error
delivery and the handler never runs. Coverage has to key the kernel (or
be passed in), the same lesson as the #206 / audit-jul2026 name-collision
fixes.

## qop-tailloop-frame-swallow-aug2026 — MISSING FIRE (interp side)

A `?` that raises inside a TAIL-LOOP iteration delivers to the handler
in the kernel and not in the node-walk: the frame-private variables map
that makes a re-entered pass a fresh evaluation frame also swallows the
handler write, which is an outward-bound event, not frame state.

## qop-scalar-error-leak-aug2026 — MEMORY LEAK (jit only)

The fused handler-less `?`/`$` path with a SCALAR success type never
drops the owned error Value: +12.9 MB/60s against a +2.9 MB/60s baseline
on a 0.1ms timer, with three controls isolating it (no error produced /
handler installed / --no-fusion all sit at baseline). The scalar arm's
comment — "a scalar `Nullable` inner is a by-value scalar (no heap)" —
is the same false assumption as result-union-nullable-abi-aug2026: true
for `[T, null]`, false for `[T, Error<E>]`, where the error is a boxed
`ValError`.

## fusion-flips-display-backend-aug2026 — ORACLE-BLIND

With `--no-fusion` the shell picks the ratatui display backend for a
program; with fusion it does not. `is_custom` decides with a `contains`
test that BINDS a free tvar, and the top-level expression's type is not
the same under the two modes.

## narrow-index-operand-verifier-aug2026 — NOT a divergence

Values agree. A narrow-int slice bound or bytes index is passed
un-widened to an i64 helper, so cranelift rejects the whole shared body
with a VERIFIER error and an arbitrarily large region falls back to the
node-walk. Two one-line `widen_to_i64` fixes.

## select-guard-prenarrow-bind-aug2026 — WRONG VALUE / MISSING FIRE

A select arm's GUARD was typechecked in `Select::typecheck0`'s first
loop, before the second loop narrows the arm's pattern binds to the
scrutinee type. The guard therefore saw its own binds as open TVars and
its first use BOUND them: `select u { v if p(v) => .. }` with
`u: [i64, f64]` and `p: fn(i64) -> bool` bound `v := i64` and compiled,
where the identical call in the arm BODY is correctly rejected (bodies
typecheck after the narrowing). Two faces, from two boxes the same
night: 00 is an ill-typed program admitted — the node-walk compares
dynamically while the kernel freezes the param to Scalar(I64) and
bottoms the f64; 01 is the one a user can hit writing CORRECT code — a
well-typed `[i64, Array<i64>]` scrutinee whose guard narrowed `v` to
i64, so the kernel froze a scalar param and silently dropped every
Array arrival. 02 was found a day earlier and PARKED as a semantics
question (what should a guard that bottoms on a non-numeric element
do?) — it was the same hole, the witness was ill-typed, and the
node-walk was emitting a FUNCTION VALUE as a program result. Fix: move
the guard's `typecheck0` into the second loop, beside the bool check
that was moved there earlier for the same reason. The narrowing is
progressive per arm, so the second loop is the only place the settled
type exists.

## oracle-tier-comment-scan-aug2026 — HARNESS

`oracle_tier` substring-scans the whole wrapper text, comments included,
so a finding whose write-up merely MENTIONS an excluded API is silently
excluded from the oracle and reports AGREE forever. Three existing pins
are affected today. README-only (no .gx).
