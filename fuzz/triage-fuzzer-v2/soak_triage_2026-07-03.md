# Overnight soak triage — 2026-07-03/04

Three campaigns (fuzz / generate / generate --reactive), PAR=24, corpus dirs
under `fuzz/soak-jul2026/`. This file is the running classification; raw
findings stay in the corpus dirs.

## Harness incident (resolved)

Rebuilding `graphix-fuzz` under the live soaks broke every child spawn:
`current_exe()` resolves to `… (deleted)` after the binary is replaced, and
`check_isolated` recorded each spawn ENOENT as a program crash at
instant-fail speed — **908,469 garbage crash files** in ~4 minutes (purged
by mtime cutoff; real findings preserved). Two harness fixes landed:

- children re-exec via `/proc/self/exe` (`child_exe()` in graphix-fuzz
  lib.rs) — immune to on-disk replacement, children run the same inode as
  the parent;
- a spawn IO error is now a FATAL harness abort (exit 2), never a recorded
  finding.

## Fixed this session (all repros re-verified AGREE on the fixed build)

1. **clone_rebind typedef panic** (`corpus-fuzz/divergence_000002`,
   triage copy `crash_clone_rebind_recompile_panic.gx`): a `type T` in an
   HOF callback body killed the runtime on every per-slot clone —
   `compiler::compile` replayed into the scope the template already
   registered `T` in, and `deftype` rejects redefinition
   ("T is already defined in scope array::fnN::fnM" → the lib.rs:1029
   `.expect()`). Fix: the default `clone_rebind` now recompiles into a
   fresh anonymous child scope (`rbN`) — the replay is hermetic, each
   clone owns (and its delete cleans) its registrations, captures resolve
   by walking up. graphix-compiler/src/lib.rs.

2. **Slice/string owned-operand taint drop**
   (`corpus-fuzz/divergence_000004`): `a[5..]` over an array-literal input
   containing `array::iter` emitted a real
   `ArrayIndexError("start index 5 out of bounds 0")` on the init cycle —
   the kernel sliced the not-yet-fired input's EMPTY placeholder and the
   error escaped the output gate. Root: `emit_owned_value_operand_node`'s
   composite arm (and the String arm) discarded the source disc when
   wrapping via `graphix_value_new_from_array` / `_new_string`, dropping
   TAINT|STALE. Fix: `propagate_flags` fold of the source disc onto the
   minted Value disc, both arms. fusion/emit.rs.

## Open — needs a decision (Eric)

3. **Interior-bottom semantics family** — 4 findings, one root:
   - `corpus-reactive/divergence_000001`: `{v0 <- v0+1; v0 + ((100*-100) % (v0-1))}`
     — at v0=1 the modulo div0s. Node-walk: the mod node bottoms, but `+`
     still fires (v0 updated) using the mod's **Cached previous value** →
     interp emits `1 + 0 = 1`. JIT: taint forces the whole output to
     bottom → event missing. From cycle 2 they agree again.
   - `corpus-reactive/divergence_000002`: same with `7 / (7 - v0)` at v0=7.
   - `corpus-reactive/divergence_000000`: same family
     (`(42 /? v0)$` at v0=0), but the recorded jit outcome was Timeout —
     rerun uncontended reproduces DIVERGE; the wedge wrinkle needs a look.
   - `corpus-generate/divergence_000001`:
     `array::fold([u8:0, u8:100], u8:100, |v7, v8| u8:1 / v8)` — slot 0
     div0s; the node-walk recovers (result 0) because its fold is CHAINED
     DATAFLOW (slot i's acc-out is an ordinary cached input to slot i+1;
     this callback ignores acc, so slot 1 fires from the element alone);
     the JIT kernel bottoms the whole fold. Probe-verified: with an
     acc-CONSUMING callback (`|acc, x| acc + 100/x`) the node-walk fold
     yields nothing at all — no special fold rule, just cached-node
     firing — and the cache-substitute design below reproduces both
     cases.

   Also `corpus-reactive/divergence_000003` (`-1/(-1+v1)` at v1=1) — 5
   findings, one root.

   The gap, stated as the invariant it violates: in the node-walk a
   bottoming op returns `None` and its consumers keep firing with the
   op's **Cached last value** (node/op.rs: `Cached` operands, error →
   log + `None`); so "no value this cycle" and "no value EVER" are
   different states. The #219 taint channel conflates them for INTERIOR
   ops: a div0 taints its consumers within the cycle even when the op
   has prior history. Kernel INPUTS already honor the distinction (the
   runtime hands STALE + last value after the first fire; TAINT only at
   init, when the node-walk's `Cached.as_ref()?` would also yield
   nothing) — which is exactly why every single-cycle program agrees
   and only warm-cache reactive cycles diverge.

   Proposed exact fix (needs Eric's sign-off — state growth + emit
   surface): make TAINT mean "no value ever seen at this point"
   uniformly. At each taint SOURCE (the `taint_if` sites: div0/mod0,
   unchecked overflow, `$`, handler-less `?`, …) allocate a kernel-state
   cache word pair; on untainted compute → store; on taint → substitute
   the cached value with STALE set and taint CLEARED (taint only if no
   history). Downstream then behaves exactly like the node-walk
   (consumers fire iff any operand fired, STALE AND-reduce already
   encodes that; first-ever bottom still gates the output). Inside HOF
   loop scaffolds the cache persists across iterations — which matches
   the node-walk too, since the callback node graph (and its `Cached`
   cells) is shared across slots (this also fixes the fold finding).
   Pure kernels with no cross-cycle inputs can skip the machinery
   entirely (no history is observable) — zero cost where fusion wins
   most. **Not patched pending discussion.**

4. **Exponential-breadth recursion wedge** (`corpus-fuzz/crash_000000`
   fib(-100), `crash_000005` fib with `fib(n-1)+fib(n+2)`, triage copy
   `hang_exponential_breadth_recursion.gx`): depth-guarded per path but
   2^N paths; non-tail recursion never polls the interrupt flag, so both
   evaluators wedge until the child's outer deadline. Proposal:
   `ctx.interrupted()` check at `GXLambda::update` entry + an interrupt
   check inside `graphix_depth_push` — makes runaway breadth abortable in
   both evaluators. Touches evaluator semantics → discuss first.

## Open — concrete JIT bugs (next in queue)

5. **pack_value_to_u64 marshal panic** (emit.rs:1090;
   `corpus-fuzz/divergence_000003`):
   `{let f = 'a: Number|x: 'a| -> 'a f64:0.; {let a = f(i64:3); let b = f(f64:2.5); cast<f64>(a)$ + b}}`
   — constrained-poly lambda at two numeric types; a kernel slot declared
   F64 receives I64 at runtime → panic kills the runtime. Two-monomorphizations
   family (#206 kin). Also suspicious that this typechecks at all — the
   body is concrete `f64:0.` against return type `'a` (a tvar-constraints
   / task #20 motivating case?).

6. **Composite param gets String** (kernel.rs:1206;
   `corpus-fuzz/divergence_000006`, same finding): the window+buffer
   program — `buffer::from_string("hello")` lands a String in a param slot
   the signature classified composite → marshal panic kills the runtime.
   `bytes`/buffer ABI classification vs runtime value.

7. **FIXED — dead `$` statement wrong-bottoms the kernel**
   (`corpus-generate/divergence_000000`): interp `Null@0`, jit no event.
   Ablation isolated it to the UNUSED `let v1: [null, string] = v0{"k2"}$`
   — the fold/guard/shadowing were all innocent; even a constant output
   was suppressed (`{let v0 = {"k1"=>"xyz"}; let v1 = v0{"k2"}$; i64:100}`
   reproduces). Mechanism: `emit_qop_node`'s non-scalar success arms
   jump to `pending_exit` (whole-kernel bottom) on the error path — fine
   when the result is consumed on the output path, wrong for a dead
   local. The dead-statement elimination pass exists for EXACTLY this
   hazard but its effect-free gate treated `$`/handler-less-`?` as
   effects because they log — yet those logs are node-walk-only by
   design (fused kernels drop them even when live). Fix: the gate now
   flags only handler-FUL `?` (writes the catch variable);
   `$`/handler-less-`?` are effect-free, so dead binds containing them
   eliminate. fusion/emit.rs `stmt_subtree_effect_free`.
   Note the LIVE-but-off-output-path variant of the qop abort is still
   wrong in principle (it's the same interior-bottom conflation as item
   3) — the placeholder rework there subsumes it.

8. **corpus-generate/divergence_000002**: select + map-miss `$` + string
   interp + nested array — interp emits a composite, jit nothing. Not yet
   analyzed.

Pre-swap findings already classified: `corpus-fuzz/divergence_000001` =
the `sys::fs::tempdir` interp-side async-quiescence flake class (the
record filter now excludes rand::/sys::/http:: programs).

## Post-restart findings (2026-07-04, fixed binary)

9. **FIXED — `list::init(i64:MAX, …)` wedge** (`corpus-fuzz/crash_000020`):
   `list::init` lacked the `MAX_ARRAY_INIT_LEN` cap `array::init` got in
   Phase 4 — building 9e18 slots wedges un-interruptibly. Same log+bottom
   cap added (graphix-package-list). `seq(0, MAX)` (crash_000010) remains
   the un-capped representative of the class — it's a stream not an
   allocation, so it belongs to the interrupt-poll discussion (item 4).

10. **HARNESS artifact — `#[native]` seeds** (divergence_000021, deleted):
    a mutant embedded a `#[native]`-carrying fixture inside a select arm
    (where fusion never descends) → CompileErr under jit only, phantom
    divergence. `#[native]` is deliberately mode-asymmetric test infra;
    build.rs now strips it from the seed pool.

Also re-recorded post-restart: fib(-1) breadth wedge (crash_000022 —
known item 4; buckets reset per campaign run).

11. **Post-fix generate stream = the qop-abort residual** (divergence_000013/
    14/15 and counting): live CHAINS of binds that consume a `$` bottom but
    never reach the output (`let a = m{k}$; let b = f(a); <output ignoring b>`)
    still whole-kernel-abort — the dead-stmt fix only eliminates fully-dead
    binds. Same root as item 3's non-scalar qop error path; the tainted-
    placeholder rework subsumes it (an interior `$` error must produce a
    taint-marked placeholder, not a pending-exit).

12. **HOF + ByRef callback under-fire** (`corpus-fuzz/divergence_000027`):
    `array::find(a, |x| {let r = &(1,2); let t = *r; t.0+t.1} > 3)` —
    interp emits Null at cycles 0 AND 1 (the ByRef/deref residue
    node-walks and delivers a cycle late; find recomputes and re-emits),
    jit emits once. Firing divergence at the fused-HOF/node-walk-residue
    seam (ByRef is correct-None for fusion; the seam's firing isn't).

13. **FIXED — UB: tainted qop unboxed Null as Array** (SIGABRT crash,
    2026-07-04; triage copy `crash_sigabrt_tainted_qop_unbox.gx`):
    `{let a = [i64:1, let n = i64:5, i64:3]; let x = a[i64:1..]; x$}` —
    the `let`-in-element makes the literal's element Bottom-typed →
    tainted; `x$`'s composite-success arm checked only `is_err` on the
    CLEAN disc, so the tainted Value::Null placeholder took the success
    path into `graphix_value_into_array_borrowed` →
    `unreachable_unchecked` (nounwind abort under debug_assertions,
    TRUE UB in release). Fix: taint folds into the abort branch
    (fusion/emit.rs emit_qop_node); both unboxers hardened from
    unreachable_unchecked to defined panics. Verified: repro bottoms
    (matches interp), untainted `$` unchanged.

14. **Unchecked integer-add overflow: node-walk bottoms, JIT wraps**
    (`corpus-fuzz`, tail-loop `count(500000, i64:MAX)` with `acc + 1`):
    interp = Timeout — the overflow at iteration 1 errors→bottoms in
    node/op.rs, the tail-call argument never arrives, the loop stalls
    forever; jit = completes with the correctly WRAPPED value
    (MIN + 499999). CLAUDE.md's fusion section documents "unchecked
    wraps" — which matches the JIT, not the canonical node-walk. Either
    the node-walk should wrap (change the op/netidx add) or bottoms are
    canonical and every JIT unchecked add needs an overflow check (perf
    cost on the hottest op). Needs a ruling — also note the stalled tail
    loop is itself an un-interruptible wedge shape (ties into item 4).

15. **Nested try/catch over-fire** (divergence_000030.gx, fuzz campaign):
    nested `try try (a[MIN]? /? a[0]?)?; a[6]? catch(e) => select
    e.0.error {...  err1 <- e ...} catch(e) => err0 <- e; [err0, err1]`
    — interp emits NOTHING (both err vars never fire → the producer
    never fires), jit emits an array at cycle 2. The fused region around
    the TryCatch/catch-read seam fires when the node-walk wouldn't.
    Unanalyzed beyond classification — queue for a focused session.
