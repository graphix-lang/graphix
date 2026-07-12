# The typing-acceptance family needs a ruling on the elaboration typing model

Status 2026-07-12: three open jul10h finds (000001/000002/000010, all
one family), root-caused, with a FAILED strictness experiment fully
documented below. The JIT side is SAFE (defensive shape-check bottoms
+ the frame-taint fix); the residual is interp-vs-jit divergence on
programs whose typing status is the actual question.

## The acceptance hole (proven, GRAPHIX_DBG_BIND)

```graphix
{let g = |a: i64, b: i64, c| (|n, _| n == i64:3)((a, b), c); g(i64:10, i64:20, i64:5)}
```

- The def gate checks the inline lambda's body once and binds
  `'n := i64` (from `n == i64:3`) — then `unbind_tvars` (lambda.rs
  def gate exit) discards ALL gate bindings for generalization.
- The site's arg check binds a FRESH copy `'n := (i64, i64)`.
- The two facts live in disjoint cells (`'_18048` at two addresses in
  the bind trace); no check ever sees both → ACCEPTED.
- The body is never re-checked per site with the site's cells — well,
  it IS: `setup_bind` runs `rf.typecheck0` on the fresh instance
  against the site-resolved cells, which catches exactly this
  ("type mismatch i64 does not contain (i64, i64)") — **and swallows
  it** (`let _ =` since inception; GXDBG_SWALLOW=1 prints it).
- Consequence: the JIT freezes the body-fact type (scalar I64 slot),
  the runtime marshals the site's tuple, the defensive param check
  bottoms → jit quiet, interp runs heterogeneous `==` → `false`.
  Divergence, and arguably a program that should never have compiled.

Variants: 000010 (string element vs `|x| x == i64:4` through
`array::map` — same mechanism through the runtime-bind path);
000001 (`'a: Number |x| x + f64:0.` instantiated at i64 AND f64 —
same disjoint-cell mechanism, but see the promotion question below;
its interp value `5.500000000002` carries f64 dust that suggests an
ADDITIONAL value-level bug worth chasing once typing is settled).

## The strictness experiment (implemented, validated, REVERTED)

Making `setup_bind`'s instance tc0 strict on the compile-time
`resolve_static` path rejected all three finds (both modes
compile-fail → fuzzer AGREE) and survived regress/detcheck/corpus —
but collided with three boundaries:

1. **Cross-module private views.** The instance body bakes the def
   module's PRIVATE types into its node types: gui's `Color` is a
   struct inside the package impl and abstract outside — 11
   gui-package tests spurious-failed under the caller's env. Running
   the re-check under the def's env snapshot (`with_restored(f.env)`)
   fixed those but broke the inverse (a test module's `SortBy`
   doesn't exist in the def env — 2 data_table tests). EITHER-view
   acceptance (caller env, then def-env retry on a REBUILT instance)
   greened gui — but the interface-test family
   (`abstract_type_map_key` etc.) fails under BOTH envs: the
   abstract registry's expansion is SCOPE-gated, not env-gated
   ("Map<i64,string> does not contain Map<Key,string>" from either
   side of the boundary).
2. **Same-scope narrowing was almost enough.** `.gxi` interfaces
   require full annotations, so the unannotated-formal hole is only
   expressible for local lambdas; gating strictness on
   `f.scope.lexical == self.scope.lexical` greened every interface
   and gui test. But —
3. **The pinned promotion semantics is the real fork.**
   `param_knot_no_leak` (lang/functions.rs) PINS that
   `'a: Number |x: 'a| -> 'a x + i64:1` accepts BOTH `f(i64:3)` and
   `f(f64:2.5)` — numeric promotion across monomorphizations is
   intended. The site re-check rejects the f64 site ("f64 does not
   contain '_N: [i64, f64]"): it cannot distinguish
   promotion-flexible operand facts (legal) from genuinely
   conflicting structural facts (the tuple case). Under promotion
   semantics, 000001 is a LEGAL program and the experiment's
   "AGREE" was both modes rejecting it.

## The questions for Eric

1. Is `(|n,_| n == i64:3)((a,b), c)` ill-typed (reject at compile) or
   dynamically-false (`==` between unrelated shapes = false)? The
   direct form `"a" == i64:1` is REJECTED today (`lhs ⊇ rhs`), so
   consistency says reject — but then the site re-check needs to
   fire, and it must NOT fire on promotion-flexible arith facts.
   Distinguishing them likely means the re-check consults the same
   promotion lattice the ops' typechecks use, or the def gate stops
   discarding STRUCTURAL body facts (unbind only promotion-flexible
   cells?).
2. What is the typing model for a per-site instance body that
   crosses a module boundary (the abstract-scope problem)? Today it
   is simply unchecked (swallowed). This blocks any strictness for
   cross-module shapes — probably fine (interfaces are fully
   annotated) but worth stating.
3. Until ruled: the three finds stay open divergences. The kernel's
   defensive param bottom keeps the JIT safe; the interp's
   heterogeneous-`==`-false is v1 behavior.

Probe: scratchpad u1_tuple_arg.gx (in-repo copy:
`{f64:0.; let g = |a: i64, b: i64, c| (|n, _| n == i64:3)((a, b), c); g(i64:10, i64:20, i64:5)}`).
GXDBG_SWALLOW=1 shows the swallowed error on any run.

## UPDATE 2026-07-12 (second session): nested constrain_known RESTORED — most of the family fixed

Eric's architecture memory checked out. `constrain_known` records
def-gate facts as cell conjuncts; the depth-1 gate (8630436f) was
skipping every nested lambda. Restored with a `closed_only` mode for
nested gates (facts with open interior cells stay unrecorded — the
entanglement scoping that motivated the depth gate; the open-leaf
snapshot fix alone was not sufficient, an `Array<'b-unbound>` fact
still perturbed downstream instances). Result:

- u1 / 000002 / 000010: REJECTED consistently in both modes ✓
- jul12a crash_000001 (SIGSEGV: an unannotated RETURN type bound to
  `Array` by the site while the body delivers i64 — the JIT deref'd
  payload 35 as a ValArray pointer): REJECTED in both modes ✓ —
  the acceptance hole was memory-unsafe, not just divergent.
- firing-jul2026/03 regression: NOT actually caused by this change —
  on HEAD both modes were MUTUALLY silent (the pin passed as [] vs
  []). Root cause was empty literals (`let res: Array<'a> = []`)
  producing only at `event.init`: a per-site instance's seed died
  under frame resets and its For bottomed on the missing init. Fixed
  by extending the Constant frame rule (STALE production at
  frame_depth > 0) to all zero-part producers (array/tuple/struct/
  map literals). 03 now emits at cycles 1 AND 2 in both modes —
  correct under P4 semantics (in-language map = fold-with-push; the
  body consumes the acc, so source fires drive it; the recorded
  single emission was MapQ-era).

## The REMAINING ruling: the promotion lie (jul10h 000001 + jul12a 000002)

`'a: Number |x: 'a| -> 'a x + f64:0.` — under 5634fbdc's rigid
suppression the operand pre-bind is suppressed, so the `+` statically
returns 'a — but the node-walk's arith PROMOTES, so at an i64 site
the runtime value is F64 where the static type (and the JIT's frozen
slot) say i64. The interp now computes correct VALUES (f64 leaks
through 'a-typed positions benignly in the node-walk); the JIT's
defensive shape check bottoms → permanent divergence on accepted
programs. 5634fbdc flagged exactly this class for review.

Options:
(a) Record the suppressed operand as an OBLIGATION on 'a that sites
    validate in the PROMOTION order (not set containment):
    `x + f64:0.` ⟹ 'a ≽ f64 → i64 site rejected, f64 site fine;
    `x + i64:1` ⟹ 'a ≽ i64 → both sites fine (param_knot_no_leak
    stays green). Needs a new constraint kind (promotion-ordered).
(b) The JIT widens 'a-returning bodies containing suppressed
    pre-binds to value-shape slots (accept the dynamic typing,
    keep the node-walk semantics).
