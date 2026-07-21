# Type-copy discipline and single instantiation

> Eric's principle (2026-07-12): **be extremely skeptical of every deep
> copy of a type. A copy made while any type checking is still pending
> forks live inference state — facts recorded after the fork land on
> one side only, and no later check can see both.** Every copy site
> must justify itself against a quiescence boundary: the source's
> inference is finished, its facts are frozen (bindings + cell
> conjuncts), and the copy is a fresh instantiation that will never be
> confused with its sibling copies.

## Why this became urgent

The instance-elaboration acceptance family (site_recheck_strictness.md:
null-through-i64-slot, cross-type compares, LambdaDef-pointer arith,
mixed-numeric accs — ~90% of soak findings by volume, one of them an
address leak) all reduce to forked type state: the call site held
SEVERAL instantiation copies of the callee's ftype, the source's
`elem := null` fact landed in one copy's cells and the predicate's
`cmp := i64` fact settled another, and no cell ever held both. Strict
enforcement (in any position) cannot catch a contradiction that never
materializes in one place.

## The fork vectors (inventory, 2026-07-12)

### V1 — vacuous frozen×frozen unification (FIXED)

`contains`' TVar×TVar arm, both unbound, both frozen: returned
`Ok(true)` WITHOUT linking the cells. `frozen` means "this var already
joined a name-alias group" (`TVar::alias` self-marks and refuses
re-entry) — it was never a statement about unification. Two settled
groups whose types are equated must share one cell from that moment on,
or they fork. Fixed by `Act::CellMerge` → `TVar::alias_cells` (cell
merge bypassing the frozen gate, keeping alias()'s occurs checks),
committed under `AliasTVars` like every other aliasing act.

This single change makes the b4/f3 null witnesses compile-reject in
BOTH modes — consistent with their direct forms — because the site's
facts now collide in one cell and the strict arg-boundary check fires.

### V2 — `reset_tvars` call sites (the deep copies)

| site | verdict |
|---|---|
| `callsite.rs` site instantiation | LEGAL — the def→site boundary; the def gate has closed, body facts live as cell conjuncts (constrain_known), bindings are solved facts. This is THE instantiation. |
| `genn.rs:76` builtin generic ftype | LEGAL — same boundary. |
| `contains.rs` settle witness | LEGAL — copies the constraint STORE's type; the store is finished by construction (and the copy is what keeps live inference from writing into the store). |
| `fntyp.rs` replace_auto_constrained | LEGAL — display only. |
| `forloop.rs:597` `self.body.typ().reset_tvars()` | **SUSPECT** — re-mints the For body's type mid-inference (the "analysis instance re-mints generalized tvars unbound" note). Needs justification or replacement with a read of the one site instance. |

### V3 — binding copies (`TVar::copy`, reset's bound-cell propagation)

Copying a BINDING is copying a fact — safe if the fact is final and the
receiving cell's conjuncts admit it. `TVar::copy`'s doc assumes "the
bind-site check has already verified the incoming binding"; on the
instantiation path (`reset_tvars_int` writing `fresh.typ = Some(t)`)
no such check runs. Latent, unwitnessed since V1's fix — hardening
candidate: validate copied bindings against the fresh cell's seeded
conjuncts where an env is available (the instantiating call site).

## The enforcement stack (as landed)

1. **`('a,'a)` comparisons** (ef90ee98) — probe both directions
   unbound, commit the widening one, reject disjoint. Kills the
   semantic nonsense (cross-type compares under discriminant order)
   and the emit's mixed-prim hole.
2. **Strict arg-boundary instance checks** (ef90ee98) — each arg's
   type must be contained by the instance's resolved formal. Two
   structured artifact classes swallowed: `UnresolvableRef` (name
   resolution under a dead (env,scope) context) and `AbstractOpaque`
   (private↔public equivalence exists only via name resolution inside
   the defining module).
3. **V1 CellMerge** (this change) — makes the facts collide so (2)
   actually fires.

The full-BODY instance recheck stays swallowed: it compares partial
inference states and is artifact-prone (param_knot's union-vs-narrowed
rtype; copy-skewed lineages). Revisit after the arith change below.

## Remaining plan

1. **Promoted result types for mixed concrete arith.** Today
   `i64 + f64` types as the union `[i64, f64]`; the runtime ALWAYS
   promotes (netidx apply_op — f64). The union is a static lie in the
   conservative direction and the direct cause of the m6 mixed-acc
   witness surviving (acc i64 vs union is "compatible enough" through
   the copy chain) and of param_knot's full-strict artifact. When both
   operands are concrete single numerics and `typ::numeric_absorbers`
   gives a direction, the static result should be the absorber;
   ambiguous pairs keep the union. Closes the mixed-acc class as a
   compile error in both modes.
2. **Runtime-dispatch strictness policy** (needs a ruling): a
   late-bound lambda whose instance fails the arg check at dispatch —
   error the dispatch (the queuefn family says some legit flows trip
   this) or de-fuse and let the interp run dynamically (safe: no
   kernel is built from the lying types, so no divergence is
   possible)? Current state: swallowed at the runtime path like
   before.
3. **V2/V3 hardening** — justify or fix `forloop.rs:597`; conjunct
   validation on copied bindings.
4. **Single authoritative instantiation + instance inlining** (the P4
   arc). CellMerge de-fangs the skew when copies MEET, but copies that
   are never unified against each other still drift (the For analysis
   instance). One instance per (site, callee), owned by the CallSite,
   read by init/analysis/freeze/fusion alike — and the inlining item
   consumes the same instance for its monomorphic body. This is the
   architecture end-state; V1–V3 make it a perf/legibility arc instead
   of a correctness emergency.
