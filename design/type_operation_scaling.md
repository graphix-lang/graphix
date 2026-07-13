# Type Operations Must Scale as DAGs, Not Trees

Status: proposed (direction agreed 2026-07-13; supersedes the size-cap /
budget stopgaps added the same day, which are to be REMOVED by phase 5).

## The defect

Graphix types are DAGs: structural nodes share `Arc`s, TVar cells are
shared by construction, and named `Ref`s/abstracts point at common
definitions. Every core type operation nevertheless walks and rebuilds
them as trees:

- `Type::resolve_tvars` deep-clones unconditionally — every call severs
  all sharing, and `setup_static_bind` calls it per static call site.
- `fusion::lowering::resolve_abstract_d` re-expands every occurrence of
  the same named type independently (the 2026-07-13 session added a
  per-call memo, which fixed compute within one call but not across
  calls, and not retention).
- `Type::normalize` / `flatten_set` re-walk shared subtrees per
  occurrence; the merge sweep restarts from `(0,0)` on every successful
  merge (cubic in set width), and `merge` re-flattens nested sets per
  comparison.
- Resolved instance signatures are retained per call site as
  independent deep copies.

Tree-cost equals DAG-cost only while types are small. The GUI widget
union is the first type population deep enough for the costs to diverge,
and they diverged everywhere at once: `3983453c` (static-instance
typechecking — the right design) put these operations on the
per-call-site hot path and the GUI package went from compiling in
under a second to a 41GB OOM.

The interim mitigations (resolution budget, unfolded-size caps, the
`setup_static_bind` bind-unresolved fallback, the
`freeze_for_abi_normalized` size gate) bound the damage by *declining
the work*. Fusion-side that is semantically safe (refusal = de-fuse),
but instance-side it creates a tractability threshold in typing
behavior — a hack. This plan makes the work cheap so the thresholds can
be deleted.

## Phase 1 — sharing-preserving rebuild walks (the keystone)

Every rebuilding walk (`resolve_tvars`, `normalize`,
`resolve_abstract_d`, and friends) returns the ORIGINAL `Arc` when the
operation changed nothing beneath it, instead of reconstructing an
identical tree. Mechanically: each recursive site notices whether any
child came back pointer-identical, and if all did, clones the input Arc
rather than `from_iter`-ing a new node.

Consequences: resolution/normalization of a type that is mostly
already-concrete (the overwhelmingly common case) allocates almost
nothing; retained instance signatures share structure with their
sources and with each other; downstream pointer-identity memos (phase
2) get stable, shared addresses to key on.

Verification: the new walks must be extensionally identical to the old
ones — differential-test old vs new implementations over the full test
corpus plus proptest-generated types, asserting structural equality of
results.

## Phase 2 — pass-local pointer-identity memoization

`normalize` and `resolve_abstract_d` process each shared subtree once
per pass: an Arc-pointer-keyed memo alongside the existing cell-keyed
`seen` sets. Sound because these passes are pure with respect to a
snapshot of TVar bindings (neither mutates bindings). With phase 1 in
place, hit rates are high because sharing survives.

Additionally, `contains`/equality get an Arc-pointer fast path
(identical Arc ⇒ same type ⇒ contains/eq trivially true), which keeps
instance boundary checks (`check_instance_type` →
`check_contains_rigid`) cheap over the now-shared resolved signatures.
`contains` binds TVars, so it cannot be memoized wholesale — the
pointer fast path is the sound subset.

## Phase 3 — definition-level resolution caching

The private resolution of a named type / abstract in a given scope is a
property of the DEFINITION, not the call site. Cache it once at the
definition level (registry or `ExecCtx`), keyed by
`(TypeRef identity | AbstractId, params, scope)`, so N call sites of
the same signature share one resolved skeleton. Invalidation follows
the same lifecycle discipline as `bind_to_lambda` (the jul12 lesson:
prune per-batch by what actually changed, never clear wholesale).

This is the same shape as the existing kernel cache
(`(LambdaId, resolved FnType)` in `build_lambda_kernel`) — instance
artifacts are deterministic functions of definition + site types, and
should be computed once per distinct instantiation, not once per site.

## Phase 4 — `flatten_set` / `merge` algorithmics

- Establish and enforce the invariant that `flatten_set`'s accumulator
  members are never `Set`s (the iterator expansion already guarantees
  it), so `merge` need not re-flatten at nested positions whose
  children were already normalized.
- Replace the restart-from-zero pairwise sweep with a worklist fixpoint
  (a successful merge re-enqueues only the merged element), making the
  already-normal case O(n log n) after a sort and the worst case O(n²)
  without the cubic restart.

## Phase 5 — remove the stopgaps

With phases 1–4 in: revert `setup_static_bind` to unconditional eager
private resolution (delete the size-cap fallback), delete the
`freeze_for_abi_normalized` size gate, and shrink the resolver budget
back to its one principled role — the non-regular-recursion backstop
(`type T<'a> = T<Array<'a>>`), which predates this episode and stays.

Acceptance:
- GUI test suite at or under its pre-`3983453c` cost (baseline: the
  `empty_table` + `context_menu_renders` pair ran in 0.92s at
  `f10d343f`), with peak RSS in the hundreds of MB, not GB.
- The interface suite (abstract-in-variant, parameterized-nested), the
  full workspace, and the fuzz regression corpus green.
- A deterministic work-counter regression test: compile a widget-scale
  fixture and assert the resolver/normalize work counters stay under
  fixed bounds (counters, not wall-clock — no flaky timing tests).
- A fresh soak.

## Explicitly out of scope (for now)

Full hash-consing/interning of `Type` (O(1) equality, global structural
sharing) would subsume phases 1–3, but it touches every corner of the
compiler and interacts with mutable TVar cells. Phases 1–4 capture the
asymptotics without the representation change; interning remains a
possible later consolidation once these walks are already
sharing-shaped.

## Non-problems (measured, not assumed)

Per-instance BODY typechecking is not the bottleneck: with resolution
bounded, the GUI wedge tests run in 0.64s **including** full instance
body checks. The cost was always in the type-operation walks. If
phase-5 measurement disagrees after eager resolution returns, revisit —
the instance-check cache of phase 3 extends naturally to body-check
results.
