# Env-independent TypeRefs: carried resolution cells

Status: built 2026-07-14
Pins: `stdlib/graphix-package-gui/src/test/data_table_test.rs` (`empty_table`), `stdlib/graphix-package-gui/src/test/widgets_test.rs` (`context_menu_renders`), `graphix-fuzz/src/lib.rs` (`check_mode_parity`)

## The defect

`TypeRef` was `(scope, name, params)` — a *query* evaluated against
whatever env the consumer holds. A retained instance signature outlives
the def env, and later consumers' envs answer the query differently
(transient scopes vanish; a name means different things in different
modules). Static-instance typechecking first bought env-independence by
eagerly expanding every ref into structure, which destroyed
name-compression: at widget-union scale a signature became a tree-sized
type per call site (the 41GB GUI wedge and the exponential `contains`
residual of `type_operation_scaling.md`).

The ruling: resolve the *name once*, while the right env is at hand,
and keep the *structure* lazy.

## The cell

`TypeRef.resolved: Arc<Mutex<Option<Arc<ResolvedRef>>>>` (`typ/mod.rs`).
`ResolvedRef` snapshots exactly what `lookup_ref` reads from the env
via `find_visible`: the def's formal params and constraints, the body
`Type`, the canonical scope, and the def's pos/ori (so `TypeRefSite`
recording survives cache hits). Substitution (`replace_tvars`), the
arity check and constraint registration stay per call — pure given the
snapshot.

- **Write-once.** Filled on the first successful `lookup_ref`, never
  overwritten: clones share the cell, and refilling would leak one
  context's view into every aliasing type.
- **Lock discipline.** The snapshot is computed WITHOUT the cell lock
  (resolution re-enters through constraint checking, and `deref_typ`
  holds TVar guards); compute, then lock-check-store.
- **Identity-excluded**, like pos/ori (Eq/Hash/Ord untouched).
  `#[pack(skip)]`: a decoded ref has an empty cell and re-resolves in
  the loading env, which is correct — a packed interface's names mean
  the public view there.
- **Params-independent.** The cell caches the NAME resolution, so
  param-substituting rebuilds SHARE it (`TypeRef::with_params`, used by
  normalize/resolve_tvars/union and, load-bearing, by
  `reset_tvars`/`replace_tvars` — the expansion-commit copies in
  `RefHist` must keep seeded cells). A scope change changes the
  resolution, so `scope_refs` mints a fresh cell (`with_scope`). The
  field is `pub(in crate::typ)`; outside construction goes through
  `TypeRef::new`/`synthetic`, so a fresh cell is compiler-enforced.
- **Accepted leak.** A recursive typedef's filled self-ref cell is an
  Arc cycle (triomphe has no Weak) — bounded per definition, growing
  only under dynamic-module redefinition churn.

## Seeding: lazy, plus one eager pass at the order-correct moment

A fill is correct iff the resolving env already holds the name's FINAL
target. Mid-compile envs are truncated by registration order, so eager
transitive seeding is wrong by construction: seeding a union body when
some earlier walk touches it resolves sibling names too early (a
`` `List(list::List) `` member meaning the tui `list` SUBMODULE's type
captured the list PACKAGE's `List<'a>` during an earlier sibling's def
gate; both a LambdaDef-creation seed and a fill-implies-transitively-
seeded invariant shipped this bug and were removed).

What remains:

1. **Opportunistic fill-on-lookup** (`resolve_in`): fills only the
   looked-up ref, at genuine-use time. Tag discrimination and the
   Ref×Ref name fast paths defer nested expansion until a walk needs
   it, which happens at typecheck time under the full env.
2. **`Env::seed_typedef_refs`** walks every typedef body (and abstract
   rep) through `Type::seed_refs` — a query-style walk with a permanent
   visited set over composite addresses and ref/tvar cells, recursing
   through filled snapshots — right before fusion, after typecheck.
   That is the one order-correct moment: every name's final target is
   registered. It exists because a recursive type's INNER occurrence is
   reached by no typecheck walk (the Ref×Ref fast path answers without
   expanding) and fusion must expand it env-free. It runs in both
   modes, so a pass the fusion gate owns never changes what the
   typechecker sees.

## Same definition

Name equality no longer implies same meaning (REPL redefinition,
cross-env views), so the Ref×Ref name fast paths in `contains`/`union`/
`diff`/`could_match` are gated by `TypeRef::cells_agree`, which uses
`ResolvedRef::same_def` — structural, shortcutting to pointer compares
because the content Arcs are shared for one `TypeDef`. Disagreement
falls through to the expansion arms. `sig_matches_int`'s fast path is
deliberately NOT gated: sig-vs-impl matching intends to relate two
views of one name. A DIFFERENT definition found in a stale env is a
horizon artifact; the cell, filled post-registration, is the name's
true meaning and wins.

`RefHist::ref_id` derives a ref's identity from the filled cell's Arc
address (fallback: the env `TypeDef` address), so a ref unresolvable in
the ambient env never collapses to a shared `None` cycle key, and an
old-cell ref never aliases a redefined def's identity.

## Fusion: refs at the freeze boundary

Instance signatures stay ref-compressed, so `Type::Ref` reaches
`freeze_for_abi`/`abi_kind` through raw-frozen node types. Both
classifiers expand through the FILLED cell (`TypeRef::expand_cell` —
env-free substitution of the ref's params into the snapshot body),
`Seen`-guarded like the Abstract arm; an empty cell or a recursive
named type is `None` = de-fuse. The fusion-side `expand_refs`
(`fusion/lowering.rs`; capped, expanding, env-backed) is the pre-pass
for kernel-signature derivation; its Ref arm short-circuits through
cells, so it can no longer run under the wrong env.

## Measurement (debug build, at landing)

- GUI wedge (`empty_table` + `context_menu_renders`): 0.68s with full
  eager instance checking — better than the 0.92s pre-instance-check
  baseline (eager expansion: 2.79s/157MB; unmitigated: 41GB OOM).
- The ~8 GUI interaction tests that hung in `contains`: all pass in
  ~2.3s; the full GUI suite in ~5s. The exponential Set⊇Set walks are
  gone because depth stops at names — the Ref×Ref fast path, the
  `ref_id` expansion memo and `same_content` prune at every named
  level.
