# Env-Independent TypeRefs: Carried Resolution State

Status: **built (2026-07-14).** Eric's ruling: `TypeRef` carries an
Arc-Mutex resolution cell — resolve the *name once* while the right env
is at hand, keep the *structure* lazy. Replaces the eager structural
expansion (`resolve_internal_type`) that static-instance typechecking
(`3983453c`) used to buy env-independence, which destroyed
name-compression and caused the `type_operation_scaling.md` campaign
and its open `contains` residual.

## The defect this fixes

`TypeRef` was `(scope, name, params)` — a *query* evaluated against
whatever env the consumer holds. A retained instance signature outlives
the def env, and later consumers' envs answer the query differently
(interface names resolve to the PUBLIC `Type::Abstract` outside the
defining module and to the private concrete form inside; transient
scopes vanish). Eager expansion made signatures env-independent by
inlining everything — at widget-union scale, tree-sized types per call
site.

## The cell

`TypeRef.resolved: Arc<Mutex<Option<Arc<ResolvedRef>>>>` —
`ResolvedRef` snapshots exactly what `lookup_ref` reads from the env
via `find_visible`: the def's formal params+constraints, body `Type`,
canonical scope, and def pos/ori (lsp `TypeRefSite` recording survives
cache hits). Substitution (`replace_tvars`), arity check, and
constraint registration stay per-call — pure given the snapshot.

- **Write-once**: filled on first successful `lookup_ref`; never
  overwritten (clones share the cell — refilling would leak one
  context's view into every aliasing type). Contexts needing a
  different view REBIND: a rebuilt ref with a fresh pre-filled cell
  (`TypeRef::rebind_resolution`).
- **Lock discipline**: the snapshot is computed WITHOUT the cell lock
  (resolution re-enters through constraint checking; `deref_typ` holds
  TVar guards) — compute, then lock-check-store.
- **Identity-excluded** like pos/ori (Eq/Hash/Ord untouched);
  `#[pack(skip)]` → decoded refs have empty cells and re-resolve in
  the loading env (correct: a packed interface's names mean the public
  view there).
- **Params-independent**: the cell caches the NAME resolution, so
  param-substituting rebuilds SHARE it (`with_params` — used by
  normalize/resolve_tvars/union and, load-bearing, by
  `reset_tvars`/`replace_tvars`, since `RefHist::expand_ref` commit
  copies must keep seeded cells). `scope_refs` changes the resolution
  → fresh cell (`with_scope`). The field is `pub(in crate::typ)`;
  outside construction goes through `TypeRef::new`/`synthetic`, so a
  fresh cell is compiler-enforced.
- Known accepted leak: a recursive typedef's filled self-ref cell is
  an Arc cycle (triomphe has no Weak) — bounded per definition,
  accumulates only under dynamic-module redefinition churn.

## Seeding: lazy by default, explicit only where timing is provably safe

A fill is correct iff the resolving env already contains the name's
FINAL target. Mid-compile envs are truncated by registration order, so
**eager transitive seeding is wrong by construction**: seeding a union
body at the time some earlier walk touches it resolves sibling names
too early (tui's `` `List(list::List) `` member — meaning the
tui::list SUBMODULE's type — captured the list PACKAGE's `List<'a>`
during an earlier sibling's def gate; both an initial
LambdaDef-creation seed and a fill-⇒-transitively-seeded invariant on
`resolve_in` shipped this bug and were REMOVED). Lazy expansion is
order-correct in practice: tag discrimination and the Ref×Ref name
fast paths defer nested-ref expansion until a walk genuinely needs it,
which happens at typecheck time under the full env.

What remains:

1. **Opportunistic fill-on-lookup** (`resolve_in`): fills ONLY the
   looked-up ref, at genuine-use time.
2. **`Type::seed_refs`** (query-style walk, permanent visited set over
   composite addresses + ref/tvar cells, recursing through filled
   cells' snapshots) — invoked ONLY where the timing is provably safe:
   `check_sig` seeds the abstract registry's private body copy against
   the module's private env (check_sig runs AFTER the module body, so
   all module names are registered, and under the OUTER env, where
   those names would mean the sig entries); the privatize walk seeds
   its rebound snapshots (program typecheck1 — everything registered).

## Same definition vs same view

Interface typedef bodies are registered TWICE (`bind_sig` into the
outer env; the resolver-injected copy compiled into the module env) —
structurally equal allocations whose nested refs fill from different
envs (abstract outside, concrete inside). Two identity tests:

- `ResolvedRef::same_def` — structural (content Arcs shared for one
  `TypeDef`, so it shortcuts to pointer compares). Used by
  `TypeRef::cells_agree`, which gates the Ref×Ref name-equality fast
  paths in `contains`/`union`/`diff`/`could_match`: name equality no
  longer implies same meaning (REPL redefinition, cross-env views), so
  disagreement falls through to the expansion arms.
  (`sig_matches_int`'s fast path is deliberately NOT gated — sig-vs-
  impl matching *intends* to relate the two views of one name.)
- `ResolvedRef::same_view` — body ALLOCATION identity. Structural
  equality is blind to nested cells; the privatize walk (below) must
  rebind on VIEW divergence, so it uses this stronger test.

`RefHist::ref_id` derives ref identity from the filled cell's Arc
address (fallback: env `TypeDef` address) — an escaped ref unresolvable
in the ambient env no longer collapses to the `None` cycle key, and an
old-cell ref never aliases a redefined def's identity.

## The privatize walk (replaces `resolve_internal_type`)

`fusion::lowering::privatize_type(reg, typ, env, scope)` —
NAME-PRESERVING, output the same size as the input, no memo/budget
machinery. Callers: `setup_static_bind` (env = def's `f.env`) and
`check_instance_type`'s AbstractOpaque retry (env = `ctx.env`, scope =
the lambda's fn{id} scope — passes the registry's visibility gate).

- `Type::Ref`: the walk env's view wins over the cell ONLY for a
  same-definition, different-VIEW divergence (`same_def(e,c) &&
  !same_view(e,c)`): rebind to a fresh cell holding the env's
  allocation, seeded from that env. This re-points a site ref that
  arrived through the caller's sig-registered allocation (public view
  — whose body's nested refs leak abstracts into the instance body:
  "expected struct not abstract") at the def's private view. A
  DIFFERENT definition in the env is a stale-horizon artifact (`f.env`
  is a mid-registration snapshot; a forward cross-submodule name
  resolves there to an outer shadow) — the cell, filled
  post-registration, is the name's true meaning and wins. Names the
  env can't see keep their cells (the old swallowed `UnresolvableRef`
  class). An empty cell REBINDS rather than fills (the shared cell may
  alias caller-held types). Then, a resolution whose body is
  `Type::Abstract{id}` visible at `scope` is re-pointed at the
  registry's private body TEMPLATE
  (`AbstractRegistry::internal_template` — formal names +
  ref-compressed body; the ref's own params keep substituting through
  `lookup_ref`).
- `Type::Abstract{id, params}`: scope-gated one-level registry body
  substitution, recursing for nested abstracts under an `AbstractId`
  cycle guard.
- TVar: deref-and-recurse (clone out of the guard — lock discipline);
  everything else COW.

**Deleted**: `resolve_internal_type`, `DefResolveCache` (+
`LambdaDef.resolve_cache`), and the shared-cache plumbing in
`ResolveCx`. The fusion-side `resolve_abstract` (capped, full
expansion for ABI sizing) is unchanged — its Ref arm's `lookup_ref`
now short-circuits through cells, fixing the it-ran-under-the-wrong-env
hazard for refs that reach fusion.

## Fusion: refs at the freeze boundary

Instance signatures now stay ref-compressed, so `Type::Ref` reaches
`freeze_for_abi`/`abi_kind` through raw-frozen node types (DynCall
slot derivation, casts, collection intrinsics, emit-time freezes).
Both classifiers gained a Ref arm expanding through the FILLED cell
(`TypeRef::expand_cell` — env-free substitution of the ref's params
into the snapshot body), `Seen`-guarded like the Abstract arm; an
empty cell or recursive named type is `None` = de-fuse, the pre-cell
behavior.

## Measurements (2026-07-14, debug build)

- GUI wedge (`empty_table` + `context_menu_renders`): **0.68s** —
  BETTER than the 0.92s pre-instance-check baseline, with full eager
  instance checking (eager-expansion design: 2.79s/157MB; unmitigated:
  41GB OOM).
- The ~8 `contains`-hung GUI interaction tests: **all pass in ~2.3s**
  — full GUI suite 163/163 (4 ignored) in ~4.9s. The exponential
  Set⊇Set walks are gone because depth stops at names: the Ref×Ref
  fast path, `ref_id` expansions memo, and `same_content` prune at
  every named level.

## Surfaced finding (needs Eric's ruling)

`list::fold`/`list::map` call sites now STATICALLY BIND where they
previously discarded (the old expanding resolver tripped AbstractOpaque
on the abstract `List`) — an improvement that un-masked two tests:
`native_fold_callback_fusable_ok` / `native_hof_callback_fusable_ok`
asserted `#[native]` inside List-HOF callbacks and passed VACUOUSLY
(the callback body never existed in the compile-time graph, so the
checker never reached it — verified against a HEAD worktree). In the
current architecture, interpreted List/Map HOFs dispatch per-slot live
CallSites that bind dynamically and interpret — callback bodies never
run fused, so an honest `#[native]` there fails. Options: (a) make
collection callback instances actually fuse (MapQ/FoldQ `Update::fuse`
descending into the prototype + per-slot reuse of the cached kernel —
a real feature), or (b) re-annotate the two tests. Left failing
honestly pending the ruling.
