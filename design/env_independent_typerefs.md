# Env-independent TypeRefs: carried resolution cells

Status: built 2026-07-14
Pins: `stdlib/graphix-package-gui/src/test/data_table_test.rs` (`empty_table`), `stdlib/graphix-package-gui/src/test/widgets_test.rs` (`context_menu_renders`), `stdlib/graphix-tests/src/lang/fusion_parity.rs`

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

`TypeRef.resolved: Arc<Mutex<Resolution>>` (`typ/mod.rs`): `Open`,
`Def(Weak<ResolvedRef>)` or `Trait`. `ResolvedRef` snapshots exactly
what `lookup_ref` reads from the env via `Env::resolve_type_name`: the
def's formal params and constraints, the body
`Type`, the canonical scope, and the def's pos/ori (so `TypeRefSite`
recording survives cache hits). Substitution (`replace_tvars`), the
arity check and constraint registration stay per call — pure given the
snapshot.

- **Write-once.** Filled on the first successful `lookup_ref`, never
  overwritten: clones share the cell, and refilling would leak one
  context's view into every aliasing type. A name that means a trait (a
  bound) fills the cell with the trait (`Resolution::Trait`) when the
  seed walk reaches it, at a definition's check; image format 35 carries
  that state.
- **Lock discipline.** The snapshot is computed WITHOUT the cell lock
  (resolution re-enters through constraint checking, and `deref_typ`
  holds TVar guards); compute, then lock-check-store.
- **Identity-excluded**, like pos/ori (Eq/Hash/Ord untouched).
  The syntax codec skips the cell: a decoded ref has an empty cell and
  re-resolves in the loading env, which is correct — a packed
  interface's names mean the public view there. A session image carries
  the cell (`image::refcell_len`, `TypeRef`'s `PackTrait` under
  `image::is_encoding`).
- **Params-independent.** The cell caches the NAME resolution, so
  param-substituting rebuilds SHARE it (`TypeRef::with_params`, used by
  normalize/resolve_tvars/union and, load-bearing, by
  `reset_tvars`/`replace_tvars` — the expansion-commit copies in
  `ContainsHist` must keep seeded cells). A scope change changes the
  resolution, so `scope_refs` mints a new cell (`with_scope`), filled
  from the source's cell when that is filled (a filled cell is the
  name's final target, which the new scope may not even reach). The
  field is `pub(in crate::typ)`; outside construction goes through
  `TypeRef::new`/`synthetic`, so a fresh cell is compiler-enforced.
- **Weak, owned by the definition.** A recursive typedef's body
  reaches its own cell once seeded, so a strong cell is a cycle. The
  `TypeDef` holds its `ResolvedRef` (a `std::sync::Arc`, the one
  allocation per definition that every resolution hands out) and a
  cell holds a `Weak`. A cell whose definition is gone, a type that
  outlived the env entry that defined it, is refused by `resolve_in`
  and logged, never re-resolved: the name may mean something else now.
  In an image the definition is an object by identity; a cell met
  while its definition is still decoding takes the `Weak` of an
  `Arc::new_cyclic` under construction, and a dead cell travels dead.
  Pins: `env::test::recursive_typedefs_free_with_their_env`,
  `image::tests::typedef_cells_decode_onto_their_definition`.

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
   it, which happens at check time under the full env. Elaboration is
   not under it: an instance elaborates under its definition's env,
   where a type its signature carries in from the caller's scope does
   not resolve by name.
2. **`Env::seed_typedef_refs`** walks every typedef body (and abstract
   rep) through `Type::seed_refs` — a query-style walk with a permanent
   visited set over composite addresses and ref/tvar cells, recursing
   through filled snapshots — at the check's end, before elaboration,
   and again right before fusion. The check's end is the order-correct
   moment: every name's final target is registered. It exists because a
   recursive type's INNER occurrence is reached by no check walk (the
   Ref×Ref fast path answers without expanding), and elaboration (a
   collection instance's prototype call under `/array`, pin
   `lang::types::instance_expands_caller_recursive_typedef`) and fusion
   must expand it env-free. Fusion's pass runs in both modes, so a pass
   the fusion gate owns never changes what the typechecker sees.

## Every written name names something

A definition's check (its gate, and each annotated `let` and each
`never<T>` in its body), and each type definition's body, bounds and
representation once every name of its statement list is registered
(`node::defer_typedef_names`, so a later sibling counts), asks of
every name its written types hold, parameters and bounds included,
whether it names something (`TypeRef::names_something`: the
filled cell, a visible typedef, or a trait), without filling a cell. A
name that does not is deferred to the end of the check
(`ExecCtx::pending_names`, `check_pending_names`), where every name is
registered: a `use` an interface defers (a module that imports from a
later sibling) names it by then. Still nothing there is an undefined
type, refused at its annotation, whether or not any relation ever
expands it.

## Same definition

Name equality does not imply same meaning (REPL redefinition,
cross-env views), so the Ref×Ref name fast paths ask whether two refs
of one written name mean one definition, through one of two gates;
disagreement falls through to the expansion arms.

- `TypeRef::cells_agree`: two filled cells by their definition's
  identity (`ResolvedRef::def_key`), two empty cells alike, a filled and
  an empty one never. It gates the fast paths of `contains` (identical
  params), `could_match` and `sig_matches` (one name over two
  definitions, a reload's, is compared by what each means).
- `TypeRef::same_definition`: filled cells the same way, two empty
  cells by written scope and name. It gates `union` and
  `union_identical` (`setops.rs`: refs to one definition merge or
  compare by their params) and `contains`' arm for two instances of one
  definition whose params contain each other (one type, no expansion).

<!-- CR claude for claude: [doc-drift/low] This paragraph describes ResolvedRef::same_as
and TypeDef::link_same, which no longer exist. Interface typedefs are now spliced into
the implementation, a .gx redefinition is refused (module.rs::check_redefinition), and
def_key is just the params allocation. Replace the link description with that rule.
Found by reading; not run. (x-doc-drift-03) -->
<!-- CR claude for claude: [doc-drift/low] ResolvedRef::same_as and TypeDef::link_same
no longer exist: the commit making a .gx unable to define a type its .gxi defines
removed them, and def_key is now only the params allocation of one TypeDef. Rewrite this
paragraph to say that. Also drop the parenthetical in Type::named_defs' doc comment,
which says an interface's and an implementation's definitions share a def_key. Found by
reading; not run. (t-typ-mod-04) -->
`ResolvedRef::def_key` is the params allocation every cell filled from
one `TypeDef` shares, unless the definition is linked to another
(`ResolvedRef::same_as`, set by `link_same`, which `def_key` follows).
A module's signature check links an interface's typedef to its
implementation's (`node/module.rs`, `TypeDef::link_same`), so a
reference to either is one definition wherever identity decides.

`RefHist::ref_id` fills the cell first and derives a ref's identity
from the resolution's definition (`ResolvedRef::def_key`, the params
allocation every cell filled from one `TypeDef` shares) and its
params, so one ref has one identity in a walk and an old-cell ref never
aliases a redefined def's identity. A constructor application and a
hole key on their content like any other type; only a ref unresolvable
in the ambient env has no identity (`None`), and it expands to nothing,
so no walk recurses under that key. The value walks
(`is_a`, casts, typed printing, union flattening) key the path of names
they expand on the same `def_key`.

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
