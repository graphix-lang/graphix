# Compiler-owned collection intrinsics

Status: built 2026-07-13 (List/Map lowering 2026-07-14)
Pins: `stdlib/graphix-tests/src/lang/collection.rs`
(`nested_same_intrinsic`, `nested_map_in_map`, `user_hof_nested`,
`filter_map_total_callback`, the `collection_bodies_*` fixtures), the
Array/List/Map package suites, `findings/fold-midchain-fired-aug2026/`
Supersedes: sync_subset, sync_control, value_returning_loops,
impure_hof_fusion, composite_hof_fusion, clone_rebind_testing

Collection HOFs are language operations implemented as ordinary
compiler Nodes (`node/collection.rs`). They are not expressible in
Graphix source and do not pretend to be ordinary Rust builtins.

## Why they are nodes

A reactive collection HOF is not a sequential loop that happens to call
a function. It owns a live subgraph per collection position.

For `array::map(a, f)`, the first value of `a` creates one callback slot
per element. A later array of the same length feeds the existing slots.
A shorter array deletes only the suffix; a longer array preserves the
prefix and appends slots. Callback state and subscriptions therefore
survive same-length updates, and individual callback outputs keep
updating without a new array value.

`fold` has the same identity requirement plus an accumulator chain:
slot `i` feeds its held output into slot `i + 1`. Shrink, growth,
taint, sleep and deletion all have collection-specific lifecycle rules,
and a sequential block cannot infer when those live slots cease to
exist. These are graph semantics, so the node graph is their correct
abstraction level.

## Source surface

The public APIs are ordinary Graphix functions in the Array, List and
Map interfaces, and the `Collection` trait's blessed impls in core
(`recursive_activations.md` §6). Their implementation lambdas use
reserved marker names (`'array_map`, `'list_fold`, `'map_filter_map`,
...). During lambda construction `CollectionIntrinsic::from_name`
intercepts each marker (before the registered `BuiltIn` table —
`register_builtin` rejects them) and `GXLambda::new_collection` builds
the corresponding Node as the lambda's body. The dispatch charges no
call-depth unit; only the per-element callback dispatch does. An
unknown marker name is an ordinary builtin reference.

The compiler therefore owns callback instantiation and every typecheck
phase; source, callback and accumulator binding; slot identity, prefix
retention and deletion; firing, taint, sleep and interruption; and
result construction. The packages own only their non-HOF value
operations. The canonical List representation (cons = a two-element
`ValArray`, nil = the static empty array) and iterator live in
`node::collection::list`, private to the compiler.

Effect inference needs no HOF special case: the node's prototype
CallSite is a normal call site, so an async callback flips the
collection lambda Async through the ordinary fixpoint.

## Interpreted semantics

`MapQ` is the shared map-shaped Node for init, map, filter, filter_map,
flat_map, find and find_map. It keeps one prototype CallSite (for
typecheck, analysis and emission) plus one live CallSite and last value
per ordinal; collection adapters supply iteration and result
construction for Array, List, Map and integer index ranges. `FoldQ`
keeps one CallSite per ordinal plus the accumulator input, cycle output
and held output; source changes resize the chain without recreating its
retained prefix; an empty fold returns the current initializer.
`find`/`find_map` scan every slot (a bottom predicate after the match
bottoms the find). A callback with labeled parameters interprets; a
callback with only labeled parameters is a type error.

## Fusion

Collection Nodes participate in the normal distributed JIT contract:
`GXLambda::emit_clif` inline-emits a collection-bodied callee as a
native loop at the call site (`emit/scaffold.rs`), or refuses and
leaves the per-slot Node intact — the same Node then runs its canonical
interpreted semantics. Async callbacks always take that path, keeping
their subscriptions and independent state. The emitter binds callback
parameters by `BindId` (tuple-destructure leaves included) and emits
the body through the normal Node emitter; scalar, String, composite,
variant, nullable and Value-shaped elements are supported where the
operation has a sound ownership rule.

Firing is `SlotFlags`: per-slot discs fold into a slots word and a
prev-length word gives exact resize detection
(`kernel_instance_state.md`) — a loop fires iff resized ∨ a slot fired;
a same-length refresh with a quiet body does not fire. In a fold each
body evaluation's STALE folds into the slots word and the accumulator
carry is one more firing source (it alone covers the empty-source
chain); taint rides the carry only, so an acc-ignoring callback
recovers.

**List and Map lower to the SAME array loops** through a FLATTEN
boundary: `graphix_list_to_valarray` / `graphix_cmap_to_pairs` consume
the collection Value, the loop runs over the flattened ValArray (the
`SlotFlags` rule over the flattened length IS the interpreted
ordinal-slot rule, since MapQ/FoldQ are collection-generic), and
`graphix_valarray_into_{list,cmap}` rebuild the result. The enabling
ABI rule: a recursive type freezes to an OPAQUE LEAF (`freeze_for_abi_d`
returns the matched, cell-filled outer ref instead of refusing), so a
List crosses kernel boundaries as a two-word Value. Fold accumulators
may be Value-shaped too (`FoldAcc::Value`): an owned two-word loop slot
whose real disc is carried whole with TAINT|STALE riding the tag bits,
so nullable max-by and map group-by folds fuse.

A TOTAL `filter_map` callback (frozen return provably null-free,
`frozen_may_be_null` conservative) can never produce the `Null` the
intrinsic drops, so `emit_filter_map_kind` routes it to the map loop —
which is what lets the trait's `map` default fuse at parity with the
intrinsic.

## Rejected alternatives

- A `sync` language subset made reactive slot lifetime implicit and
  could not express prefix retention or asynchronous callback updates.
- General `loop`/`break` semantics described sequential evaluation, not
  live collection subgraphs.
- `clone_rebind` copied compiled graphs at runtime and had to
  reconstruct binding identity, wake roots, captures and state
  ownership. Fresh CallSites owned by the collection Node make those
  relationships explicit.
- Keeping MapQ/FoldQ behind the package `BuiltIn` interface put graph
  construction and typechecking on the wrong side of the compiler
  boundary.
- Retiring the intrinsics in favour of Graphix bodies: measured and
  declined per operation (`bench/collection/README.md`,
  `recursive_activations.md` §10) — the scaffold loops are the
  predictable fast path, and only Array `map`/`filter` and Map's impl
  have a Graphix derivation at parity today.
