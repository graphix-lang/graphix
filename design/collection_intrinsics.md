# Compiler-Owned Collection Intrinsics

Status: **implemented and current** (2026-07-13).

This design supersedes the Graphix `sync` subset, generalized value-returning
loops, and the `clone_rebind` callback-template machinery. Collection HOFs are
language operations implemented as ordinary compiler Nodes. They are not
expressible in Graphix source and do not need to pretend to be ordinary Rust
builtins.

## Why They Are Nodes

A reactive collection HOF is not a sequential loop that happens to call a
function. It owns a live subgraph per collection position.

For `array::map(a, f)`, the first value of `a` creates one callback slot per
element. A later array of the same length feeds the existing slots. A shorter
array deletes only the suffix; a longer array preserves the prefix and appends
slots. Callback state and subscriptions therefore survive same-length updates.
Individual callback outputs can continue updating without a new array value.

`fold` has the same identity requirement plus an accumulator chain: slot `i`
feeds its held output into slot `i + 1`. Shrink, growth, taint, replay, sleep,
and deletion all have collection-specific lifecycle rules. A sequential block
cannot infer when those live slots cease to exist.

These are graph semantics, so the node graph is their correct abstraction
level.

## Source Surface

The public APIs remain ordinary Graphix functions in the Array, List, and Map
interfaces. Their implementation lambdas use reserved marker names such as
`array_map`, `list_fold`, and `map_filter_map`.

During lambda construction the compiler maps each reserved name to the
exhaustive `CollectionIntrinsic` enum and builds the corresponding collection
Node. The marker never dispatches through the registered `BuiltIn` table.
Unknown marker names remain ordinary builtin references and must be registered
normally.

The compiler therefore owns:

- callback instantiation and all typecheck phases;
- source, callback, and accumulator binding;
- slot identity, prefix retention, and deletion;
- firing, taint, sleep, replay, and interruption behavior;
- collection-specific result construction.

Array, List, and Map packages own only their non-HOF value operations. The
canonical List value representation and iterator live in
`node::collection::list`, shared by the compiler Nodes and the List package.

## Interpreted Semantics

`MapQ` is the shared map-shaped Node implementation used by init, map, filter,
filter-map, flat-map, find, and find-map operations. It stores one callback
CallSite and last value per ordinal. Collection adapters supply iteration and
result construction for Array, List, Map, and integer index ranges.

`FoldQ` stores one callback CallSite per ordinal plus the accumulator input,
cycle output, and held output. Source changes resize the chain without
recreating its retained prefix. Empty folds return the current initializer.

These names describe internal Node families; they are no longer package
builtins or alternate evaluators hidden behind `Apply`.

## Fusion

Collection Nodes participate in the normal distributed JIT contract:
`Update::emit_clif` either emits into the open CLIF function or refuses and
leaves the Node intact.

Supported Array operations emit native loops directly from the collection
Node. The emitter binds callback parameters by `BindId`, including tuple
destructure leaves, and then emits the callback body through the normal Node
emitter. It supports scalar, String, composite, variant, nullable, and general
Value-shaped elements where the underlying operation has a sound ownership
rule.

If the source, callback, or result shape cannot be emitted, the compile attempt
fails without changing the graph. The same collection Node then executes its
canonical per-slot interpreted semantics. Async callbacks always take this
path, retaining their subscriptions and independent state.

List and Map HOFs lower to the SAME array scaffold loops (2026-07-14)
through a FLATTEN boundary: the collection Value is consumed by
`graphix_list_to_valarray` / `graphix_cmap_to_pairs` (walking the
canonical `list::*` cons functions / `make_pair` — one semantic seam
with the interpreted finishes), the loop runs over the flattened
ValArray (the SlotFlags firing rule over the flattened length IS the
interpreted ordinal-slot rule, since MapQ/FoldQ are
collection-generic), and collection results rebuild at
`graphix_valarray_into_{list,cmap}` (`list::from_iter` /
`split_pair`+`CMap::from_iter`). The enabling ABI rule: recursive
types freeze to an OPAQUE LEAF (`freeze_for_abi_d`'s Seen-hit returns
the matched, cell-filled outer ref with frozen params instead of
refusing), so a List value crosses kernel boundaries as a 2-word
Variant and every list-typed DynCall registers. Known v1 gap: the
fold loop has no Value-shaped accumulator carry, so a List/Map-valued
fold acc interprets (pinned by `list_fold_list_acc_interprets`).

## Rejected Alternatives

- A `sync` language subset made reactive slot lifetime implicit and could not
  express prefix retention or asynchronous callback updates.
- General `loop`/`break` semantics described sequential evaluation, not live
  collection subgraphs.
- `clone_rebind` copied compiled graphs at runtime and had to reconstruct
  binding identity, wake roots, captures, and state ownership. Fresh CallSites
  owned by the collection Node make those relationships explicit.
- Keeping MapQ/FoldQ behind the package `BuiltIn` interface put graph
  construction and typechecking on the wrong side of the compiler boundary.

## Validation

The Array, List, and Map package suites exercise interpreted collection
semantics. Array tests additionally pin direct-loop fusion across primitive,
String, composite, nullable, variant, destructured, captured, nested, and
may-bottom callback shapes. The differential fuzzer remains the authority for
agreement between fused execution and the canonical node walk.
