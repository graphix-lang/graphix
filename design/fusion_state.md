# Fusion-state metric & end-state gap map

Last updated 2026-06-04.

This document tracks how close the test corpus is to the fusion/JIT
end-state, using the bidirectional `run!` harness as the source of
truth.

## The mechanism

Every fixture runs through `run!` (in `graphix-package-core/src/testing.rs`)
which expands to three `#[tokio::test]` modes — `interp`
(`FusionDisabled`, ground truth), `fused` (`JitDisabled`, kernels run
on the interpreter), and `jit` (full fusion + JIT). Each fixture
declares an expected fusion outcome, asserted **bidirectionally** — a
fixture that fuses when it shouldn't, or fails to fuse/JIT when it
should, fails the test:

- `run!(name, code, pred)` — default; asserts **`Jit`** (`JIT_INVOCATIONS > 0`).
- `... ; FuseExpect::Interp` — fuses but runs on the interpreter
  (`FUSION > 0 && JIT == 0`).
- `... ; FuseExpect::None` — no fused kernel (`FUSION == 0`).

Measure the current map any time with:
`GRAPHIX_FUSION_DISCOVERY=1 <test-bin> --nocapture --test-threads=1 jit`
(prints `FUSEMAPF <path> Fuses|None`, `FUSEMAPJ <path> Jit|NoJit`, and
— under a debug-only instrumentation — `FUSEBAIL <path> <reason-tags>`
listing *why* each non-fusing fixture bailed).

### The FUSEBAIL instrumentation

`graphix-compiler` carries a debug-only thread-local fusion-bail log
(`gir_jit_helpers::{record_fuse_bail, take_fuse_bails, reset_fuse_bails}`).
Lowering records a short tag at each give-up site:
- `node:<Variant>` — `emit_node`'s catch-all hit an un-lowerable node
  (`Sample`, `Map`, `ByRef`, `StructWith`, `Connect`, `Lambda`, …).
- `dostmt:<Variant>` — a block statement that isn't a `Bind`/`Nop`/
  `TypeDef`/`Use`.
- `emit:<Helper>` — a producer/accessor sub-emitter bailed
  (`emit:ArrayRef`, `emit:Select`, `emit:Array`, …).
- `call:<name>` / `callqual:<path>` — a call to a function the kernel
  builder doesn't know how to lower (bare or module-qualified).

The `run!` discovery branch harvests these per fixture. This is the
tool that turns "this fixture is None" into "this fixture is None
*because* X", which is what makes the gap map actionable.

## Current metric (graphix-tests, 555 `run!` fixtures)

| state | count | share | meaning |
|---|---:|---:|---|
| **Jit** | 199 | 36% | fuses + JIT-compiles + runs native |
| **Interp** | 1 | 0% | fuses, runs on interp (JIT can't lower yet) |
| **None** | 368 | 66% | no fused kernel |

(562 fixtures total. The 2026-06-05 work: scalar `find`/`filter_map`/
`flat_map` lowerings, composite-element *output* for `init`/`map`,
composite-element + `|(k,v)|` destructure across all 5 array HOFs
(map/fold/filter/flat_map/find_map), composite *output* for `find`
(`Nullable<composite>`), the `/buffer` `EFFECT=Sync` fix, `GirOp::ValueEq`
(non-prim `==`/`!=`), and `GirOp::BytesIndex` (`bytes[i]`). See the log
entries.)

**This replaces the previous (badly inflated) figure of 385 Jit.**
That number counted the hollow `let result = {code}` *wrapper*
identity kernel as fusion; #139's identity-kernel suppression removed
it, dropping the honest count to ~115. The 547-fixture corpus is the
real fusion frontier.

## None breakdown (the work queue)

From a FUSEBAIL harvest, by dominant blocker:

| count | category | tractability |
|---:|---|---|
| ~132 | **qualified user-fn calls, resolved but type-rep-blocked** | the big one |
| 82 | EMPTY (destructure-let / select-internal / **typecheck-error**) | mixed: some correct-None |
| 78 | async-None (sys/json/pack/toml/sqlite/tcp/net/tls/http/fs) | **correct None** |
| 32 | lambda-bind-in-body (`node:Lambda`) | needs registry coordination |
| ~8 | map HOFs over `CMap` (`callqual:/map/{map,filter,filter_map,fold,change}`) | composite-element + destructure-pattern + CMap iteration — see `composite_hof_fusion.md` (multi-phase) |
| 13 | reactive `never()` | **correct None** |
| ~20 | structural: `emit:Select`, `emit:Array`, array slice, ByRef/Deref, try/catch, Sample, StructWith, checked-arith, Connect | per-construct lowering |
| ~12 | misc builtin calls (`filter`, `sum`, `divide`, `all`, …) | several are loose-`Number` **correct-None** |

### The qualified-user-fn-call cluster (~132 — the biggest opportunity)

Calls to **graphix-implemented** functions (`list::*`, `array::*`
graphix HOFs, `buffer::*`, interface-module functions) used to bail
silently. Two foundational fixes (2026-06-04) made them *resolve*:

1. **Interface-proxy resolution.** A signed module re-exports each impl
   binding's value to its public `val` signature binding under a
   *different* BindId; the caller references the signature BindId.
   `static_resolve::collect_lambda_binds` now follows the Module's
   `proxy()` map (impl_id → sig_id) so a caller's interface `Ref`
   resolves to the impl `LambdaDef`. Without this, `resolved_apply()`
   was `None` for every cross-module-through-interface call.
2. **Qualified source-name.** `emit_node`'s CallSite Lambda arm now
   derives the kernel registry / `GirOp::Call` key from the full path
   when `ident_of` rejects a module-qualified name.

After resolution the kernel build still bails on **type
representability** — the remaining, per-cluster blocker:
- `/list` (44): **recursive variant types** (`List<'a> = [\`Cons('a,
  List<'a>), \`Nil]`) — `GirType::from_type` can't represent an
  infinite type.
- `/array` scalar-callback HOFs (`map`/`filter`/`fold`/`init`):
  **CLOSED (2026-06-05, task #144).** `array::map(a, |x| x>3)` and
  friends now fuse + JIT end-to-end. The fix had two independent parts;
  the "runtime hang" earlier blamed on static-dispatch was a *wrong
  reconstruction* (the static-dispatch path runs MapQ fine — verified by
  removing the would-be fix and seeing the suite stay green):
  1. *Plumbing* — three gaps stopped the EXISTING `analysis_pred` /
     `emit_gir` machinery from ever firing: (a) the stdlib is compiled
     separately so `array::map`'s Bind isn't in `bind_to_lambda` → a
     `ctx.cached` fallback in `try_resolve_callsite` resolves it; (b) the
     callback formal is a **TVar bound to Fn**, not bare `Type::Fn`, so
     `invoke_apply_fn_arg_hook` needs `farg.typ.with_deref(...)`; (c)
     `BuiltInLambda` didn't delegate `static_resolve_fn_args` to its
     inner `MapQ`.
  2. *The actual hang* — `MapQ`/`FoldQ`/`Init::refs` reported their
     `analysis_pred`'s **synthetic internal bindings** (the per-element
     `x`/`acc`/`i` and the callback-function handle) as free variables.
     Fusion's `collect_region_inputs` (= `refs.refed − refs.bound`) then
     registered them as region inputs and built a feeder
     `reference(synthetic_id)` that nothing ever produces → the kernel
     waited forever. Fix: `Refs::mark_bound` + masking those synthetic
     ids in each builtin's `refs()`. A `refs()` that surfaces a binding
     it owns as a free var is just wrong, fusion or not.
  Still None in `/array`: the non-`emit_gir` HOFs (`find`/`find_map`/
  `filter_map`/`flat_map`/`group` — no codegen override yet) and
  nested/composite-element maps (`array::map(a, |x| array::map(...))`).
- `/buffer` (34): **bytes + mutable-ref** buffer ops (ByRef/Deref).
- `/inner`, `/mod_a`, `/outer` abstract (opaque) types: **mostly CLOSED
  (2026-06-05).** `check_sig` persists abstract→concrete in
  `gir::ABSTRACT_REGISTRY`; `resolve_abstract` (fusion-internal) consults
  it before `from_type`. 11 → Jit, 1 → Interp. Still None: parameterized
  abstracts (`Box<'a>`, unsubstituted body), `recursive` (`List`),
  `in_variant` (abstract variant payload), `struct_impl` (string return).

So the ~132 split further by *type-system feature needed*, not by a
single fix.

## Distance to end-state

Of the ~426 None, a large fraction are **correct-None**: 78 async/IO,
13 `never()`, a chunk of the 82 EMPTY (typecheck-error / error-
expecting / `Err`-asserting fixtures), and several loose-`Number`
builtins. The genuinely-should-fuse remainder is dominated by the
type-system features above (recursive variants, composite-element
HOFs, abstract-type rep, `GirType::Map`) plus the structural cliffs
(lambda-binds-in-bodies, Sample/Connect/ByRef reactivity, array
slices, StructWith).

## How to close a gap

1. Pick a blocker from the FUSEBAIL harvest.
2. Implement the lowering / type-rep in `fusion/lowering.rs` +
   `gir`/`gir_interp`/`gir_jit`.
3. The bidirectional check then **fails** the now-fusing fixtures
   ("this now JITs — upgrade to `Jit`"); remove their `; FuseExpect::None`
   and the `// ASPIRE:` comment.
4. Re-harvest; the Jit % moves monotonically up, with no silent
   regressions.

## Session log

- **2026-06-04**: interface-proxy resolution + qualified source-name +
  TypeDef/Use-skip-in-blocks. 115 → 119 Jit (`rectypes0`, `rectypes1`,
  `typedef_tvar_ok`, `interface_no_abstract_types`). Established the
  FUSEBAIL instrumentation and this accurate map.
- **2026-06-04**: composite / string / value-shape element reads in the
  JIT (`a[i]`/`t.0`/`s.field` of non-prim element type). 119 → 120 Jit
  (`structaccessor`).
- **2026-06-04**: `array[i]` bounds-check seam — one shared
  `node::array::array_index` used by node-walk / gir-interp / JIT;
  `ArrayGet` is now `Nullable<elem>`-typed and bounds-checked (was
  unchecked + bare-scalar-typed — a real correctness divergence that no
  fixture covered). `array_indexing0` Interp → Jit + 6 new bounds-check
  fixtures. Jit 120 → 127, Interp → 0, corpus → 553.
- **2026-06-04**: `GirType::Map` as a Value-shape leaf (the `bytes`
  pattern) + constant map-literal fold + map builtins via DynCall. Jit
  127 → 136 (`map0/1/2/map_empty`, `map_len`, `map_get*`). Deferred:
  `m{key}` access, map mutations/HOFs, non-constant literals.
- **2026-06-05**: scalar-callback `/array` HOFs fuse+JIT (task #144).
  Plumbing (cached-fallback resolution + `with_deref` fn-arg detection +
  `BuiltInLambda` `static_resolve_fn_args` delegation) let the existing
  `analysis_pred`/`emit_gir` machinery fire; the real hang was
  `MapQ`/`FoldQ`/`Init::refs` leaking `analysis_pred`'s synthetic
  bindings as free vars → orphaned kernel feeder (fixed via
  `Refs::mark_bound` + masking). 136 → 143 Jit (`array_map0`,
  `array_filter`, `array_fold0`, `array_init0/1/2`,
  `gir_fused_deferred_map`). Method note: the "static-dispatch hang"
  hypothesis was disproven by *removing* the speculative reset and
  watching the suite stay green — empirical over reconstruction.
- **2026-06-05**: `array::find` + `array::filter_map` scalar lowerings,
  interp **and** JIT — `GirOp::ArrayFind` (predicate bool →
  `Nullable<elem>`, early-exit) and `GirOp::ArrayFilterMap` (body
  `Nullable<out>` → collect non-null `Array<out>`), each a per-`MapFn`
  `emit_gir` override mirroring `ArrayFilter`/`ArrayMap`. JIT: the
  `ArrayFilterMap` loop checks each body result's discriminant against
  `null` and pushes the cast payload (in `compile_scalar_impl`, Array
  result); the `ArrayFind` loop early-exits to a two-word `(disc,
  payload)` merge (in `compile_value_expr`, Nullable result — same phi
  shape `compile_ifchain` uses). 143 → 146 Jit (`array_find_scalar`,
  `array_find_scalar_none`, `array_filter_map_scalar`). The pre-existing
  `array_find`/`array_find_map` stay None (composite-tuple elements +
  destructure-pattern callbacks — the composite-element-HOF cliff);
  `array_filter_map` stays None (its `false => x ~ null` arm uses the
  Sample operator `~`, an `emit_expr` gap). Follow-ups: `flat_map`
  (variable-length per-element output); composite-element HOFs (widen
  `in_elem`/`out_elem` `PrimType`→`GirType`) to flip the existing
  composite fixtures; `Sample` in `emit_expr`.
- **2026-06-05**: `array::flat_map` scalar lowering, interp + JIT —
  `GirOp::ArrayFlatMap` (body `Array<out>` → concatenate). The JIT is
  linear (no nested cranelift loop): each iteration compiles the body to
  an owned `*ValArray` and calls the new
  `graphix_value_buf_extend_from_array` helper (clones each element into
  the output buf, drops the array — the inner loop lives in Rust). 146 →
  147 Jit (`array_flat_map`, an existing None flipped). Completes the
  scalar non-`emit_gir` HOF sweep (find/filter_map/flat_map). Still None:
  `find_map`/`group` and all composite-element HOF fixtures.
- **2026-06-05**: composite-element *output* for `array::init` + `map`.
  Removed the redundant `elem_typ`/`out_elem` `PrimType` fields from
  `GirOp::ArrayInit`/`ArrayMap` (they always equalled `body.typ`); the
  interp pushes via `into_value` (any element shape) and the JIT via the
  existing `compile_and_push_field` (per-shape push dispatch — prim →
  `push_<T>`, composite → `push_array`, value-shape → `push_value`,
  string → `push_arcstr`). 147 → 149 Jit (`array_init3` tuple-output
  flipped; new `array_map_tuple` covers `map`'s composite output without
  nesting). **Input** stays scalar (`in_elem: PrimType`). Remaining
  `/array`: composite-element *input* (`array_find`/`find_map` read
  `Array<(string,i64)>` + destructure-pattern callbacks) and nested-HOF-
  with-capture (`array_map1`'s inner `array::map(b, |y| x+y)` captures the
  outer `x` — the inner kernel doesn't thread the capture / inner array
  input yet).
- **2026-06-05**: `/buffer` `EFFECT=Sync` fix (+7 Jit). The pure buffer
  builtins (`buffer::to_string`/`from_string`/`len`/`concat`/`to_array`/
  `from_array`/`to_string_lossy`) are `EvalCached` impls that never set
  `EFFECT`, so they inherited the conservative `Async` default →
  fusion-discovery's `Sync`-only filter skipped them (FUSEBAIL
  `callqual:/buffer/...`). Same class as the `str::escape` fix. Added
  `const EFFECT = EffectKind::Sync` to all 9 `buffer.rs` `EvalCached`
  impls. That exposed a latent JIT bug: the value-shape-return `DynCall`
  arm (`gir_jit.rs`, ret_kind=2) `debug_assert`'d `Variant|Nullable`
  only and *panicked* on a `bytes` return — widened to
  `return_type.is_value_shape()` (a general fix for any bytes/datetime/
  duration/map-returning DynCall, latent until buffer exercised it).
  Flipped 7 simple buffer fixtures (149 → 156). Still None: byte
  indexing/slicing and the `encode`/`decode`/`varint`/`zigzag` family
  (`Array<Encode/Decode>` spec args).
- **2026-06-05**: `GirOp::ValueEq` — Value-shape `==`/`!=` (+2 Jit).
  `gir::cmp` bailed on any non-primitive operand (`as_prim().is_none()`),
  so `Map == Map` (and any variant/nullable/bytes/datetime equality)
  couldn't lower. New op mirrors `ValueArith`: owned `(disc, payload)`
  operands (`compile_owned_value_operand`) + a consuming `graphix_value_eq`
  helper (netidx `Value` PartialEq) → `Bool`; interp compares
  `into_value()`s. Ordering operators (`<`/`>`) on non-prim stay
  unlowered. Flipped `map_insert`/`map_remove` (156 → 158; both end in
  `m == {literal}`). **Then extended to String + composite** (Array/
  Tuple/Struct): the interp `ValueEq` was already general (`into_value()`
  compares any shape); the JIT's `compile_owned_value_operand` gained
  String (`graphix_value_new_string`) and composite
  (`graphix_value_new_from_array`, refcount-cloning a Borrowed local)
  arms that wrap the operand into a `Value` before `graphix_value_eq`.
  No *existing* fixture was blocked solely on string/composite `==`, so
  added 3 to exercise+cover the new paths (`value_eq_string`,
  `value_eq_string_ne`, `value_eq_tuple`; 158 → 161, 560 fixtures).
  Remaining map: `change`/HOFs (`callqual` — graphix `map::change` +
  MapQ-based map/filter/fold), `m{key}` (`MapRef`), `iter`/`iterq`.
- **2026-06-05**: `GirOp::BytesIndex` — `bytes[i]` (+2 Jit). Extracted a
  shared `node::array::bytes_index` (bounds-checked, negative-from-end,
  → `Value::U8`/error) and refactored the node-walk `ArrayRef` to use it
  (node-walk / interp / JIT agree). `emit_array_ref` falls through to a
  `BytesIndex` op when the source is value-shape `bytes`; result
  `Nullable<u8>`. JIT: `graphix_bytes_index(Value, i64) -> Value` over an
  owned bytes operand (`compile_owned_value_operand`). Flipped
  `bytes_index`/`bytes_neg_index` (161 → 163). Still None in `/buffer`:
  `bytes_slice*` (`b[a..c]?` — slicing + the `?` error operator) and
  `encode`/`decode`/`varint`/`zigzag` (`Array<Encode/Decode>` specs).
- **2026-06-05**: abstract-type resolution in fusion (+11 Jit, +1 Interp).
  A cross-module call to a graphix fn whose arg/return is an *abstract*
  type (`type T;` in a `.gxi`, concrete `type T = i64` private in the
  `.gx`) couldn't fuse: the arg type reaching `ensure_lambda_kernel` is a
  `Type::Ref("T")` that `GirType::from_type` can't lower (no env, can't
  resolve the name). Fix: `node::module::check_sig` persists each
  abstract→concrete mapping into a global `gir::ABSTRACT_REGISTRY`
  (`IntMap<AbstractId, Type>`) keyed by the globally-unique `AbstractId`;
  a new fusion-internal `resolve_abstract(typ, env, depth)` resolves
  `Type::Ref` via `lookup_ref(env)` and `Type::Abstract` via the
  registry, recursing through Tuple/Array/Set/Struct composites (depth
  cap → recursive types like `List` terminate, return as-is → don't
  fuse). Applied at the 3 `from_type` sites in `ensure_lambda_kernel`
  (arg / capture / return). The abstraction stays **opaque to the type
  system** — only the optimizer peeks at the concrete rep to size kernel
  slots. Flipped 11 `/inner` fixtures None → Jit (`abstract_type_basic`,
  `byref`, `multiple`, `different_modules`, `in_array`, `in_tuple`,
  `map_key`/`map_value`/`map_key_and_value`, `nested_module`,
  `two_modules_combined`) and `abstract_type_in_typedef` None → Interp
  (its `Pair = {first: First, second: string}` struct param has a string
  field — composite-with-string kernel param isn't JIT-lowerable yet, so
  it fuses on interp). Still None: parameterized abstracts (`Box<'a>` —
  registry holds the unsubstituted body; `params.is_empty()` guard skips
  them), `recursive` (`List` recursive type), `in_variant` (variant
  payload of abstract), `struct_impl` (cross-module string return).
  163 → 174 Jit, 0 → 1 Interp.
- **2026-06-05**: `GirOp::MapRef` — `m{key}` map access (+4 Jit). Mirrors
  `array_index`/`bytes_index`: a shared `node::map::map_get(src, key)`
  (the value or the `map key not found` error) drives the node-walk
  `MapRef`, the fusion interp, and the JIT (`graphix_map_ref(Value,
  Value) -> Value`) so all three agree. `emit_map_ref` lowers a `MapRef`
  node whose source is `GirType::Map` to a value-shape op typed
  `Nullable<V>` (the node's `[V, Error]` via `from_type`); both operands
  compile through `compile_owned_value_operand` (same 4-word-in /
  2-word-out helper sig as `value_add`). Flipped `map_ref0`/`map_ref1`/
  `map_ref2`/`map_ref_missing` (174 → 178). Still None: `map_nested`/
  `map_complex_keys`/`map_with_arrays` (their composite map *literals*
  don't constant-fold — `emit_map_new` bails on non-constant/composite
  entries, a separate cliff) and `map_ref_wrong_type` (typecheck error,
  correct-None). Remaining map ops: `map::change`/HOFs (`callqual`),
  `map::insert`/`remove`, `iter`/`iterq`.
- **2026-06-05**: composite-literal const-fold (+1 Jit). `node_const_value`
  (used by `emit_map_new`) only folded `Constant`/`ExplicitParens`, so a map
  literal with a nested composite value (`{"outer" => {"inner" => 42}}`)
  bailed. Now it recurses into `Array`/`Tuple` (→ flat `ValArray`) and `Map`
  (→ `Value::Map`) nodes when every element folds; extracted shared
  `const_valarray` / `const_map` helpers (the latter now also backs
  `emit_map_new`). Flipped `map_nested` (178 → 179). `map_with_arrays` stays
  None for a *different* reason (heterogeneous union value type
  `[Array<i64>, Array<string>]` → `from_type` on the MapRef result is None);
  `map_complex_keys` stays None (struct keys are `let`-bound Refs, not inline
  constants — needs let-const-propagation).
- **2026-06-05**: `GirOp::ArraySlice` — `a[i..j]`/`a[i..]`/`a[..j]`/`a[..]`
  (+6 Jit). Mirrors `array_index`/`bytes_index`/`map_ref`: shared
  `node::array::array_slice` (usize bounds) + `array_slice_i64` (fused i64
  bounds → usize, negative → "expected a non negative number") drive node-walk
  / interp / JIT. `emit_array_slice` lowers an `ArraySlice` node (source
  `GirType::Array`/`Bytes`, integer-scalar bounds) to a value-shape op typed
  `Nullable<source>`; JIT `graphix_array_slice(src: Value, start, end, flags)`
  with bit-flags for present bounds. Flipped `array_indexing1`/`2`/`3`/`4`
  (all four bound combinations) + 2 new error-path fixtures (`array_slice_oob`,
  `array_slice_oob_is_err` — OOB slice → error through all 3 backends). 179 →
  185. The helper handles `Bytes` too, so it also flipped the 3 `bytes_slice*`
  fixtures (`buffer::to_string(b[1..4]?)`) None → **Interp**: the whole
  ArraySlice + `?` + `buffer::to_string` chain now fuses on the interpreter,
  but JIT-compiling a `QopUnwrap` whose success type is value-shape (bytes)
  isn't wired yet (`compile_value_expr`'s QopUnwrap arm), so jit mode falls
  back to interp. 1 → 4 Interp.
- **2026-06-05**: value-shape `QopUnwrap` JIT codegen (+3 Jit, −3 Interp).
  Added the `compile_value_expr` `QopUnwrap` arm the previous entry flagged:
  for a value-shape success type (Variant/Nullable/DateTime/Duration/Bytes/
  Map) the inner `Nullable<T>` compiles to an owned `(disc, payload)`; on
  `Error` disc it drops the error Value, signals pending, jumps to
  `pending_exit`; otherwise the non-error Value *is* the result (passed
  through, consumer takes ownership). Flipped the 3 `bytes_slice*` Interp →
  Jit (the full `b[1..4]?` → `buffer::to_string` chain now JITs). 185 → 188
  Jit, 4 → 1 Interp.
- **2026-06-05**: `{s with f: x}` (StructWith) via expand-to-StructNew (+2
  Jit). No new GirOp: `emit_struct_with` knows the struct's sorted field set
  at compile time, so it builds a `StructNew` whose fields are either the
  per-field replacement or a `StructGet` copying the unchanged field from
  the source (a `Ref` to a struct kernel input). Reuses already-lowered
  ops. Flipped `structwith2` (`{selected with y}` shorthand), `structwith3`
  (`{selected with y: selected.y + 1}`). `structwith0` stays None (typecheck
  error — replacing a string field with an int); `structwith1` stays None
  (its struct has a `string` field copied via StructGet — composite-with-
  string-in-block cliff, same as `structaccessor`); `structwith4`/`5` bail
  on `node:Lambda`. 188 → 190.
- **2026-06-05**: composite-element + destructure-pattern HOF foundation,
  Phases 1–2 (interp) — see `composite_hof_fusion.md`. `array::map` with a
  `|(k, v)|` tuple-destructure callback over `Array<(i64,i64)>` now fuses
  (interp). Phase 1: `MapFn`/`FoldFn::emit_gir` `in_elem` `PrimType` →
  `GirType`. Phase 2: `StructPatternNode::tuple_leaves` accessor +
  `register_kir_binding_bid` (BindId-tagged slots) + `input_snapshot`/
  `emit_hof_body_destructured` (composite element + per-leaf `TupleGet`
  lets in a `Block`); `MapQ`/`FoldQ::emit_gir` extract the leaves;
  `MapImpl` lowers the destructure; `GirOp::ArrayMap.in_elem` widened, the
  interp loop binds the composite element into the `arrays` slot (the JIT
  bails on composite → interp). New `array_map_destructure` fixture
  (`[(1,2),(3,4)]`, `|(k,v)| k+v` → `[3,7]`) lands **Jit** (Phase 2-JIT
  below). The 5 non-`MapImpl` array HOFs explicitly bail on destructure
  (their flips come when each gets a composite path).
- **2026-06-05**: composite-element JIT loop (Phase 2-JIT) — flips
  `array_map_destructure` Interp → **Jit** (190 → 191, 2 → 1 Interp). The
  JIT `ArrayMap` arm now branches on `in_elem.as_prim()`: `Some` = scalar
  fast path; `None` = composite — per-iter `graphix_valarray_get_array`
  (owned `*mut ValArray` clone) → `bind_composite(elem_local)` → body
  (`TupleGet`s clone the fields) → `graphix_valarray_drop` of the owned
  element. No leak/double-free (the getter clones, the body clones the
  fields it reads, the drop frees the per-iter clone). Remaining for the
  cluster: the other 5 array HOFs (Filter/Find/FilterMap/FlatMap/Fold need
  the same scalar/composite branch in their emit + interp + JIT loops),
  composite-with-string elements (string leaf routes to interp), and
  Phase 3–4 = map `MapFn` impls + CMap iteration.
- **2026-06-05**: composite-element + destructure extended to `array::fold`
  (+1 Jit, 191 → 192). `ArrayFold.elem_typ` `PrimType` → `GirType`; the
  interp + JIT loops gained the same scalar/composite element-binding split
  as `ArrayMap` (acc stays scalar). `FoldImpl::emit_gir` lowers
  `|acc, (k,v)|` via the shared `emit_hof_body_destructured` (nested in a
  `with_input` that registers the scalar acc). New `array_fold_destructure`
  (`array::fold(a, 0, |acc, (k,v)| acc+k+v)` → `10`) lands **Jit**. The
  pattern is now proven on 2 HOFs; `Filter`/`Find`/`FilterMap`/`FlatMap`
  replicate the identical split.
- **2026-06-05**: composite-element + destructure extended to
  `array::filter` (interp; +1 Interp, 1 → 2). `ArrayFilter.elem`
  `PrimType` → `GirType`; the interp loop branches scalar/composite and
  pushes the *original* composite element on a `keep`. `FilterImpl::emit_gir`
  lowers `|(k,v)|` via the shared helper. New `array_filter_destructure`
  (`array::filter(a, |(k,v)| v>3)` over `[(1,2),(3,4),(5,6)]`) lands
  **Interp** — the JIT composite-filter path needs a conditional drop on
  the not-kept owned element (distinct from map/fold's unconditional
  consume), a documented follow-up; the JIT arm bails on composite (→
  interp). The destructure foundation is now exercised across 3 array HOFs
  (map+fold Jit, filter Interp).
- **2026-06-05**: JIT composite-filter conditional drop — flips
  `array_filter_destructure` Interp → **Jit** (192 → 193, 2 → 1 Interp).
  The JIT `ArrayFilter` arm now branches scalar/composite: composite gets
  each element via `graphix_valarray_get_array` (owned), binds it, evals
  the predicate, then on `keep` moves it into the output
  (`graphix_value_buf_push_array`) / on not-keep drops it
  (`graphix_valarray_drop`) via a dedicated `drop_block` — so the owned
  per-iter element is consumed exactly once (no leak / double-free). All 3
  array HOFs (map/fold/filter) now JIT composite elements + destructure.
- **2026-06-05**: Sample operator `~` lowering (+1 Jit, 193 → 194). `a ~ b`
  in a fused kernel lowers to `b` — guarded by a soundness check: a fused
  kernel recomputes its whole output on *any* input change, while `~`
  emits `b` only when the trigger `a` fires, so `a ~ b ≡ b` is sound iff
  every external ref of `b` is also an external ref of `a` (a recompute
  from a value-dep change then implies a trigger-dep change). The trigger's
  refs still surface as region inputs via `Sample::refs`. Flips
  `array_filter_map` (`false => x ~ null` arm — `null` has no external refs
  ⊆ the trigger's, so it lowers) None → **Jit**. Non-qualifying Samples
  (value depends on inputs the trigger doesn't) bail — correctly unfused.
- **2026-06-05**: `GirOp::ArrayFindMap` — `array::find_map` (+1 Interp, 1 →
  2). Closes the existing `array_find_map` cliff (`|(k, v): (string,i64)|`
  destructure callback whose `false => v ~ null` arm needed Sample). New op
  = `ArrayFilterMap`'s Nullable body + `ArrayFind`'s early-exit (returns the
  first non-null body result as `Nullable<out>`); `in_elem` is composite,
  bound via the proven scalar/`arrays`-slot split. `FindMapImpl::emit_gir`
  (was absent → DynCall) builds it with the destructure path. Interp-first:
  the value-shape result + composite-with-string element aren't JIT-lowered
  yet (the JIT arm errors → `fuse()` runs it on interp), so it lands
  **Interp**. Pulls together destructure ✓ + Sample ✓ + composite-input ✓ +
  the new early-exit op. The JIT (early-exit Nullable merge like `ArrayFind`
  + composite element) is the follow-up to Jit it.
- **2026-06-05**: JIT `ArrayFindMap` (+1 Jit, new `array_find_map_prim`).
  The `compile_value_expr` arm mirrors `ArrayFind`'s early-exit value-shape
  merge, but compiles the body to its `Nullable<out>` `(disc,payload)`
  directly and drops the owned per-iter composite element every pass (the
  result is the body value, not the element). New `array_find_map_prim`
  (`array::find_map([(1,10),(2,20),…], |(k,v)| select k==2 {true=>v,
  false=>null})` → `20`) fuses + **Jit**, validating the codegen on an
  all-prim composite element. `array_find_map` (its sibling) **stays
  Interp**: its element is `(string, i64)`, and a composite element
  containing a `string` field still routes the kernel off the JIT — a
  separate **composite-with-string-element** cliff, not an ArrayFindMap
  issue (the all-prim case JITs cleanly through the same arm).
- **2026-06-05**: Block-let-String JIT fix (+1 Jit, flips `array_find_map`
  Interp → **Jit**; 195 → 196). Found via a `JITDBG` probe on the
  swallowed `compile_kernel_with_callees` error: the `GirOp::Block` let arm
  in `gir_jit.rs` *errored* on a `GirType::String` value ("String locals
  aren't supported"), even though the May-2026 String-locals work made them
  first-class — that work updated `GirStmt::Let`/`bind_string` but **missed
  the `GirOp::Block` let arm**, so *any* kernel with a string `let` inside a
  block (e.g. a `|(k, v)|` destructure over `Array<(string, _)>`, whose leaf
  `k` binds as a string local in the destructure `Block`) was silently kept
  off the JIT. Fix mirrors `GirStmt::Let`'s arm (`compile_scalar` →
  `bind_string`; block scope-exit already drops it). A real
  correctness/completeness fix beyond the metric — string locals now JIT in
  *all* contexts, not just top-level lets. (`array_find_map` was the only
  fixture exercising it; the suite found no others.)
- **2026-06-05**: `array::flat_map` composite element + destructure (+1 Jit;
  196 → 197, new `array_flat_map_destructure`). `GirOp::ArrayFlatMap.in_elem`
  widened `PrimType` → `GirType`; interp + JIT loops got the identical
  scalar/composite element split as the other 4 array HOFs. `flat_map`'s
  body output stays an owned `Array<out>` concatenated by the existing linear
  `graphix_value_buf_extend_from_array`, so the composite *element* is the
  only new owned per-iter value to drop (after the body, before `extend`) —
  no early-exit / conditional-drop wrinkle. **This completes the
  composite-element + destructure foundation across all 5
  linear/conditional array HOFs (map / fold / filter / flat_map / find_map).**
  Remaining `/array` cliffs: composite-with-**string** elements (string leaf
  forces interp — the `array_find`/`array_find_map` `(string,_)` fixtures);
  composite *output* for `find` (`Nullable<composite>`); the map-HOF cluster
  (CMap iteration GirOps + `MapFn` impls).
- **2026-06-05**: `array::find` composite element + composite **output** (+2
  Jit; 197 → 199, `array_find` None → Jit + new `array_find_composite`).
  `array::find` returns the matched element, so a composite element makes the
  result `Nullable<composite>` — the composite-*output* case (distinct from
  the element-transform HOFs). `GirOp::ArrayFind.elem` widened `PrimType` →
  `GirType`; `FindImpl::emit_gir` got the destructure + composite branch. The
  JIT arm is a **conditional-consume** of the owned per-iter element feeding a
  value-shape early-exit merge: found edge wraps it via
  `graphix_value_new_from_array` (consume) → `(ARRAY_DISC, payload)`; advance
  edge drops it via `graphix_valarray_drop` (like `ArrayFilter`'s conditional
  consume, but into a Nullable merge). **All 5 linear/conditional array HOFs
  now JIT composite elements + destructure, and `find` JITs composite output —
  the array-HOF composite story is complete.**
