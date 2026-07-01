# Composite-element + destructure-pattern HOF fusion (plan)

> **STATUS (2026-07-01): IMPLEMENTED for the array HOFs.** All eight array
> HOFs (`map`/`filter`/`flat_map`/`filter_map`/`find`/`find_map`/`fold`/
> `init`) fuse as native loops, including composite (tuple/struct) *elements*
> and `|(k, v)|` **scalar** destructure *leaves*; HOF-of-HOF composes into a
> single multi-loop kernel with no intermediate array. Per-slot HOF
> callbacks fuse into a template kernel once at compile time and then
> `clone_rebind` per array slot.
>
> The lowering mechanism described below in GIR vocabulary
> (`emit_gir`/`GirOp`/`GirType`) is **historical**: the GIR IR was deleted
> (F3, 2026-06-12) and fusion is now distributed as per-node
> `Update::emit_clif` + the `fusion/scaffold.rs` HOF loop scaffolds. Read
> the `emit_gir`/`GirOp`/`GirType` names below as archaeology of the design,
> not as the current mechanism. Current architecture:
> `design/distributed_jit.md`.
>
> **Still-open gaps (#150):** a HOF over `String`- or `Value`-shape
> (variant/nullable) **elements** de-fuses; **composite** (non-scalar)
> destructure **leaves** node-walk; `filter_map` over composite elements.
> Separately, a callback that captures a *local* lambda
> (`array::map(a, |x| g(x))`) de-fuses — a static-resolution limit, not a
> shape one.

The largest remaining tractable fusion cluster. Closes the composite
array HOFs (`array::find`/`find_map` over `Array<(string, i64)>`) and the
**entire map-HOF cluster** (`map::map`/`filter`/`filter_map`/`fold`),
which all share the same blockers. ~12+ fixtures.

This is a **multi-phase feature**, not a single-tick change. Land each
phase green before the next. Interp-first (like the datetime/bytes and
map-HOF-for-arrays work): a phase can land as `FuseExpect::Interp` and a
later phase flips it to `Jit`.

## The blockers (empirically confirmed)

Every map-HOF callback is `|(k, v)| ...` (see
`stdlib/graphix-package-map/src/graphix/mod.gxi`):
```
val map: fn(m: Map<'k,'v>, f: fn(kv: ('k,'v)) -> ('k2,'v2)) -> Map<'k2,'v2>;
val fold: fn(m: Map<'k,'v>, init: 'acc, f: fn(acc:'acc, kv:('k,'v)) -> 'acc) -> 'acc;
```
and the array composite HOFs (`array::find`/`find_map`) likewise take
`|(k, v): T|` over `Array<(string, i64)>`. So four compounding cliffs:

1. **Composite-element input.** The loop element is a `('k,'v)` tuple,
   not a scalar. `MapQ::emit_gir` (graphix-package-core) bails at
   `ai.elem.as_prim()?` (lib.rs ~1005); `GirOp::ArrayMap.in_elem` (and
   `ArrayFilter`/`ArrayFold`/`ArrayInit`/`ArrayFind`/`ArrayFilterMap`/
   `ArrayFlatMap`) are `PrimType`.
2. **Destructure-pattern callback arg.** `|(k, v)|` has no single param
   name, so `MapQ::emit_gir` bails at the `FnArgKind::Positional { name:
   Some(n) }` match (lib.rs ~995-999). The pattern *is* available:
   `GXLambda::args() -> &[StructPatternNode]` (node/lambda.rs:137) gives
   the arg-binding patterns; `g.args()[0]` is the `(k,v)` tuple pattern.
3. **String in composite local.** Map keys are `string`, so the element
   tuple is `(string, i64)` and the destructured `k` is a `string`
   local inside the loop body — the composite-with-string-local cliff
   (also the lone remaining `Interp`: `abstract_type_in_typedef`, and
   `structwith1`).
4. **CMap iteration** (map HOFs only). `Value::Map(CMap<Value,Value,32>)`
   is a tree — no O(1) indexed access. Interp can iterate in Rust; JIT
   needs iteration helpers.

## Phase 1 — widen the HOF-trait element type (`PrimType` → `GirType`) ✅ DONE (2026-06-05)

Behavior-preserving prerequisite, **landed**. Widened `MapFn::emit_gir` /
`FoldFn::emit_gir` trait signatures' `in_elem: PrimType` → `GirType`
(graphix-package-core), and `MapQ`/`FoldQ::emit_gir` callers pass
`ai.elem.clone()` (full GirType) instead of `ai.elem.as_prim()?`. The 6
array impls (`MapImpl`/`FilterImpl`/`FindImpl`/`FilterMapImpl`/
`FlatMapImpl`/`Init`) each do `let in_elem = in_elem.as_prim()?;` at the
top — so composite elements still bail there (unchanged behavior), but the
GirType now *reaches* the impl. Full workspace builds clean; existing prim
HOF fixtures stay green. Flips nothing (the prerequisite).

Remaining for the composite path (Phase 2): widen the loop GirOps'
`in_elem: PrimType` → `GirType` (`ArrayMap`/`ArrayFilter`/`ArrayFold`/
`ArrayInit`/`ArrayFind`/`ArrayFilterMap`/`ArrayFlatMap`) + interp/JIT loop
element-binding for composite elements.

## Phase 2 — destructure-pattern callback binding ✅ DONE (interp + JIT, 2026-06-05)

**Landed + green, end-to-end through native JIT.**
`array::map(a, |(k, v)| k + v)` over `Array<(i64,i64)>` fuses AND JITs
(`array_map_destructure` fixture, **Jit**). All 6 wiring steps below are
implemented. The JIT `ArrayMap` loop branches on `in_elem.as_prim()`:
composite → per-iter `graphix_valarray_get_array` (owned clone) →
`bind_composite` → body → `graphix_valarray_drop` (the getter and the
body's `TupleGet` field reads both clone, so the per-iter drop frees only
the clone — no leak / double-free). The 5 non-`MapImpl` array HOFs still
explicitly bail on a destructure callback (each flips when it gets its own
scalar/composite branch in emit + interp + JIT — the same shape as
`ArrayMap`).

Original mechanism notes (now implemented):

The callback `|(k,v)| body` compiles its arg pattern to
`StructPatternNode::Slice { tuple: true, binds: [Bind(k_id), Bind(v_id)] }`
(node/pattern.rs:19). `GXLambda::args()[0]` exposes it. The leaf `Bind`s
carry **BindIds** (not names); the body's `Ref(k_id)`/`Ref(v_id)` resolve
to them, and the recent **BindId-keyed slot lookup**
(`lookup_local_by_bind_id`) means a kernel slot tagged with that BindId is
found without needing the source name.

Mechanism (no per-iteration kernel inputs — the leaves are *derived* each
iteration, so they're intra-body lets):
1. `MapQ::emit_gir` (core lib.rs ~995): when `g.args()[0]` is a
   `Bind(id)` → today's single-name path. When it's `Slice {tuple, binds}`
   → the destructure path: the element GirType is a `Tuple`/`Struct`; for
   each leaf `Bind(id_i)` at position `i`, the leaf type is the i-th
   element type.
2. The impl emits the body inside a scope where the **composite element**
   is bound to a synthetic `elem_local` and each leaf is a `GirStmt::Let`
   `__leaf_i = TupleGet(elem_local, i, leaf_typ)` registered with
   `bind_id = id_i` (via `register_kir_binding` carrying the BindId). The
   body becomes `Block { lets: [leaf lets...], tail: body_kir }`.
3. `GirOp::ArrayMap { elem_local, in_elem: composite_gtyp, body: block }`.
   The interp/JIT loop binds `elem_local` to the composite element
   (ValArray) per iteration; the body's leaf `TupleGet`s read it.

**Loop element-binding detail (the real interp/JIT surgery).** Today's
`GirOp::ArrayMap` interp arm (gir_interp.rs ~1451) binds the element as a
**scalar** local: `env.push(elem_local, zero_reg(prim))` then per-iter
`env.locals[slot] = RegValue::from_array_elem(&arr, i, prim)` (unsafe
scalar extraction). A composite element is a `Value::Array` — it must bind
into the **composite** env slot (`env.arrays` via `push_array`), set per-
iter to `arr[i].clone()` (the element Value), and the body's `TupleGet`
reads from that array slot. So `GirOp::ArrayMap.in_elem: PrimType →
GirType`, and the loop arm branches: `Prim` → today's scalar fast path;
composite → bind into the array slot. Same split in the JIT arm
(gir_jit.rs ~4305): scalar uses `from_array_elem`-style reads, composite
binds the per-iter element pointer into `env.composites`. This split is
the bulk of Phase 2 — mirror it across `ArrayMap`/`ArrayFilter`/
`ArrayFold`/`ArrayFind`/`ArrayFilterMap`/`ArrayFlatMap`/`ArrayInit`.
`ArrayFold` additionally has the `acc` local (already supports composite
acc via `body.typ`); only its `elem` binding needs the split.

Trait shape: pass the pattern (or a pre-resolved `&[(BindId, usize,
GirType)]` leaf list) alongside `in_elem`, via a shared
`bind_destructure_leaves(ctx, leaves, elem_local, |inner| emit body)`
helper so every impl reuses it.

### Status (2026-06-05): ArrayMap composite *plumbing* landed (green)

Done + verified green (behavior-preserving, composite branch is dead
until the destructure wiring below lands):
- `GirOp::ArrayMap.in_elem`: `PrimType` → `GirType` (gir.rs).
- Interp `ArrayMap` arm (gir_interp.rs): `match in_elem.as_prim()` —
  `Some(prim)` = today's scalar `locals` fast path; `None` = composite,
  binds the element `Value::Array` into the `arrays` slot per-iter
  (snapshot/truncate `env.arrays`), body's `TupleGet` reads it.
- JIT `ArrayMap` arm (gir_jit.rs): `in_elem.as_prim().ok_or(...)?` shadow
  — bails on composite → `fuse()` falls back to interp (interp-first).
- `MapImpl::emit_gir` wraps `GirType::Prim(in_elem)` (still scalar-only).

**Remaining for the first flip (destructure wiring):**
1. `register_kir_binding` needs an `Option<BindId>` variant (today it
   hard-codes `bind_id: None`) so leaf slots carry the pattern BindId.
   The Ref arm (lowering.rs:2632) prefers `lookup_local_by_bind_id(r.id)`,
   so a leaf slot tagged with `k_id`/`v_id` resolves the body's `Ref`s
   without needing source names.
2. `MapFn::emit_gir` / `FoldFn::emit_gir` signature gains
   `elem_binds: &[(BindId, usize)]` (bind_id + tuple position; empty =
   today's single-name scalar path). Mechanical across the 6 array impls
   + FoldImpl (most ignore it).
3. `MapQ::emit_gir` (core lib.rs ~995): when `g.args()[0]` is a tuple
   destructure, resolve each leaf into `(BindId, position)` and pass the
   composite `ai.elem` GirType + the leaf list. (Today it bails on the
   pattern at the `FnArgKind::Positional{name: Some}` match.)
   **Visibility constraint:** `node::pattern` is `pub(crate)`, so core
   can *call* `g.args()` (the pub return type is usable) but **cannot
   match on `StructPatternNode` variants**. Add a pub accessor
   `StructPatternNode::tuple_leaves(&self) -> Option<Vec<(BindId,
   usize)>>` (in graphix-compiler) — `Some` for `Slice{tuple:true,
   binds}` mapping each `Bind(id)`→`(id, i)` and skipping `Ignore`,
   `None` otherwise. Core calls `g.args().first()?.tuple_leaves()` via
   method syntax (no type name needed). `BindId`/`Vec`/`usize` are all
   nameable in core.
4. `MapImpl::emit_gir` (composite path): push a composite element input
   (synthetic `elem_local`, `in_elem` = the Tuple GirType) via a
   `TupleInput`; for each leaf `(id, i)` register a `Let { local:
   synth_i, value: TupleGet(elem_local, i, leaf_typ) }` with `bind_id =
   id`; wrap the body in `Block { lets: [leaf lets], tail: body_kir }`;
   emit `ArrayMap { in_elem: Tuple, elem_local, body: block }`.
5. New fixture `array::map([(1,2),(3,4)], |(a,b)| a+b)` → `[3, 7]` (all-
   prim → flips to **Interp** first, since the JIT ArrayMap arm bails on
   composite; the JIT composite loop is the Phase-2-JIT follow-up).

With Phases 1+2, add an **all-prim** destructure fixture
(`array::map([(1,2),(3,4)], |(a,b)| a+b)` → `[3, 7]`) for the pure `Jit`
path. `array::find`/`find_map` over `Array<(string,i64)>` fuse
**interp-first** (the string leaf routes via
`kernel_contains_composite_element_op` until composite-with-string-local
JIT lands).

## Phase 3 — map MapFn impls + CMap-iteration GirOps (interp)

Add `MapFn::emit_gir` to graphix-package-map's `MapImpl`/`FilterImpl`/
`FilterMapImpl` and `FoldFn::emit_gir` to `FoldImpl`. New GirOps
`MapMap`/`MapFilter`/`MapFold` (or a generic `MapIter` carrying the
collect strategy) whose interp arm iterates the source `CMap`, binds the
`(k,v)` tuple element (Phase 1/2 machinery), runs the body, and collects
into a new `CMap` (map/filter_map) / accumulates (fold). Land
`map_map`/`map_filter`/`map_filter_map`/`map_fold` as **`Interp`**
(`Fuses + NoJit`). `MapQ::emit_gir`'s `resolve_array_input` is
array-specific — the map path needs a parallel `resolve_map_input`
returning a map kernel slot, or generalize.

## Phase 4 — CMap iteration in the JIT

Helpers: `graphix_cmap_len(Value) -> i64` and `graphix_cmap_nth(Value,
i64) -> Value` (the i-th `(k,v)` as a 2-elem `ValArray`), or a
cursor-state helper. The loop codegen mirrors `ArrayMap` but reads
elements via these. Flip the map HOFs `Interp → Jit`. (The
string-in-key still routes those *specific* fixtures to interp until the
composite-with-string-local JIT gap closes — track separately.)

## Cross-cutting: composite-with-string local (the lone `Interp`)

`abstract_type_in_typedef` + `structwith1` fuse on interp but not JIT
because a composite kernel **param/local** with a `string` element isn't
JIT-lowerable (the producer `StructNew`/the element read handle strings;
the gap is the composite slot ABI carrying a string element). Closing
this also lets the map-HOF string-key fixtures reach `Jit` in Phase 4.
Independent of the HOF work; can land anytime.

## Other clusters (status)

- **`/list` recursive variant types**: list HOFs (`list::map`/`filter`/
  `fold`/`find`) now fuse through the per-slot callback path (they never
  batch-loop — a recursive-variant `List` has no O(1) indexed access), so
  this is no longer a blocker for list-HOF *callbacks*. A recursive-variant
  value carried directly as a kernel element remains unrepresentable.
- **lambda-binds-in-bodies** (nested/transitive lambda calls,
  formerly "`node:Lambda`", #203): **RESOLVED** — a `let f = |x| ...`
  called inside a fused callback/callee body now fuses (cross-statement
  static resolution + transitive-callee discovery). The one honest
  remaining shape is a callback that *captures* a **local** lambda
  (`array::map(a, |x| g(x))`), which still de-fuses.
