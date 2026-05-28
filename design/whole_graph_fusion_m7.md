# M7 — GIR coverage for `array::*`

A focused implementation plan for the array half of M7 from
`whole_graph_fusion.md`. Picks up where the M5/M6 work left off:
builtin effects are classified, user-lambda effect inference runs,
`apply_site_effect` computes call-site joins. The next missing piece
is GIR ops that can absorb `array::map` / `fold` / `init` / `filter`
into a kernel rather than dispatching per-element through the runtime.

## Where we are

- `GirType` enum exists in `graphix-compiler/src/gir.rs`
  (`Prim(PrimType)` | `Array(PrimType)`) with `as_prim`, `as_array_elem`,
  `rust_name`, `from_type`, `From<PrimType>` impls. **No existing field
  uses it yet.** Build is green.
- Existing scalar pipeline (`Input.prim`, `GirExpr.typ`,
  `KnownFusedFn.{arg_types,return_type}`, `GirKernel.return_type`) all
  still typed `PrimType`. Roughly 290 sites across `gir.rs`,
  `fusion.rs`, `gir_interp.rs`, `gir_jit.rs` read or construct values
  with `PrimType`-typed fields.

## Why a single big-bang refactor stalls

Naively flipping `GirExpr.typ: PrimType` → `GirType` cascades to ~290
mismatched-type errors. Each is mechanical (wrap in `GirType::Prim`,
or insert `.as_prim().unwrap()` for scalar-only sites) but the change
crosses too many files at once to verify incrementally — the tree is
broken at every checkpoint until the last fix lands. **Don't do this.**

## Incremental plan that keeps the build green

The trick is to extend the system in ways where old scalar-only call
sites keep compiling without touching them, by introducing array
support alongside (not in place of) the existing PrimType fields.

### Step 1 — Array inputs as a separate slot list

Add a sibling field to `GirKernel`:

```rust
pub array_params: Vec<ArrayInput>,  // alongside `params: Vec<Input>`
```

with

```rust
pub struct ArrayInput {
    pub name: ArcStr,
    pub elem: PrimType,
    pub bind_id: Option<BindId>,
    pub rust_name: String,
}
```

Existing scalar `Input` sites are untouched. Kernels that take an array
input gain entries in `array_params`. The Rust emitter renders them as
`Vec<i64>` parameters; the interp/jit reads them from a parallel
`array_inputs: Vec<ValArray>` table.

### Step 2 — `GirOp::ArrayLen { name }`, `GirOp::ArrayGet { name, idx }`

These produce scalar output (`PrimType::U64` for len, `elem` for get),
so `GirExpr.typ` stays `PrimType`. The new ops reference an array param
by name (not by `Local`, since locals are scalar). Lower `array::len(arr)`
and `arr[i]` Apply expressions in fusion's `emit_expr`.

This validates the array-input plumbing end-to-end without touching
`GirExpr.typ`.

### Step 3 — `array::fold` as a special-cased Apply

`array::fold(arr_param, init: Scalar, |acc, x| body)` lowers to:

```rust
GirOp::ArrayFold {
    array: ArcStr,           // name of an array_param
    init: Box<GirExpr>,      // scalar
    acc_local: ArcStr,
    elem_local: ArcStr,
    body: Box<GirExpr>,      // scalar (typ = init.typ)
}
```

Result type is the scalar `init.typ` — still fits in `GirExpr.typ:
PrimType`. The body inlines the callback's body with `acc` and `x` as
locals. Closes the reduction half of `vectorize_dot`.

### Step 4 — Array-typed expressions

Now we need composition: `array::map(...)` produces an array that
`array::fold` consumes downstream. This is where `GirExpr.typ` must
become `GirType`.

Two options:

1. **Big refactor**: flip `GirExpr.typ` to `GirType` in one shot, fix
   all ~290 cascade sites mechanically. Best done as its own focused
   chunk of work — budget ~half a day with a known recipe (wrap
   constructors, `.as_prim().unwrap()` on scalar reads).

2. **Sibling expression type**: add a parallel `GirArrayExpr` with
   `typ: PrimType` (the element type) and `op: GirArrayOp`. Producer
   ops (`ArrayInit`, `ArrayMap`) build `GirArrayExpr`; consumer ops
   that take arrays and produce scalars (`ArrayFold`) bridge between
   the two trees. Lots of duplication but no scalar-pipeline churn.

Recommend **(1)**. It's the right end state and the cascade is
mechanical. Doing the refactor in a focused dedicated session is
cleaner than maintaining two parallel expression trees indefinitely.

### Step 5 — Producer ops

After step 4, add:

- `ArrayInit { n: Box<GirExpr>, idx_local: ArcStr, body: Box<GirExpr>, elem_typ: PrimType }`
  — `arr.typ = GirType::Array(elem_typ)`
- `ArrayMap { input: Box<GirExpr>, elem_local: ArcStr, body: Box<GirExpr>, result_elem: PrimType }`
- `ArrayFilter { input: Box<GirExpr>, elem_local: ArcStr, predicate: Box<GirExpr> }`
  — predicate is `GirType::Prim(PrimType::Bool)`; result preserves elem type

Lower `array::init`, `array::map`, `array::filter` Apply sites to
these in fusion. Recognise the lambdas via the existing
`apply_site_hint` mechanism so callbacks can be unannotated.

### Step 6 — Backends

- **Rust emitter (AOT)**: render `ArrayInit` as
  `(0..n).map(|idx| body).collect::<Vec<_>>()`, `ArrayMap` as
  `arr.iter().map(|elem| body).collect()`, `ArrayFold` as
  `arr.iter().fold(init, |acc, &elem| body)`. LLVM should vectorize
  these.
- **GIR interp**: evaluate over `ValArray` directly. Per-element
  dispatch through the existing scalar interpreter; no SIMD but no
  runtime-graph dispatch either, which is the whole win.
- **CLIF JIT**: emit a basic-block loop (header / body / latch /
  exit). Cranelift won't auto-vectorize, but the loop body inlines.
  Defer this past M7 if the AOT/interp bench numbers are good enough.

### Step 7 — Bench

Compile `bench/vectorize_dot.gx` through fusion + interp (and AOT)
and confirm:

1. The whole `array::init` × 3 + `array::fold` chain compiles to one
   kernel (or three call-chained kernels — both are fine).
2. Numbers vs the interpreter baseline. Target: at least 5× on the
   inner loop, ideally closer to 20×. SIMD via LLVM (AOT path) should
   be visibly faster than the JIT path, validating the design's
   prediction.

## Files to touch

- `graphix-compiler/src/gir.rs` — new ops, type extension
- `graphix-compiler/src/fusion.rs` — `emit_expr` cases for `array::*`
  Apply sites, `build_kir_kernel` for array-typed lambda args
- `graphix-compiler/src/gir_interp.rs` — evaluate new ops
- `graphix-compiler/src/gir_jit.rs` — fall back to interp for kernels
  containing array ops in v1 (gate via a flag on the kernel)
- `bench/vectorize_dot.gx`, `bench/sum_squares.gx` — verify

## Estimated effort

- Steps 1–3 (array inputs, len/get, fold): ~2 days
- Step 4 (GirType refactor): ~1 day if focused
- Steps 5–6 (producer ops + backends): ~2-3 days
- Step 7 (bench, fixes): ~1 day

Total: 1 week, matching the design doc estimate.

## Tuples and structs (M7.7, M7.8)

Graphix tuples and structs share the array runtime representation
(`Value::Array(ValArray)`), so the unsafe per-slot extraction we
added for arrays (`ValArray::get_unchecked<T>(i)`) drops in directly.
The hard part — boundary plumbing, `from_iter_exact` output path,
GirType cascade — is already done. M7.7 / M7.8 are small extensions.

### Why this is mostly mechanical

- **Same Value layout.** Both lay out as a `ValArray` of N slots;
  each slot is a `Value::<variant>(payload)` with the payload at
  offset 8. `get_unchecked::<T>(i)` works identically.
- **Same boundary path.** Pass-by-`&ValArray` at kernel entry,
  `ValArray::from_iter_exact(...)` for the result. One pooled
  allocation per output, no Vec.
- **Same fusion hook.** `emit_expr`'s current `_ => None` arm is
  where we add `ExprKind::Tuple { args }`, `ExprKind::Struct(s)`,
  `ExprKind::TupleRef { source, field }`, `ExprKind::StructRef
  { source, field }` cases.

### What's new vs arrays

- **Per-slot types instead of one element type.** `Array<f64>` has
  uniform `PrimType::F64`; `(i64, f64, bool)` has three different
  ones. So slot definitions and GirType variants carry
  `Vec<PrimType>` (tuple) or `Vec<(ArcStr, PrimType)>` (struct,
  sorted by field name).
- **Static accessor.** Tuple/struct field positions are known at
  compile time — the accessor op carries `idx: usize` (or
  `sorted_idx: usize` for struct), no expression to evaluate. The
  result `PrimType` is baked into the op (since it can't be derived
  from a single uniform field type).
- **No length op, no init/map producer.** Size is fixed; values are
  explicit per slot. Producer ops are explicit lists rather than
  loops.
- **Struct field-name resolution.** Graphix canonicalises struct
  fields by alphabetical sort, so `{y: 1, x: 2}` lays out as
  `[x_value, y_value]`. The lowering does the name → sorted_index
  lookup at compile time.

### Concrete GIR additions

```rust
GirType::Tuple(Vec<PrimType>),
GirType::Struct(Vec<(ArcStr, PrimType)>),  // sorted by name

// New input slot variants (sibling to ArrayInput):
struct TupleInput {
    name: ArcStr,
    elems: Vec<PrimType>,
    bind_id: Option<BindId>,
    rust_name: String,
}
struct StructInput {
    name: ArcStr,
    fields: Vec<(ArcStr, PrimType)>,  // sorted
    bind_id: Option<BindId>,
    rust_name: String,
}

// New ops:
GirOp::TupleGet  { name: ArcStr, idx: usize, elem_typ: PrimType },
GirOp::StructGet { name: ArcStr, sorted_idx: usize, elem_typ: PrimType },
GirOp::TupleNew  { fields: Vec<GirExpr>, elem_types: Vec<PrimType> },
GirOp::StructNew { fields: Vec<(ArcStr, GirExpr)>, sorted_types: Vec<(ArcStr, PrimType)> },
```

`TupleGet` lowers byte-identically to `ArrayGet`. `TupleNew` lowers
to `ValArray::from_iter_exact([Value::I64(f0), Value::F64(f1), ...].into_iter())`
— same one-alloc shape as `ArrayInit`.

### Where this pays off

- **Mandelbrot complex `(re, im)` tuple.** The current bench
  manually unpacks; with tuple fusion, `let (re, im) = c` flows
  through the kernel and the runtime never boxes the pair per
  cycle.
- **Struct returns from helper functions.** A function returning
  `{distance: f64, angle: f64}` becomes one pooled alloc per call
  with no per-field heap traffic.
- **2D / 3D point types in graphics-y code.** Same shape as
  mandelbrot — small numeric tuples threaded through tight loops.

### Where it doesn't help

- Orchestration code (UI widget configs, callback registration,
  options structs). Fusion targets tight math; these run at the
  graph-construction layer where the per-cycle overhead is already
  amortised across rare events.

### Estimated effort

- M7.7 (Tuple): ~half a day. GirType variant, TupleInput slot,
  TupleGet/TupleNew ops, fusion lowering for `ExprKind::Tuple` and
  `ExprKind::TupleRef`, Rust emitter, two tests.
- M7.8 (Struct): ~half a day. Same structure plus the
  field-name-to-sorted-index helper.

Total ~1 day for both; the GirType refactor cost has already been
paid.

## Schedule

The original plan was: M7.1 GirType → M7.2 Array{Len,Get} →
M7.3 ArrayFold → M7.4 Array{Init,Map} → M7.5 ArrayFilter → M7.6
bench. Tuples/structs slot in *after* the bench validates the array
machinery, not before — the bench is a sanity check on whether the
"unboxed boundary, `from_iter_exact` output, kernel-body-only
producer" pattern actually wins. If it does, M7.7/M7.8 reuse the
validated design with high confidence; if it doesn't, we'd want to
know before building more on top.

Updated sequence:

1. M7.5 — `ArrayFilter` (~half day)
2. M7.6 — bench `vectorize_dot` end-to-end (~1 day)
3. M7.7 — Tuple ops (~half day)
4. M7.8 — Struct ops (~half day)
5. M7.9 — bench mandelbrot's complex tuple inner loop (~half day,
   confirms tuple fusion delivers on its premise)

Total: 2.5–3 days additional after the array bench, against the
original ~1 week M7 estimate.

## Out of scope for M7

- Stateful sync ops (`count`, `sum`, `hold`, `take`, `skip`, `mean`)
  — separate GIR work, parallel to arrays. Mentioned in design doc;
  pick up after the array win is on the bench board.
- `str::*` ops over current strings — needs a `Type::String` extension
  in `GirType`, low priority until UI fusion benches show it matters.
- `select` over arbitrary patterns — already mostly covered, audit
  during step 5 if anything trips.
- Reference ops (`&`, `*`, `*r <- v`) — already partial in fusion
  per the design doc; leave alone.
- Vectorization/SIMD via cranelift — defer past M7.
- **Variants** (`` `Foo(i64) ``, `` `Bar ``). Tagged unions need
  pattern-match-on-discriminant in GIR (we already have `select`;
  the missing piece is reading the variant tag from a `Value`).
  Worth a separate milestone after structs/tuples land — same
  ValArray-boundary pattern but with a tag dispatch on top.
- **Maps**. Heterogeneous-keyed runtime structures don't fit the
  "Vec<PrimType> slot list" model; would need a hash-table GIR
  primitive. No bench motivation yet.
