# Graphix fusion architecture

Captures the design decisions and remaining work for the fusion +
KIR + JIT system on the `graphix-cranelift` branch. Written for
future-me to pick up without rebuilding context. Last updated
2026-04-30.

## What we have today

Three execution paths share the same kernel IR (`KirKernel`):

1. **Node graph** (existing). Default fallback. Every Graphix lambda
   compiles to a tree of `Box<dyn Update>` nodes. Slow but always
   correct, with full reactive semantics.

2. **KIR interpreter** (`kir_interp.rs`). Tree-walking interpreter
   over typed `RegValue`s with a scope-stack env. ~9-50× faster than
   the node graph depending on workload. Wrapped as `KirNode` (an
   `Apply<R, E>`) so the rest of the runtime sees a normal function
   value.

3. **KIR JIT** (`kir_jit.rs`). Cranelift codegen. Each kernel becomes
   a typed CLIF function plus a uniform `extern "C" fn(*const u64,
   *mut u64)` wrapper for type-erased dispatch from `KirNode`.
   Sync (`GRAPHIX_JIT=1`) compiles at Lambda::compile; async
   (`GRAPHIX_JIT_ASYNC=1`) queues to a background worker that
   atomic-fills a `OnceLock` slot.

4. **AOT** (`fusion::rewrite_program` + `kir_to_rust_kernel` +
   `emit_package`). `graphix compile` rewrites the AST so fusable
   lambda bodies become builtin references, emits a Cargo crate
   where each kernel is a Rust source function, rustc/LLVM compiles
   it. Currently the only path that gets LLVM optimizations
   (vectorization, alias analysis).

`fusion::build_kir_kernel` is shared between (2/3) and (4) — both
paths consume the same `KirKernel`.

## The fragility problem (and why it points at lazy fusion)

The current snapshot-based design captures `fusion_known_kernels` at
each `Lambda::compile` time and threads it through the InitFn
closure. Two consequences:

- **Lexical-order dependency**. The mandelbrot outer callback's
  cross-kernel call to `iterate` only works because iterate is bound
  *first* — its eager fusion populates `fusion_known_kernels` before
  the outer callback's snapshot is taken. Reorder the lets and it
  breaks.
- **Annotation dependency**. If iterate isn't fully annotated, its
  eager fusion fails, the snapshot is empty, and the outer kernel's
  `iterate(...)` call can't lower as `KirOp::Call`. Mandelbrot drops
  from 53× back to ~14×.
- **Startup cost**. Eager JIT compiles every fusable kernel at parse
  time. Acceptable for small programs; quadratic-feeling for big ones.

The fix is **fully lazy fusion**, replacing the eager snapshot with
on-demand resolution:

```rust
pub struct LambdaDef<R, E> {
    // ... existing fields ...
    pub body_for_fusion: Option<Expr>,      // None for builtin shims
    pub fusion_cache: Mutex<KernelCache>,    // lazy build slot
    pub fusion_binding_name: Option<ArcStr>, // for self-recursion fn_name
}

pub enum KernelCache {
    NotAttempted,
    InProgress,                               // breaks cycles
    Built { kernel: Arc<KirKernel>, signature: KnownFusedFn },
    Failed,
}
```

- `Bind::compile`: when value is a Lambda, register the LambdaDef
  reference under the BindId in `ctx.fusion_lambdas` (already added
  to ExecCtx, currently unused).
- `Lambda::compile`: drop the eager fusion code path entirely. Just
  capture `body`, `argspec`, `binding_name` into the InitFn closure.
- At first call (InitFn fires): try to fuse. Walk the body; for each
  `Apply { Ref { name } }`, do `env.lookup_bind(scope, name) →
  BindId`, look up `ctx.fusion_lambdas[bind_id]`, lock its cache,
  lazy-build if needed, return signature.
- Recursion: lock state goes through `InProgress` so a callee that
  references its caller mid-build sees the in-progress state, treats
  it as unresolved, and returns None. v1 limit: mutual recursion
  doesn't fuse cross-kernel.

This naturally gives the progression: GXLambda → KirNode (interp) →
KirNode (JIT) per function as it heats up. No snapshot fragility,
no order dependency, no parse-time cost beyond what every kernel
actually needs.

## `KirOp::Call` vs `KirOp::DynCall`

Critical for correctness with rebindable function values. KIR
needs *both* call shapes:

### `KirOp::Call { fn_name: ArcStr, args: Vec<KirExpr> }`

Static, baked-in dispatch to a specific KirKernel. The interpreter
resolves `fn_name` against a `KernelRegistry` (snapshot of stable
kernels visible at fusion time); the JIT (when M4d v2 lands) emits
a direct cranelift call to the callee's compiled function.

**Required precondition: the binding is provably stable.** Stable
means:

- `let X = lambda` form (not `<-` target anywhere in the program)
- Not a function arg (HOF args bind a different value per
  invocation)
- Not an outer-scope capture in a closure that escapes the binding's
  scope

Emitting `Call` when the binding is unstable causes silent wrong
output: a later `<-` to the binding doesn't propagate to the call
site.

### `KirOp::DynCall { fn_value: Box<KirExpr>, args: Vec<KirExpr> }`

Dynamic dispatch. `fn_value` is a KIR expression that evaluates to
a function value (typically `KirOp::Local("callback")` for a HOF
arg, or a Ref-resolution to an unstable binding). The interpreter:

1. Evaluates `fn_value` → `Value::Abstract(LambdaDef)`
2. Calls `lambda_def.init` to get a fresh `Apply<R, E>` (or reuses
   one, depending on how we handle CallSite reuse)
3. Packages KIR args as netidx Values (via RegValue::to_value)
4. Calls `apply.update` with the args
5. Unpacks the result Value back to RegValue

Slower than `Call` (full Apply dispatch overhead) but always correct
because the function value is read at call time. Late-bound by
construction.

The JIT path for DynCall: cranelift can emit a call into a Rust
helper that does the env lookup + Apply dispatch. Slower than
direct CLIF cross-module call but doesn't require static linkage.

### Stability analysis: when to emit which

At fusion time, when lowering an `Apply { function: Ref { name } }`:

```
if name resolves to a function arg (HOF):
    emit DynCall { fn_value: Local(name), args }

else if name resolves to a let-bound binding:
    if binding's BindId is in `unstable_bindings` set:
        emit DynCall { fn_value: Local(name), args }
    else if binding's lambda fuses (lazy_build returns Built):
        emit Call { fn_name: name, args }
    else:
        emit DynCall { fn_value: Local(name), args }
```

The `unstable_bindings` set is populated by a pre-pass over the AST
that finds every `Connect { tgt, value }` (the `<-` operator),
collects each tgt's BindId, and stuffs them in a `HashSet<BindId>`
on ctx (or equivalent). Pre-pass is O(program size); cheap.

For the conservative interim version (no DynCall yet): if the
callee is unstable or a HOF arg, just *fail fusion*. The kernel
falls back to GXLambda. This is what current code does
accidentally-by-design (function args have non-primitive types so
they fail to enter `FusionCtx::inputs`, which propagates up and
kills the kernel). It's correct but loses optimization opportunity.

## Bench numbers (snapshot 2026-04-30, post-M4d v2)

**Caveat**: numbers were collected on a laptop while it was being
plugged/unplugged from AC power, so CPU frequency was scaling around.
*Ratios within the same run* (e.g. JIT vs interpreter on the same
script invocation of `run_compare.sh`) are reliable; absolute times
and cross-run comparisons are noisier than the table suggests.
Re-bench on a stable power state if you want firm numbers.

| benchmark | path | time | speedup vs node graph |
|---|---|---|---|
| mandelbrot (annotated, Graphix iterate + outer callback) | node graph | 9.62s | 1× |
| | KIR interpreter | 0.206s | 47× |
| | KIR JIT sync (M4d v2 cross-module) | 0.157s | 61× |
| | KIR JIT async | 0.191s | (gates leaf only) |
| mandelbrot (unannotated) | node graph | 10.45s | 1× |
| | KIR interpreter | 0.187s | 56× |
| | KIR JIT sync (M4d v2 cross-module) | 0.149s | 70× |
| mandelbrot_fullfused (Rust pixel_auto) | node graph | 0.33s | — |
| | KIR JIT (also fuses fold callback) | 0.14s | — |
| fold_squared (1M unannotated HOF) | node graph | 65.1s | 1× |
| | KIR interpreter | 6.6s | 9.9× |
| | KIR JIT | 6.4s | 10.2× |
| sum_tail (10M tight loop, single call) | KIR interpreter | 0.700s | — |
| | KIR JIT sync | 0.118s | 5.9× over interp |
| | KIR JIT async | 0.660s | (slot doesn't fill before single call returns) |
| sum_of_squares (1M reduction over array) | KIR interpreter | ~10s | — |
| | KIR JIT sync | ~10s | (no win — array machinery dominates) |
| dot product (1M two-array elementwise) | KIR interpreter | ~85s | — |
| | KIR JIT | ~85s | (same — array/fold dispatch is the bottleneck) |

**M4d v2 impact** (mandelbrot): JIT sync was 0.176s pre-M4d v2 (when
outer fell back to interpreter because it had a Call); now ~0.15s
because outer JIT-compiles into the shared module and CLIF-calls
iterate directly. About 12% faster, putting JIT within ~5% of the
AOT-fullfused 0.14s baseline.

**M4d v3 impact** (three-level hot loop, 100k iterations):
- KIR interpreter: 0.866s
- KIR JIT sync (full transitive JIT): 0.835s

JIT sync is ~4% faster than interpreter — pre-M4d v3 the outer
kernel would fall back to interp because middle had a Call (to
leaf), making JIT and interp identical. Now outer→middle→leaf is
a fully native-code chain with two CLIF calls.

Observations:

- **The interpreter is shockingly close to JIT for many workloads**.
  The reason: per-call overhead (CallSite, Apply::update, arg
  marshalling) dominates over the inner loop in any pattern that
  invokes the kernel many times over short bodies. JIT only wins
  decisively in `sum_tail` where one call runs 10M inner iterations.
- **The JIT and AOT are now neck-and-neck on mandelbrot**. KIR JIT
  is 0.16s, AOT-fullfused (Rust `pixel_auto` callback) is 0.14s.
  Within 14%. The remaining gap is interpreter-dispatched
  `KirOp::Call` from outer to iterate; closing it requires either
  cross-module CLIF calls or KIR-level inlining.
- **Vectorization-shaped benches (sum_of_squares, dot product) don't
  show meaningful JIT/interp gap** because the array-construction
  and fold-dispatch overhead is so large it dwarfs the inner-loop
  cost. Concrete numbers (1M elements): sum_of_squares is ~10s in
  both interp and JIT; dot product is ~89s interp / ~87s JIT.
  Neither shows the JIT advantage we expected, because the inner
  kernel is a tiny fraction of total time.

## The array-machinery bottleneck (open optimization)

The vectorize-bench result reveals a strategic finding: on data-
parallel workloads going through `array::init` / `array::fold`, the
per-element Apply dispatch dominates over kernel execution. Two
consequences:

1. **JIT vs interpreter gaps shrink to noise** for these patterns.
   The kernel is fast; getting it faster doesn't help when 90%+ of
   time is in array plumbing.

2. **Mandelbrot's 49× speedup (with KIR JIT and interp basically
   tied) reflects this.** The per-pixel `array::init` dispatch +
   slot Apply construction is the dominant cost; even in fully-
   fused mode the inner iterate loop is a small fraction of total
   time. The only reason mandelbrot wins so hard is that pre-fusion
   the *entire* per-pixel callback was a GXLambda interpretation,
   which is far worse than the array dispatch overhead. Fusion of
   the outer callback removes the GXLambda overhead but doesn't
   speed up the array machinery.

3. **AOT will face the same wall** on these benchmark shapes. Even
   if AOT compiles the kernel to vectorized SIMD with rustc/LLVM,
   the result still goes through the same `array::init` per-element
   dispatch. AOT's vectorization advantage is invisible until we
   bypass the dispatch.

To actually measure vectorization (and to give the JIT a fair fight
against AOT), we need a benchmark shape where the inner loop
dominates total runtime. Three options:

- **`sum_tail`-style benches**: one call with millions of inner
  iterations, no array. This is the only shape where JIT currently
  beats interp clearly (6.5×). Works for testing kernel-level
  optimizations but doesn't represent typical Graphix code.

- **Custom hand-built bench rig**: bypass `array::init` entirely,
  call the fused kernel from a Rust loop over a `&[f64]`. Tests
  what AOT/JIT *could* deliver if dispatch overhead were absent.
  Useful as a ceiling but disconnected from how users write code.

- **A new array primitive that takes a fused kernel and a slice and
  runs the loop directly** (something like `array::fold_native(arr,
  init, fn)` where the fold loop is in Rust and the per-element
  call dispatches into the KIR interpreter or JIT'd wrapper
  directly, no Apply tree per slot). This is the version that
  actually moves the perf needle for users — it lets fusion's gains
  reach data-parallel patterns. Probably the right next-tier
  perf lever after M4f / M4g / M4d v2 are done.

## AOT: keep or drop?

**Open question.** The data above suggests AOT's case is weaker
than expected for the workloads we have:

- **Vectorization isn't visibly winning** in the current bench
  shapes (the array dispatch eats it).
- **Mandelbrot AOT is 0.14s vs JIT 0.16s** — within 14%. The gap
  closes more with M4d v2 (cross-module JIT calls).
- **Code cost**: ~1500 lines for the AOT-specific path
  (`kir_to_rust_kernel` + `rewrite_program` + `emit_package` +
  `render_lib_rs/cargo_toml` + the standalone-build pipeline).

**The case for keeping AOT**:

- Single-binary distribution with no JIT engine
  (`graphix package build-standalone`). Currently produces a binary
  that has zero JIT footprint at runtime. If we drop AOT, every
  standalone binary carries cranelift (~5-10MB).
- Zero-startup distribution targets — embedded, CLI tools, anywhere
  parse + typecheck + fuse cost matters.
- LLVM gets to optimize the kernels with everything it knows about
  the surrounding context (rustc inlining, cross-procedure
  optimization, vectorization). Cranelift will not do these for us.

**Resolution**: keep AOT for now. Re-evaluate after lazy fusion +
DynCall + cross-module JIT (M4d v2) land — at that point the JIT
path may be close enough that the 1500 lines becomes harder to
justify. Specifically, we need:

- A real vectorization-shaped benchmark that exercises the inner
  loop without the array-construction overhead masking it. Probably
  needs to bypass `array::init` / `array::fold` and run the kernel
  in isolation.

## Roadmap (priority order)

### Done
- M1 KIR + AOT refactor
- M2 KIR interpreter + KirNode
- M3 Cranelift JIT lowering
- M4 v1 Lambda::compile wiring (eager + deferred)
- M4b v1 Sync JIT integration
- M4c v1 Deferred fusion (resolved-FnType injection for unannotated)
- M4c v2 Outer-scope const inlining at runtime
- M4d v1 Kernel registry + interpreter `KirOp::Call`
- M4d v2 Cross-module JIT calls (shared JITModule, sync path)
- M4d v3 Transitive fan-out via two-phase declare-then-define
- M4e v1 Async JIT compile via background worker
- M4f Lazy fusion via per-LambdaDef cache
- M4g v1 Conservative connect-target stability gate
- M5 Bench harness

(M4h — eager runtime typecheck — turned out unnecessary; the
apply_site_hint fix below solves the underlying issue without
requiring eager typecheck.)

## Lazy fusion + unannotated callees: solved via call-site FnType

A subtle limit nearly bit us: `ctx.fn_types[lambda_spec_id]` holds
the lambda's own `Arc<FnType>`, whose TVars stay unbound until the
lambda is first called (typecheck runs lazily inside
`CallSite::update`). So when outer's lazy fusion asked for
iterate's kernel, `apply_fntype_to_lambda` was reading iterate's
own typ — full of TVars — and silently no-op'ing the patch.
Iterate's argspec stayed unannotated, kernel build failed, outer
fell back to GXLambda. Concrete cost: mandelbrot dropped from 49×
to ~13× when iterate's annotations were removed.

**The fix exploits 2-pass typechecking.** Each call site (Apply
expr) goes through `CallSite::typecheck`, which constraint-solves
the lambda's signature against the actual argument expressions and
inserts the resolved FnType into `ctx.fn_types[apply_id]` (see
`graphix-compiler/src/node/callsite.rs:405`). This resolved type is
*always concrete* — the args have known types from the surrounding
context. So:

- `discover_callee_names` returns `Vec<(ArcStr, ExprId)>` — each
  callee plus the Apply expression's ExprId.
- `lazy_resolve_kernel` takes an `apply_site_hint: Option<ExprId>`.
- `try_build_lazy` looks up `ctx.fn_types[apply_site_hint]`
  preferentially, falling back to `ctx.fn_types[spec_id]` only when
  no hint is available.

No eager typecheck pass needed. Bench result: unannotated mandelbrot
runs at ~0.18s, identical to the annotated form. **Fully-annotated
and unannotated lambdas now fuse equivalently through arbitrary
nesting.**

The kir_lazy_no_annotations and kir_lazy_three_level tests in
`graphix-tests/src/lang/functions.rs` cover the unannotated /
multi-level cases.

**M4f — Lazy fusion via per-LambdaDef cache.** ✅ Done (including
cleanup of all eager-path scaffolding).

`fusion_lambdas: BTreeMap<ArcStr, Arc<FusionLazyEntry>>` is populated
by `Bind::compile` when the bound value is a Lambda. The
`Lambda::compile` InitFn closure captures the lambda + a const
snapshot + a `Mutex<LazyState>` cache; on first call it patches
the argspec from the call-site `resolved` FnType (a no-op when
the user already annotated), walks the body for callees via
`fusion::discover_callee_names`, lazy-resolves each through
`fusion::lazy_resolve_kernel` (which locks the callee's
`Mutex<FusionLazyCache>` and recursively builds, breaking cycles via
the `InProgress` state), then runs `build_kir_kernel`. The kernel,
JIT wrapper (sync), async slot (when on), and runtime
`KernelRegistry` (built from the lazily-resolved callee kernels)
all land in the lazy state cache; subsequent call sites of the same
LambdaDef just clone Arcs.

The eager scaffolding is gone: `fusion_known_kernels`,
`FusedKernelEntry`, the `runtime_registry`/`known_signatures`/
`snapshot_kernels_full` locals, the `DeferredState` enum, and the
`fusion_known_kernels` save/restore in `Block::compile` are all
removed. Single fusion path, no compile-time snapshot fragility.

mandelbrot's 49× speedup is preserved (lazy resolves iterate at
the outer callback's first call); sum_tail's 6.3× JIT-over-interp
is preserved. All 139 graphix-compiler + 665 graphix-tests pass.

**M4g v1 — Conservative stability gate.** ✅ Done.
`fusion::scan_connect_targets` walks the program AST collecting
names that appear as `<-` LHS. graphix-rt's main `compile` entry
runs it before the inner compile loop, populating
`ctx.unstable_bindings`. `Bind::compile` checks the set before
inserting into `fusion_lambdas` — unstable bindings just don't get
registered as fusable callees, so cross-kernel calls to them can't
lower as `KirOp::Call` and the caller's kernel falls back to
GXLambda. Correct, conservative, ~50 lines. Other gx.rs entries
(`check_inner` and the variants further down) don't yet run the
scan; not bench-blocking.

**M4g v2 — `KirOp::DynCall`.** ✅ Done (HOF args + static-non-
fusable callees, interpreter only).

KIR additions:
- `KirKernel.fn_params: Vec<FnParam>` — function-typed parameters
  alongside the primitive `params`. Each `FnParam` carries a
  [`FnSource`] (`Param { arg_pos }` for HOF args / `Binding {
  bind_id }` for stable-bound non-fusable callees) + the callee's
  prim arg/return types.
- `KirOp::DynCall { fn_index, args, arg_types, return_type }` —
  lowers `Apply{Ref(name)}` against a fn-typed param. fn_index is
  the position in `kernel.fn_params`.

Fusion: `build_kir_kernel` accepts function-typed params (when the
callee signature is all-primitive) and registers them as fn_inputs
in `FusionCtx`. `emit_known_fused_call` checks fn_inputs first; a
hit emits `DynCall` instead of `Call`.

Interpreter: `KirNode<R, E>` is now generic, holding a
`Vec<DynCallSlot<R, E>>` with one slot per fn-param. Each slot has
pre-allocated `BindId`s + `Ref` arg-nodes (one per callee arg) +
a cached `(LambdaDef*, Box<dyn Apply<R, E>>)` invalidated when the
incoming `LambdaDef` pointer changes. Dispatch:
1. Side-channels each arg `Value` into `event.variables[bind_id]`.
2. Calls `apply.update(ctx, &mut arg_refs, event)` (the Refs read
   the freshly-stashed values).
3. Cleans up `event.variables` afterward.
Returns `Option<Value>` — `None` propagates up as a "Pending"
`BodyResult` so the kernel returns `None` this cycle when a
DynCall callee has no value yet.

JIT: `KirOp::DynCall` errors out in the JIT lowering. Kernels
containing it fall back to the interpreter (the fall-back path was
already there for other reasons; just doesn't break).

Limitations of v1:
- Unstable bindings (callee names that appear as a `<-` target)
  are still rejected by `resolve_binding_fn_input` — letting them
  through DynCall would silently dispatch into stale code on the
  cycle the rebinding fires before the kernel sees the update.
  Lifts in v3 with a stability-aware lookup.
- Synchronous semantics: a `None` from any DynCall short-circuits
  the kernel. Multi-cycle reactive callees would need prim-arg
  caching across cycles.
- `Box<dyn Apply>` is constructed via `LambdaDef.init` with
  `resolved: None`. The inner Apply uses its own typ; if it's a
  fusable lambda it'll fuse on its own first call.

Tests:
- `kir_dyncall_hof` (`combine(square, 5) → 26`) — HOF args path.
- `kir_dyncall_static_nonfusable` (`outer(5) → 51` where
  `helper`'s body uses `array::fold`, can't fuse) — Binding-source
  path.

**M4d v2 / v3 — Cross-module JIT calls.** ✅ Done (sync path).

Implemented as a single shared `JITModule` in `kir_jit::SHARED_JIT`
(a static `Mutex<SharedJit>`). Kernels with `KirOp::Call` go
through `kir_jit::compile_kernel_with_callees`, which uses a
two-phase declare-then-define so transitive fan-out and mutual
recursion both work:

**Phase 1 — declare every kernel** in the closure (parent + all
callees) via `cranelift_module::Module::declare_function`,
populating `funcids: BTreeMap<ArcStr, (FuncId, Signature)>`.
Cached entries (keyed by `Arc::as_ptr`) reuse their existing
FuncIds; fresh kernels queue for phase-2 body compilation.

**Phase 2 — define each freshly-declared body**. For each kernel,
walk its body for `KirOp::Call` sites and `declare_func_in_func`
each callee's `FuncId` (already declared in phase 1). The body
emits direct CLIF `call` against the resulting `FuncRef`. Self-
recursion (e.g. naive `fib`'s `fib(n-1) + fib(n-2)`) is just a
Call site whose name matches the parent — funcids has the
parent's own FuncId mapped to its name, so it routes correctly.

**Phase 3 — wrapper + finalize** the module so the new code is
mapped read-execute.

The caller (lambda.rs `InitFn`) is responsible for passing the
*transitive* closure of callees. After lazy-resolving immediate
callees, it walks each resolved kernel for further `KirOp::Call`
sites via `kernel_ir::collect_call_sites` and pulls those in too.

The lambda.rs `InitFn` still routes leaf kernels through
`compile_kernel_with_wrapper` (private module — cheaper, no
mutex contention); kernels with calls go through the shared
path.

`SharedJit::by_kernel` stores `Arc<KirKernel>` alongside the
FuncId+Sig — without this, dropped Arcs could free their memory,
the allocator could land a new (different-content) Arc at the
same address, and a stale FuncId would bind to the wrong code.

Limitations:
- Async path still gates on leaf kernels — the bg worker doesn't
  thread the shared module yet (M4e v2 territory).
- Shared module is process-global and never freed. Acceptable
  for typical programs; revisit if a long-running process churns
  many distinct programs.
- Cranelift doesn't inline across function boundaries, so a
  tight cross-kernel call still pays one `call`/`ret` per
  invocation. Closing the residual gap to AOT (~5-12% on
  mandelbrot) would require KIR-level inlining or building a
  single super-function from the call chain.

Tests: `kir_jit::tests::shared_module_cross_kernel_call`,
`shared_module_self_recursion` (non-tail fib via Call),
`shared_module_callee_dedup` (same Arc shared across parents),
`shared_module_transitive_fan_out` (3-level chain in one
compile session).

**M4e v2 — IR-hash cache.** Hash KirKernel structurally; dedupe JIT
compiles across distinct `Lambda::compile` invocations that produce
identical KIR. Marginal on bench programs but real for codebases
with shared utility lambdas.

### Considered + parked

**Drop AOT** — re-evaluate after M4f + M4g + M4d v2 land. If the JIT
is provably within ~10% of AOT on a real vectorization bench, the
1500-line AOT path is hard to justify. For now, keep it.

## Key invariants to preserve

- `KirNode` is *not* generic over `R/E` (only its `Apply<R, E>` impl
  is). The struct has no `PhantomData<(R, E)>`. Required because
  `Apply<R, E>` requires `Send + Sync` and we don't want to bound
  R/E with those everywhere.
- `KirKernel` is `Send + Sync` (auto-derived). Keep it that way so
  the JIT worker thread can ship them across.
- `WrappedKernel` carries its `JitCtx` so the mmap'd code stays
  alive. Don't separate them.
- `kir_jit::pack_reg_to_u64` / `unpack_u64_to_reg` are the only sane
  way to cross the wrapper ABI. Don't transmute around them.
- The KIR has typed `KirExpr { op: KirOp, typ: PrimType }`. Every
  expression carries its result type; backends rely on this. Don't
  introduce untyped variants.

## Files

- `graphix-compiler/src/kernel_ir.rs` — KIR types + Rust-source
  backend
- `graphix-compiler/src/kir_interp.rs` — Tree-walking interpreter +
  `KirNode<R, E>`
- `graphix-compiler/src/kir_jit.rs` — Cranelift JIT + uniform-ABI
  wrapper + async worker
- `graphix-compiler/src/fusion.rs` — Front end: Graphix Expr → KIR.
  Also AOT package emission.
- `graphix-compiler/src/node/lambda.rs` — Lambda::compile, InitFn
  closure with eager + deferred + JIT wiring
- `graphix-compiler/src/node/bind.rs` — Bind::compile, populates
  `fusion_known_consts` (and will populate `fusion_lambdas` once
  M4f lands)
- `graphix-compiler/src/lib.rs` — `ExecCtx` fields:
  `fusion_known_consts` (compile-time const inlining map, snapshot-
  saved at Block boundaries), `fusion_lambdas` (lazy fusion entry
  map), `current_binding_name` (binding-hint side channel for
  self-recursion). Plus `FusionLazyEntry` and `FusionLazyCache`
  types.
- `bench/run_compare.sh` — 4-way harness
  (graph/interp/sync-JIT/async-JIT)
- `bench/mandelbrot_bench.gx`, `bench/sum_tail.gx`, `bench/fold_squared.gx`,
  `bench/vectorize_*.gx` — corpus
