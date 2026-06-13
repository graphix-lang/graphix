# Distributed JIT: `Update::emit_clif` + `jit()` — deleting the GIR IR

Status: **executing (staged).** Supersedes `delete_gir_ir.md` (which planned
the same GIR removal around a *central* `compile_node` walker; its scoping
analysis, risk list, and oracle caveats remain valid and are folded in here).

## The architecture

Fusion/JIT becomes two trait methods on `Update` (and one on `Apply`),
completing the pattern `update`/`delete`/`sleep`/`refs` already follow —
each node owns its case; there is no central walker and no builtin
side-trait:

```rust
trait Update<R, E> {
    /// Emit this node's computation into the open kernel; return its
    /// SSA result. Default: Err — this node doesn't fuse (correct for
    /// every async node, free of churn). CLIF is NOT store-and-combine
    /// data (cranelift has no inliner; SSA values/blocks live in one
    /// open FunctionBuilder), so the combinable unit is emit-into-the-
    /// open-function, recursion is `child.emit_clif(cx)`.
    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr>;

    /// Fuse this subtree. `Some(replacement)` = "I fused myself:
    /// delete me and swap this in" — the parent (or the compile-time
    /// driver, for roots) calls `child.delete(ctx)` then
    /// `*child = replacement`. `None` = no replacement at this level;
    /// the impl already recursed `jit` into its own children via
    /// &mut self and swapped any that returned Some.
    fn jit(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>>;
}

trait Apply<R, E> {
    /// The builtin emission hook (replaces GirEmitter::emit_gir).
    /// Ok(None) = shape not handled → DynCall fallback; the impl MUST
    /// NOT have emitted instructions before returning None. Err =
    /// abort the kernel build → the region node-walks (partial
    /// emission fine — the function is discarded).
    fn emit_clif(&self, cs: &CallSite<R, E>, cx: &mut BodyCx)
        -> Result<Option<CompiledExpr>> { Ok(None) }
}

/// Mechanics only, NO policy — the steps identical for every fusable
/// case: sync/effect check, region inputs → KernelSig, callee prepass,
/// compile under the jit lock (emit_clif recursion from `node`),
/// FusedKernel + feeders. Ok(None) = not sync / didn't compile.
pub fn try_fuse<R, E>(node: &dyn Update<R, E>, ctx: &mut ExecCtx<R, E>)
    -> Result<Option<Node<R, E>>>;
```

Policy lives in each node's `jit` — that's the point. A sync-capable node
tries `try_fuse(self, ctx)` then recurses children; an async node just
recurses. Case-specific fusion logic goes in the node implementing the
case: MapQ's per-slot template fuse is `template.jit(ctx)` inside MapQ;
callee-kernel handling lives with CallSite/the lambda Apply. Maximality
falls out of top-down order; a failed compile falls through to child
recursion — better granularity than the central region planner (a
non-emittable root no longer loses the whole region).

`emit_clif` deliberately takes no `&mut ExecCtx`: emission runs inside the
jit lock (`ctx.jit.lock()` held by try_fuse's compile phase). Everything
needing ExecCtx (callee kernel cache, capture lookup) happens in the
analysis phase, before the builder opens.

**`BodyCx`** is one honest context struct bundling borrows of the existing
emission triple — `pub b: &mut FunctionBuilder` (raw CLIF escape hatch) +
private `env: &mut JitEnv` + `ctx: &LowerCtx` — with a small public method
set: `helper(name)`, `mark()/truncate()`, `bind_scalar/_tainted/
bind_composite/bind_string/bind_value`, `read_elem`, `require_valid`
(Scalar2 → bottom-abort), `interned_str` (lazy stable-address interning —
replaces the GIR-body prewalk that built KernelStrings/KernelValues; a
Node prewalk mirroring emission coverage would be silent-drift dangling-
pointer UB). graphix-compiler re-exports `cranelift_codegen`/
`cranelift_frontend` so packages stay in version lockstep without a direct
dep.

**Scaffold library** (`jit/scaffold.rs`): the 8 HOF loop scaffolds
(map/filter/filter_map/flat_map/find/find_map/fold/init), extracted FROM
the existing GirOp arms (the arms become thin closures over them — a
production refactor validated before any emit_clif HOF lands). Body-result
semantics preserved verbatim; the two that matter:
- a may-bottom **map** body → `emit_bottom_abort` (kernel-wide pending) at
  runtime;
- a may-bottom **fold/filter** body → `.single()` Err → **de-fuses at
  build time**, never a runtime abort.

**`KernelSig`** (`kernel_abi.rs`): the kernel ABI contract — fn_name, the
8 kind-grouped param vecs + fn_params, tail_call_slots, return_type,
has_tail_loop, with `abi_params()`/`abi_param_wire_slots()`/`abi_return()`.
Built once per kernel, shared by `Arc`: the runtime dispatch node
(`GirNode`, eventually `FusedNode`) and the JIT cache key off the same
allocation — the `Arc<KernelSig>` IS the compiled-callable handle. During
the parallel period `GirKernel = { sig: Arc<KernelSig>, body }` with a
transitional `Deref<Target = KernelSig>` so read sites don't churn.
`PrimType`/`AbiKind`/`abi_kind`/`freeze_concrete`/the Input family/
`AbiParamKind`/`AbiReturn` all live there too — the durable, body-free
half of the boundary.

## Build strategy

The new system grows under `CFlag::DirectNodeJit`; the GIR path stays the
default and keeps the suite green until the flip; then delete everything
the new system didn't use. Correctness is structural throughout: the
node-walk is canonical and untouched; the differential fuzzer
(interp-vs-new, `Mode::DirectJit`) gates every stage; a JIT bug can lose
fusion, never produce a wrong answer. Old-vs-new region-selection
differences can't cause value divergences — only coverage differences,
tracked by FuseExpect fixtures (re-annotated deliberately, once, at the
flip).

## Stages

- **A — kernel_abi carve** (landed): `KernelSig`; `GirKernel` wraps it;
  ABI consumers re-key (via the transitional Deref). Rider: deleted the
  vestigial async-JIT machinery (`AsyncJitSlot`, the worker thread,
  `GirNode::with_async_jit` — no callers; a GirNode now REQUIRES a
  wrapped JIT, field non-optional) and the dead `KernelRegistry` (its
  reader was the deleted GIR interpreter; callee dispatch is direct CLIF
  calls via `compile_kernel_with_callees`).
- **B — the skeleton, end-to-end on scalars** (landed): trait surface
  (`Update::emit_clif`/`jit`, `Apply::emit_clif`, BodyCx, re-exports);
  Stage-1 `compile_node` arms became node-impl shims (Constant/Ref/
  arith/cmp/bool/Not/TypeCast/ExplicitParens/Block in node/{mod,bind,
  op}.rs, op macros emit via `BinOp::$name` etc., checked arith keeps
  the default-Err); `fusion::try_fuse` (identity gate → return-type
  gate → `collect_region_inputs` → `sig_from_inputs` builds KernelSig
  with NO GIR body → `compile_kernel_with_callees_direct` on an
  empty-body GirKernel → FusedKernel) + `fusion::jit_node` child-visit
  protocol; `jit` recursion impls on Module/Block/Bind (Bind fuses its
  VALUE — the ValueBind shape — and whole-Bind fusion is structurally
  impossible since Bind has no emit_clif). The driver replaces the
  walker plan entirely under the flag. `by_kernel` is evicted on
  failed compiles (risk 1 closed — failure is the common no-fuse
  signal now, and lambda-kernel Arcs get re-submitted in Stage E).
  Observed: whole-do-blocks fuse as single kernels with in-kernel lets
  (the maximality improvement over the walker, which descended).
  Gates: DirectJit probes + nonscalar-fallback + 120-program
  generated sweep, full suites, classic-path fuzz — all green.
- **C — shape coverage** (in progress): LANDED — C2 BindId-keyed scalar
  env slots (`AbiParamDesc.bind_id` from the input structs → entry
  binder → `lookup_bind_id`; `Ref` emission resolves id-first; block
  lets carry `Bind::single_bind_id()`; the try_fuse dup-basename guard
  narrowed to non-scalar inputs); C1 string + value-shape constants and
  Refs with LAZY interning (`BodyCx::interned_str/interned_value` →
  per-entry-boxed arenas on LowerCtx, merged into KernelStrings/
  KernelValues post-emit — no Node prewalk, coverage exact by
  construction); per-shape block lets + scope-exit drops + block-tail
  ensure-owned (`node_composite_source`, the Node twin of
  `classify_composite_source`: Ref→Borrowed, parens/block-tails
  transparent, else Owned; `ensure_owned_{composite,value}_src`);
  full-shape `emit_kernel_return` (Value/composite/string returns with
  the pending check + ensure-owned, Scalar2 bottom-output routing);
  Discard statements (owned results dropped — better than the GIR arm,
  which leaks them; can't diverge values). Also fixed: `Module::jit`
  recursed `source` (the dynamic-module source-string node) instead of
  `nodes` — module-wrapped programs never fused until this. Sweep:
  1106 regions fuse / 120 generated programs; remaining blockers are
  float-modulo (pre-existing parity) + per-node shape gaps below.
  C3 LANDED — composite producers (Tuple/Array via the shared TupleNew
  mirror, Struct with sorted interned field names, Variant nullary +
  payload forms) and accessors (TupleRef/StructRef via
  compile_element_read; ArrayRef array+bytes paths; MapRef/ArraySlice
  via owned-Value operands → Value results), plus ValueArith
  (datetime/duration → graphix_value_<op>) and ValueEq (non-scalar
  ==/!= via graphix_value_eq; ordering on non-scalars stays Err,
  mirroring gir::cmp — this also killed a latent prim_of panic on
  string equality). One deliberate improvement over the GIR arms:
  owned producer SOURCES of accessor reads are dropped after the read
  (`emit_accessor_source_drop`) — the GIR arms never met owned sources
  (env-named = borrowed); leak-prevention only, can't diverge values.
  NOTE for the Stage-F soak: the new-path ownership logic
  (ensure/drop discipline) has value-equality coverage but no leak
  oracle in DirectJit mode — worth a leak-checked soak run.
  Sweep: 1231 regions / 120 programs (+125 over C1/C2).
  C4 LANDED — `?`/`$` unwrap (`emit_qop_node`, the Node twin of
  wrap_qop fused with both GIR QopUnwrap arms: non-Nullable inner
  passes through; scalar success is branchless Scalar2; string/
  composite success keeps the branch-abort with owned-error drop +
  clone-if-borrowed; Value-shape success ends in
  ensure_owned_value_src) and builtin DynCall (`emit_dyncall_node`,
  the Node twin of marshal_dyncall_args + both return-decode arms,
  ret_kind 0–4, buf popped before the pending branch;
  `CallSite::emit_clif` dispatches resolved-Lambda → Err (Stage E),
  `Apply::emit_clif` hook, then the DynCall fallback with the
  source-order labeled/positional arg assembly mirrored from
  emit_known_fused_call). Discovery reuses the classic Node-based
  prepass verbatim: try_fuse runs `walk_node_for_builtin_calls`,
  installs `discovery.fn_params` on the sig (which activates the
  shared `GirNode::pre_init_builtin_slots` runtime machinery
  unchanged) and threads `apply_sites` to emission via a defaulted
  `BodyEmitter::builtin_apply_sites` → `LowerCtx` →
  `BodyCx::builtin_site`. Verified firing (not just value-agreeing)
  via one-time instrumentation: 3 DirectJit kernels with a builtin
  slot, 5 qop unwraps (4 scalar + 1 Value-shape Duration). The
  generated sweep can't produce `?`/`$`/builtin calls, so permanent
  probes were added (`direct_node_jit_qop_dyncall_probes` in
  graphix-fuzz): scalar/OOB/map-ref/duration `$`, str::len scalar
  DynCall (bare + in arithmetic), str::to_upper String-return
  DynCall — all three modes agree. Sweep stays 1231 (expected —
  generator gap, not coverage loss). C4 verification also surfaced a
  PRE-EXISTING classic-path crash (#199, FIXED): composite-success `?`/`$`
  (`a[1..]$`) SIGSEGVs under fusion+JIT at committed HEAD — the
  direct path mirrored the crashing arm faithfully. Root cause: the
  QopUnwrap composite-success arm handed a Value::Array's INLINE
  ValArray payload bits to consumers expecting the boxed
  `*mut ValArray` composite ABI, so the scope-exit drop did
  `Box::from_raw` on the Arc's data pointer. Fixed in the GIR arm and
  the direct mirror together via new `graphix_value_into_array`
  (owned inner, consumes) / `graphix_value_into_array_borrowed`
  (borrowed inner, clones); the string-success path was never
  affected (ArcStr bits are the same one-word representation inside
  and outside a Value). Regression artifacts:
  findings/composite-qop-jun2026 (2 programs, in the regress gate),
  2 run! fixtures (FuseExpect::Jit), 3 DirectJit probes, 3 mutation
  seeds. Lesson for the remaining C/D arms: any place a payload word
  crosses from the Value world to the composite ABI needs an explicit
  re-box — review new `payload` passthroughs for this confusion.
  C5 LANDED — `select` via `emit_select_node` (the Node twin of
  emit_select_as_expr + compile_ifchain fused into one pass; canonical
  reference is Select::update / PatternNode::is_match). Scrutinee
  emitted ONCE, SSA-reused by every arm cond — stabilization for free
  (no __sel_scrut temp; `fused_select_scrutinee_evaluated_once`
  gates it). Pattern coverage at GIR parity (Ignore/Bind/Literal/
  Variant-tag+scalar-payloads; null + single-prim type predicates;
  slice/tuple/struct patterns Err). Arm binds use the pattern's real
  BindId in the JitEnv (no known_consts inlining, no shadow guard —
  kills that bug class); variant payload reads and guards evaluate
  INSIDE the matched region (node-walk order; the GIR inlines payload
  reads into the cond, which would be UB-adjacent under a wrong tag).
  Scalar merges always thread the validity phi (expr_may_value_bottom
  has no Node twin) but report Scalar2 only when actually tainted.
  Deliberate non-mirrors, both safe-direction: explicit `disc != NULL`
  test where the GIR uses an order-UNSOUND trivially-true shortcut
  (#200, live wrong-value divergence on the classic path: `select v {
  i64 as _ => 1, null as _ => 0 }` with a fused Nullable scrutinee
  takes the first arm on null); refuse-to-fuse where the GIR's
  compile_ifchain last-arm trap is reachable (#201, SIGILL: a
  possibly-bottom scrutinee's garbage cond bits miss every arm).
  Both since FIXED in the classic path (Eric's call: interim
  production exposure + fuzz-gate hygiene): #200 emits an explicit
  `!IsNull(scrut)` cond in emit_type_predicate_cond; #201 refuses in
  compile_ifchain when the scrutinee is possibly-bottom (Scalar2)
  and the final arm is conditional — the refusal, not a pending-exit
  gate, because the validity-AND is the precise semantics for a
  NESTED select (pending-exit would wrongly bottom a kernel whose
  output doesn't consume the select; the stmt form gates because a
  body-position select IS the kernel output). Repros in
  findings/select-jun2026 (regress gate). `freeze_normalized`
  (kernel_abi.rs) added because typecheck leaves select result types
  as un-flattened arm unions (`Set([i64, TVar→i64])`) that
  freeze_concrete rejects — try_fuse's gate + the arith/cmp/let/
  DynCall-arg relays now use it; without it no select-rooted region
  was ever attempted. 18 select probes (direct_node_jit_select_probes)
  verified firing in DirectJit, all three modes agree; the generated
  sweep covers selects natively.
  C6 LANDED — StringInterpolate via `emit_string_interpolate_node`
  (the Node twin of compile_concat, sharing the extracted
  `string_buf_push_helper` prim→push map; same part restriction —
  non-scalar/non-string parts Err, and a possibly-bottom Scalar2
  part de-fuses via `.single()`, both GIR parity) and checked arith
  (`+?`/`-?`/`*?`/`/?`/`%?`) via `emit_checked_arith_node` — NEW
  coverage, the GIR path never lowered these. Semantics by
  construction: the node-walk's error-wrapping core was extracted to
  `node::op::wrap_arith_error` and the `graphix_value_checked_*`
  helpers call it on `Value::checked_*` — overflow / div0 is the
  catchable `ArithError` error VALUE (flows through `is_err`/`$`),
  never bottom; `[T, Error]` freezes to the Nullable wire shape so
  qop/select/return consume it unchanged. `arith_emit_clif!` gained
  the base-BinOp param so the checked nodes route to the new relay.
  15 probes (`direct_node_jit_string_checked_probes`), verified
  firing via one-time instrumentation: 5 interpolations + 10 checked
  emissions. Blocker profile (before == after, on the direct-test
  workload): the planned "Block generalization" had no real
  occurrences — every Block shape in the sweep already emits;
  remaining non-noise blockers are float modulo (27, pre-existing
  parity), possibly-bottom non-scalar select scrutinee (8,
  deliberate refusal), undefined value-shape local (3 — a region
  input whose type doesn't freeze is skipped by
  collect_region_inputs, then its Ref fails emission; correct
  degradation), Map literal (2, no direct relay yet), `let x = null`
  (2, Null-shape lets unsupported on both paths). The 1779/158
  Bind/Module "failures" are the jit_node attempt-then-recurse
  protocol, not coverage gaps.
- **D — HOFs**: D0 fusion-stats counter FIRST (Eric-approved,
  2026-06-10, **landed**): compile-time `FusionStats { attempted, fused,
  failed: Vec<(ExprId, reason)> }` on ExecCtx (per-ctx, parallel-safe —
  the existing `FUSION_INVOCATIONS` counters are runtime-side and
  per-thread), populated by `try_fuse` (direct) and the classic splice
  (fused count only, for the Stage-F old-vs-new audit), exposed via
  the `ToGX` exec pattern (like `env_stats`), and asserted by the
  `direct_node_jit_*` probes (`fused > 0` in DirectJit) — this
  retires the per-stage temporary-eprintln firing-check ritual (C4,
  C5, and C6 each rebuilt it by hand; the C5 freeze gap — no select
  region EVER attempted, invisible to value gates — is the bug class
  it permanently catches) and feeds the Stage-F coverage audit + the
  blocker-profile measurements. Landing notes: graphix-fuzz's
  `run_program_with_stats` subtracts the post-init baseline (the stdlib
  root fuses 9 regions per init under DirectJit — the old ~1231 sweep
  instrumentation figure was 120×9 stdlib + 142 per-program); the
  `fused > 0` assertions immediately exposed two scalar probes that had
  NEVER compiled (`cast<f64>(7) + 1.0` — typecheck error, cast returns
  `[f64, Error]`; a one-expression inner block — parse error) and
  passed for their whole life via CompileErr == CompileErr agreement;
  the repaired cast probe is a real current blocker (cast CallSite
  doesn't emit CLIF → deliberate fallback); the Stage-1 "nonscalar
  falls back" probes now wholly fuse (Stage C composites) and were
  re-pointed to assert fusion. D1 scaffold extraction (**landed**,
  2026-06-11 — the first step since Stage A that restructured the LIVE
  classic path, run as its own gated step): the eight HOF loop bodies
  moved out of the GirOp arms into `gir_jit::scaffold`
  (`gir_jit_scaffold.rs`, a `#[path]` child module so it keeps private
  access to JitEnv/LowerCtx) as `emit_{init,map,filter,filter_map,
  flat_map,fold,find,find_map}_loop` taking body closures over
  `BodyCx`, plus `push_field` (the push half of
  `compile_and_push_field`, shared with the producer arms) and the
  internal `bind_elem`/`drop_composite_elem`/buf-lifecycle helpers;
  the arms are now thin closures (gir_jit.rs net −785 lines).
  `bind_elem` dispatches on `abi_kind` and Errs on string/value-shape
  elements — the #150 gap inputs that the old `scalar_prim`-None ⇒
  composite dispatch would have type-confused (unreachable from
  lowering; the explicit refusal is a clean de-fuse if that ever
  changes). Gate methodology worth keeping: a **CLIF differential** —
  the new permanent `GRAPHIX_DUMP_CLIF` env hook dumps every finalized
  kernel; 44 programs (28 HOF probes in /home/eric/tmp/d1_probes + the
  findings corpus) captured before/after and diffed normalized
  (ExprId/`__hof_N` labels + pointer iconsts are the only run
  variance) came back instruction-for-instruction identical, a far
  sharper gate than value agreement for a pure refactor. Value gates
  also green: 1423 fixtures (all FuseExpect::Jit intact), fuzz suites
  6/6, regress 16/16, 150 generated + 400 mutated programs, 0
  divergences. A 27-agent adversarial review (4 angles × verify)
  confirmed the refactor and surfaced only D2-facing contract gaps +
  one stale comment, all addressed as doc-contract text on the
  scaffolds. **D2 preconditions recorded:** (1) `ArraySrc { owned:
  true }` (fresh-producer input arrays) drops only on the NORMAL
  post-loop path — a mid-loop pending abort (`pending_exit`) or build
  Err won't release it, because a raw SSA value is invisible to
  `emit_pending_cleanup`. The contract is one-or-the-other: register
  the ptr for pending cleanup (a valarray analogue of
  `register_hof_buf`) + owned:true, OR env-bind it as an owned
  composite + owned:false — env-bind PLUS owned:true double-drops on
  the normal path (doc'd on `ArraySrc`). (2) `emit_filter_map_loop` is
  PrimType-in/out by construction (the GIR op's own restriction) — if
  D2's filter_map needs wider shapes it extends the scaffold, not
  bypasses it. (3) The scaffold surface (and the BodyCx pieces its
  closures need) is `pub(crate)` — but `MapFn`/`FoldFn::emit_clif`
  impls live in package CRATES (graphix-package-core/-array), so D2
  re-exports the scaffolds `pub` alongside the plan's §1 BodyCx
  public-surface work. (4) All scaffold bindings are name-only
  (`env.bind`/`bind_composite`); the direct path resolves BindId-first
  (C2, the #162/#167 shadowing class) — `HofElem` grows an
  `Option<BindId>` when D2 wires the first Node-body closure. (5) The
  bare-`ClifValue` closures (fold init/body, filter/find pred,
  flat_map body) carry the may-bottom BUILD-time de-fuse contract —
  closure must Err on Scalar2, never strip validity (doc'd on
  `emit_fold_loop`); `push_field` (map/init) is the one RUNTIME
  bottom-abort seam. **D2-map landed** (2026-06-11):
  `MapFn::emit_clif` (defaulted trait method, no ExecCtx — emission
  runs under the jit lock), `Apply::emit_clif` on MapQ (the emit_gir
  orchestration twin: analysis_pred → inner CallSite → Lambda body;
  elem name from the callback FnType, elem BindId from the arg
  pattern's `StructPatternNode::single_bind_id()`),
  `MapImpl::emit_clif` via `scaffold::emit_map_loop` (V1 gates, all
  pre-emission: destructured → D3, elem shape ∈ bind_elem's set,
  `node_composite_source(array) == Borrowed` — owned producers await
  the pending-cleanup stage —, body type freezes + not Unit/Null).
  Supporting: scaffold surface + `node_composite_source` went `pub`;
  `HofElem` gained `id: Option<BindId>`; JitEnv composites gained a
  BindId column + BindId-first composite Ref resolution (classic
  slots are id-less → resolution unchanged; CLIF differential
  re-verified identical). Result: whole-block maximal fusion
  (`{ let a = [..]; map(..) }` = ONE kernel, literal + loop), and the
  direct path EXCEEDS classic on two probe shapes classic never
  inlined (composite-tuple elements with accessor bodies; qop `$`
  bodies) — the recurse-granularity gain the plan predicted. **Two
  hard lessons, both now structural:** (1) risk 6 materialized at its
  worst — `BuiltInLambda` (the builtin plumbing wrapper) delegated
  ten Apply methods but not `emit_clif`, so the trait default's
  Ok(None) silently swallowed EVERY builtin's hook; all 8 map probes
  "passed" while no map ever inlined. Fixed with the delegation (+ a
  loud comment); any new Apply method MUST be added to
  BuiltInLambda's delegation set. (2) `fused > 0` is necessary, not
  sufficient — the array-literal region satisfied it vacuously.
  graphix-fuzz probes now use a `Fuse::{No, Some, Clean}` ladder:
  `Clean` (fused>0 AND no non-ancestor-noise blocker in
  `stats.failed`) catches the silent-loss class — used by the 7
  wholly-fusing map probes; `Some` remains for probes with a
  legitimately-refusing auxiliary region (e.g. a bare-Null let).
  Verification rig: `graphix-fuzz run` now runs all THREE modes and
  prints per-mode `attempted/fused` + the failed-reason list — the
  diagnostic that cracked the wrapper bug in minutes after value
  gates and fused-counts had both lied. Known nested-HOF gap filed as
  #203 (resolve_static_calls doesn't descend lambda bodies — the
  inner map never resolves; classic-parity, runtime per-slot
  machinery carries correctness). **D2-filter landed** (2026-06-11):
  `FilterImpl::emit_clif` via `scaffold::emit_filter_loop` — one new
  method; the MapQ orchestration is generic over `MapFn`, so each
  remaining HOF is now just its own `emit_clif`. Same V1 gates as map
  plus the predicate type must freeze to `bool` (mirroring emit_gir);
  the pred closure enforces the BUILD-time de-fuse contract — a
  Scalar2 (may-bottom) predicate Errs, never runtime-aborts, because
  there is no runtime keep-vs-drop answer for a bottom predicate
  (only the node-walk represents it: the pred slot never fires and
  filter's output BLOCKS — pinned by a probe with an actual div-0,
  all modes Timeout-agree). The de-fuse probe that carries real
  weight is the STATICALLY-may-bottom / runtime-clean one (div by
  element, no zero present): the kernel de-fuses at build, then
  node-walks to a real value all modes agree on — a value-blind
  Timeout==Timeout agreement can't catch a wrong de-fuse, that one
  can. Direct again EXCEEDS classic: composite-tuple elements with
  accessor predicates inline (classic emit_gir requires a
  register-scalar element for single-name callbacks; verified
  attempted=0/fused=0 classic vs fused=1 direct, with the not-kept
  drop_block + push_array element MOVE confirmed in the kernel dump).
  CLIF differential: classic byte-identical; exactly the two filter
  probes' DirectJit kernels changed (the inline landing), with
  attempted 7→4 — the maximality/fewer-recursive-attempts gain.
  compare.sh's normalizer now handles the two log-noise classes
  (timestamps normalized so line PRESENCE still compares;
  nondeterministic "could not send batch" dropped). **D2-fold landed**
  (2026-06-11): `FoldFn::emit_clif` (defaulted, the 2-arg
  `(acc, elem)` twin of MapFn's), FoldQ's `Apply::emit_clif`
  orchestration (acc name/BindId from the callback's first param +
  arg pattern, init_arg = positional 1), `FoldImpl::emit_clif` via
  `scaffold::emit_fold_loop`. The scaffold's acc bind gained an
  `Option<BindId>` (classic arm passes None — CLIF differential
  proves invariance), so init-position reads of an outer binding
  named `acc` and the loop's own acc resolve BindId-first without
  collision (probe: `fold(a, acc, |acc, x| acc + x)` — the kernel
  threads the acc as a loop block param seeded from the outer
  value). Gates: map's set + acc must be a register scalar that init
  and body types agree on. BOTH the init and body closures carry the
  build-time de-fuse contract (a bottom acc poisons every later
  iteration — no per-element runtime seam exists): the plan's
  explicit parity fixtures are probes (statically-may-bottom body
  `acc / x` and init `10 / n`, both runtime-clean → de-fuse, then
  node-walk to a REAL value all modes agree on). Composite-elem
  folds (`|acc, p| acc + p.0 * p.1`) again EXCEED classic. The
  probe suite also found **#204** (pre-existing, both paths): a HOF
  callsite in OPERAND position (`k + fold(...)`) never statically
  resolves — `static_resolve.rs::visit_mut` only descends
  Module/Block/Bind/CallSite, so the site gets no analysis_pred, no
  bound function, and neither inlines nor DynCalls (classic
  attempted=0, identical gap; values correct via node-walk). Same
  class as #203. **#204 FIXED** (2026-06-11, pulled ahead of the
  Stage-F flip so the coverage audit doesn't bake position cliffs
  into the accepted set): static resolution now descends every
  child-bearing node — operands, select scrutinee/guards/arms,
  composite-literal elements, accessor sources, string
  interpolations, connect RHS, try/catch, qop — EXCEPT `Lambda`
  bodies (compile per call site, #203's territory) and
  `FusedKernel` (post-fusion synthetic). `collect_lambda_binds` is
  the canonical enumeration: an EXHAUSTIVE `NodeView` match (no `_`
  arm), so a new node variant is a compile error there instead of a
  silently-untraversed container; `visit_mut` mirrors it with
  per-type downcast arms (mutability forces concrete-type
  dispatch). Position probes (operand / select-arm / array-element
  HOFs, all `Clean`) guard the runtime behavior. Benefits BOTH
  paths: classic gains the same nested-position fusion. The full
  canonical-traversal trait method (one `visit_children` in the
  delete/sleep/refs family, retiring the narrow walkers —
  static_resolve, find_node_by_id, the jit recursion,
  walk_node_for_builtin_calls' Expr fallback) is deliberately
  deferred to Stage E, where #203's lambda-body descent forces full
  traversal anyway. Related observation recorded for E: the
  `Update::jit` recursion is also spine-only (Module/Block/Bind) —
  with #204 fixed the containing region fuses at its root so this
  rarely matters, but a mixed sync/async region root that fails
  try_fuse won't currently retry pure sub-expressions at operand
  depth. **D2 ladder COMPLETE** (2026-06-11): flat_map, filter_map,
  find, find_map, and array::init all landed in one batch — each a
  single `emit_clif` (MapFn impls for the first four; Init has its
  own `Apply::emit_clif` mirroring its GirEmitter). Per-HOF notes:
  flat_map's body must freeze to `Array<scalar>` (the
  array-returning branch of the `['b, Array<'b>]` union; bare-elem
  bodies node-walk, classic parity) and hands the scaffold an OWNED
  array — a Borrowed body source (bare Ref) is refcount-cloned via
  `ensure_owned_composite_src` (now `pub`; kernel-verified: clone +
  extend per iteration, both inputs dropped at exit). filter_map is
  scalar-in/scalar-out (the scaffold binds through the per-prim
  getter — no bind_elem; composite elems node-walk, widen with
  #150); body gate = `nullable_inner` + `scalar_prim`. find returns
  the matched element as a `Nullable<elem>` `(disc, payload)` pair —
  composite elements EXCEED classic again (kernel-verified: found
  edge CONSUMES the element via `value_new_from_array`, advance edge
  drops it). find_map's first non-null body pair IS the kernel
  result, so a Borrowed body pair is cloned via
  `ensure_owned_value_src` (now `pub`) — the scaffold's owned-pair
  contract. init: integer-frozen `n` (may-bottom n Errs =
  build-time de-fuse), the index param binds the loop counter
  Variable itself, body pushes via push_field (the runtime
  bottom-abort seam, like map). The three remaining name-only
  scaffold binds (fold acc — done earlier —, filter_map elem, init
  idx) all gained `Option<BindId>` columns (classic passes None;
  differential-proven invariant). `CompositeSource` is now `Copy`.
  OPERATIONAL RULE (bitten twice): `cargo test` does NOT rebuild
  `target/debug/graphix-fuzz` — ALWAYS `cargo build -p graphix-fuzz`
  before CLI kernel inspection or differential capture, or the dump
  shows the previous build's behavior. **Owned-array-arg widening
  landed** (2026-06-11): fresh-producer inputs (literals, slices,
  inlined-HOF results) now feed the loop scaffolds. Mechanism: a new
  ValArray-typed `LowerCtx::owned_input_stack` mirroring
  `dyncall_buf_stack` — `scaffold::adopt_owned_src` registers the
  input at loop entry (a pend inside the body frees it from
  `emit_pending_cleanup` via `graphix_valarray_drop` — the buf stack
  has the WRONG destructor for a finished ValArray), and
  `drop_owned_src` drops + pops on the normal path: exactly once on
  either path. The env-bind alternative was REJECTED — for the
  record, the CORRECT reason (an earlier writeup misstated it):
  select arms DO mark/truncate the JitEnv per arm (all four merge
  shapes — verified), but `truncate` is compile-time hygiene that
  emits NO drops, which is sound today only because arms bind
  nothing but scalars (non-scalar scrutinee binds refuse to lower).
  An env-bound owned composite input adopted inside an arm would be
  truncated at arm end without a drop — a normal-path LEAK. The
  stack entry's lifetime (loop entry → drop+pop at loop end) never
  interacts with env scoping at all. The HOF impls' Borrowed
  gate became `owned = (source == Owned)` passed through `ArraySrc`.
  Payoff beyond slices/literals: with #204 covering arg positions,
  HOF-of-HOF arguments now fuse as MULTI-LOOP SINGLE KERNELS —
  kernel-verified: `filter(map(a, f), g)` emits the map loop,
  adopts its result, runs the filter loop over it, and drops the
  intermediate exactly once at exit (attempted=4 fused=1, whole
  block). Pipeline probes: filter∘map, fold∘map, find∘filter,
  flat_map∘init, plus a pending-path probe (mid-outer-loop overflow
  bottom-abort frees the adopted intermediate — crash-detects
  wrong-destructor/double-free). **D3 landed** (2026-06-11), closing
  Stage D's functional scope: destructured `|(k, v)|` callbacks
  inline via `HofElem::leaves` — per-leaf `(BindId, position, prim)`
  triples computed by package-array's `scalar_leaves` from the
  frozen tuple element type (register-scalar leaves only; composite
  leaves node-walk — a future widening with #150), bound by
  `bind_elem` off the owned composite element BindId-first (the
  body's leaf Refs carry the pattern BindIds; the synthetic
  `__leaf{id}` names are never looked up). Sparse patterns
  (`|(k, _)|`) fall out free — `tuple_leaves` skips Ignore slots so
  unbound positions get no read. Scalar leaf Variables need no
  per-iteration drops (the element's own drop covers the
  allocation), so the pending path is unchanged. Applies to the six
  bind_elem scaffolds (map, filter, fold, flat_map, find, find_map);
  filter_map stays scalar-elem-only (its scaffold has no bind_elem —
  #150). Adding the required `leaves` field to `HofElem` made the
  compiler enumerate every construction site (12 — the
  14th-commandment payoff); classic arms pass `&[]` (bind
  bookkeeping emits nothing → CLIF-identical). Kernel-verified: the
  3-leaf fold emits element read + three leaf reads + body + element
  drop, acc threading as a block param. Stage D is now functionally
  COMPLETE — remaining gaps are recorded parity items (#150
  string/value elements, #203 nested-HOF lambda bodies, composite
  leaves); next is Stage E.

  **Stage E execution plan** (recon 2026-06-11 — the parallel period
  gives three shortcuts the original plan didn't bank on):
  (1) `build_lambda_kernel` (lowering.rs) is FusionCtx-FREE — it
  takes `(g, kernel_name, ec)`, does formal-arg → RegionInput
  translation, closure conversion (BindId-sorted captures, fn-typed
  skipped, String/Unit/Null returns refused), and caches
  `CachedKernel` by `(LambdaId, resolved FnType)` in
  `ec.fusion_kernels`. Post-Stage-A its `kernel.sig` IS a
  `KernelSig`. The direct analysis calls it directly (make
  pub(crate)) — no new cache, no new builder.
  (2) `compile_kernel_with_callees_direct` ALREADY accepts the
  callee map (passed empty today): callees compile from their GIR
  bodies (classic-proven — including GirOp::Call self/mutual
  recursion via shared FuncIds AND the tail rebind-and-jump
  machinery), only the PARENT emits via Node. So tail loops and
  recursion come FREE in E — callee-body Node-emission (and the
  has_tail_loop agreement assert) moves to Stage F prep.
  (3) The CallSite::emit_clif dispatch already has the fall-through
  shape: a missing lambda-call entry just bails = de-fuse.
  E-substages:
  - **E1 discovery**: a FULL-coverage immutable Node walker (the
    canonical `for_each_node`, exhaustive NodeView match like
    collect_lambda_binds') walks the region in try_fuse's ANALYSIS
    phase (before the jit lock); at each CallSite with resolved
    `ApplyView::Lambda(g)`: `build_lambda_kernel(g, "__dl_{lambda
    id}", ec)` (cache-hit returns the FIRST builder's name — always
    use the RETURNED fn_name, a lambda can be cached under a
    `__hof_*` name by the per-slot path), record `ExprId →
    LambdaCallInfo { fn_name, kernel: Arc<GirKernel>, captures }`
    (the apply_sites pattern) + accumulate `BTreeMap<ArcStr,
    Arc<GirKernel>>` callees including the build's transitive
    `sub_called`. Thread both into LowerCtx.
    `define_kernel_body`: a Node-emitted parent declares FuncRefs
    for ALL funcids entries (its GIR body is empty —
    collect_call_sites finds nothing).
  - **E2 call emission**: CallSite::emit_clif's Lambda arm looks up
    LambdaCallInfo by spec id (miss → bail = de-fuse); marshals the
    COMBINED formals+captures list in the callee's kind-grouped ABI
    order (scalars, composites array→tuple→struct, value-shapes
    variant→nullable two words — the Node twin of
    compile_call_clif_args, same owned-arg post-call drops);
    captures resolve from the parent env BindId-first (V1: scalar +
    composite captures; value-shape captures bail — the variant/
    nullable env tables are still name-keyed); result unpacked per
    the callee's return ABI (1 result scalar/composite, 2 results
    value-shape, classified Owned).
  - **E3 verification**: probes — multi-callsite same lambda,
    captures (scalar + composite), recursion (`let rec` fib),
    mutual recursion, deep tail recursion (exercises the GIR tail
    machinery through a direct parent), composite/value args +
    returns; recursion-weighted fuzz; full battery + CLIF
    differential (classic invariant; DirectJit gains call kernels).

  **E1+E2 LANDED** (2026-06-11). Working and kernel-verified: simple
  calls, multi-callsite-one-kernel, scalar captures (closure
  conversion — the capture rides the parent region as an input and
  forwards as the trailing call arg; verified `imul`-only callee),
  f64, composite literal args (owned, dropped post-call), labeled
  args with defaults. Probe suite: 11 probes, 6 Clean. The `Clean`
  noise set gained "function-valued let" (a lambda-valued let can
  NEVER emit by design — the binding node-walks while call sites
  fuse; distinct message so it doesn't mask real let-shape gaps).
  Three findings from the campaign:
  (a) TWO silent `Ok(None)` paths in try_fuse made `attempted`
  disagree with the failure list (FusedKernel::new Err after a
  successful compile; the duplicate-basename refusal) — both now
  log. The debugging rule held: every silent fallback eventually
  costs an investigation.
  (b) **#205** (pre-existing classic GIR, newly reachable):
  `GirStmt::Return` routes on the un-normalized select arm-union
  type — a lambda kernel whose body is a Nullable-returning select
  fails "GIR malformed". Classic never built such a kernel (no
  recurse-on-failure granularity). Probe pinned, flip on fix.
  (c) **Recursive lambdas have NEVER fused** (classic included):
  `build_lambda_kernel` routes through
  `build_kir_kernel_from_region`, which hardcodes `self_info: None`
  ("Regions have no self-recursion") — the complete
  `SelfInfo`/tail-rebind machinery exists in `build_kernel` but
  nothing connects it at this entry; classic's consumers (per-slot
  HOF callbacks) never needed it. **E3 = construct `SelfInfo` in
  `build_lambda_kernel`** (name + params/source_args from the
  already-translated inputs) and pass through a self-aware build,
  unlocking recursion + the tail rebind-and-jump for BOTH paths.
  Probes pinned (recursion at depth 500 — note 50k-deep recursion
  OVERFLOWS THE NODE-WALK'S STACK, a canonical-model reality; the
  fused tail loop won't have that limit, so E3 adds a fused-only
  deep probe). Also added: `ExecCtx::fusion_building` — a
  re-entrancy guard in `build_lambda_kernel` (mutual recursion
  would otherwise recurse the build forever: the cache entry lands
  on completion and per-build `known_fns` only covers
  self-recursion; pre-existing classic landmine, now a de-fuse). (#202 turned out NOT to be Apply::emit_clif work:
  `cast<T>(x)` is a TypeCast NODE whose emit_clif relay exists — the
  gap is `emit_cast_node` refusing the union `[T, Error]` result
  shape; an independent graphix-compiler item.)

  **E3 LANDED** (2026-06-11) — recursive lambdas fuse for the first
  time on any path, and the recon found a LIVE WRONG-CODE CRASH
  before writing a line of the feature. The actual build-killer was
  NOT the `self_info: None` hardcode (a recursive body would still
  have lowered as plain `GirOp::Call` native recursion): it was the
  CAPTURES SCAN — a rec binding's env type is a TVar-wrapped `Fn`
  that the shallow `Type::Fn` skip misses, so `freeze_concrete`
  rejected the "capture" and silently killed every recursive build.
  The instrumented bisect (entry/exit eprintlns + the
  rec-without-self-call control probe, which DID fuse) found it in
  two runs. E3's pieces:
  - **Self-reference is not a capture**: `build_lambda_kernel` gains
    `self_bind: Option<BindId>` (the call site's fnode Ref id,
    threaded from discovery / `ensure_lambda_kernel` / the per-slot
    path) and skips it in the scan. Recursion detection =
    `external.contains(self_bind)`.
  - **#206, found by auditing the name-matched tail-call plan**:
    `{ let f = |x| x+1; let f = |n| f(n)*2; f(3) }` — f2's body call
    to the shadowed OUTER f name-resolved against f2's own
    `known_fns` self entry (registered by `finish_kernel` pre-body)
    → `GirOp::Call` on ITSELF → stack overflow. Live under
    DirectJit (E1 discovery builds what classic's planner never
    did); the SAFETY INVARIANT comment only covered
    lambda-binds-in-bodies. Fix: `KnownFusedFn::self_bind` — every
    name-keyed `find_fn` resolution must match the call site's
    fnode Ref BindId when the entry carries one. Repro in
    findings/lambda-jun2026. NOTE for #203: when inner-body sites
    resolve statically, `known_fns` itself must re-key by BindId
    (the resolved outer `f` would early-return on the self entry's
    `contains_key`).
  - **`SelfInfo` = `{ name, bind_id, source_args }`** (the `params`
    field had ZERO consumers — deleted). `try_emit_tail_call`
    matches self-calls by fnode-Ref BindId, not ident.
    `body_has_self_tail_call` (pure pre-scan mirroring `emit_tail`'s
    positions: root / Block last / Select arms / ExplicitParens)
    decides `has_tail_loop` BEFORE emission; over-approximation is
    a vestigial loop head, under-approximation can't happen by
    construction (self_info is only Some when it returned true).
  - **Captures + tail loops compose**: `tail_call_slots` could NOT
    be truncated to formals — it doubles as the runtime arg layout
    (`arg_layout`, gir_interp.rs). Instead the JIT TailCall arm
    rebinds only the leading `args.len()` slots (assert relaxed to
    `<=`; the clone-bump loop took `.take(args.len())` — it indexed
    `composite_sources[i]` across ALL slots, an OOB panic with
    captures). Trailing capture slots stay bound: loop-invariant
    within one kernel invocation.
  - **Tail-loop gate**: formals must all be Prim/Array/Tuple/Struct
    (the JIT rebinds Scalar+ValArray only); otherwise `has_tail`
    stays false and self-calls remain plain native recursion —
    correct, just stack-bound. Per-slot HOF path threads the
    callback's Ref BindId too: a recursive NAMED callback's tail
    self-call fuses (BindId-matched), its non-tail self-call
    de-fuses on the `__hof_*`/source-name miss (sound asymmetry).
  - **Direct-path Bind arm**: rec FN-valued lets now get the
    "function-valued let" treatment (binding node-walks, call sites
    fuse — Clean-compatible); rec non-fn lets (reactive feedback)
    keep a distinct refusal.
  - **Kernel-verified**: the tail probe compiles to a textbook loop —
    `block1(n, acc)` head, `icmp`+`isub`+`iadd`+`jump block1`, no
    call instruction; cranelift's SSA builder turned the rebinds
    into block params. fib(15) (double recursion) and
    capture-under-tail-loop probes Clean. The fused-only deep probe
    runs the SAME loop at 5M depth (node-walk overflows ~50k —
    DirectJit-only by design, value asserted against the closed
    form). Probe ladder: both pinned probes flipped to Clean + fib +
    capture + #206 shadow probe; 5 recursion seeds added to the
    fuzz corpus (recursion-weighted mutation).
- **E — cross-kernel calls + tail loops**: callee discovery prepass
  (CachedKernel build minus emit_body — already pure analysis), lambda-
  CallSite emit + captures as trailing args (BindId-keyed env lookup),
  tail detection as a pure Node predicate + rebind-and-jump emission.
  GIR-vs-Node `has_tail_loop` agreement assert while both live.
- **F — flip + delete**: long fuzz soak + FuseExpect coverage audit, then
  remove the flag and delete gir.rs, the emit_*/compile_expr families,
  `GirEmitter`/`ApplyView::FusedBuiltin`, `BodyEmitter`, the central
  fuse()/FusePlan walk, and node_shape's GirOp half (re-expressed as
  `KernelMatcher` over `EmitTag`s recorded during emission). Also dies

  **Stage F sequencing** (the E-period shortcut must be repaid before
  anything deletes — GIR bodies are load-bearing in three places):
  F0a callee bodies via Node (landed, below) → F0b self-calls + tail
  loops on the Node path → F0c per-slot HOF (`fuse_callsite`) + split
  (`build_body_split`) kernels via Node → F1 soak + FuseExpect audit →
  F2 flip → F3 delete → F4 node_shape EmitTags. Scope fact pinned by
  probe: lambda-calls-OTHER-lambda inside a callee body has NEVER
  fused on any path (inner sites are #203-unresolved; per-callee
  `known_fns` starts empty, so `find_fn` misses and the callee build
  fails) — the `sub_called` transitive-closure plumbing serves a
  currently-unreachable case. A callee body's only cross-kernel
  reference is SELF.

  **F0a LANDED** (2026-06-12): non-recursive callee lambda bodies emit
  via the Node path. `discover_lambda_calls` returns a third map —
  callee-kernel identity (`Arc::as_ptr`, the define loop's cache key)
  → body Node, reached live through this site's resolved `GXLambda`
  (recursive callees excluded via the new
  `lowering::CachedKernel::is_rec`; they keep GIR bodies until F0b).
  `compile_kernel_with_callees_direct` builds per-callee
  `NodeBodyEmitter`s (empty apply/lambda site maps — see the scope
  fact) and the impl's define loop routes by identity
  (`BTreeMap<usize, &dyn BodyEmitter>` replacing the parent-only
  `Option`). The `needed` FuncRef set became per-emitter
  (`lambda_call_sites` fn_names minus self): for the parent this
  equals the old funcids-minus-self; for a Node-emitted callee it's
  EMPTY, matching the no-unused-FuncRefs shape of its GIR body
  (differential-proven). NEW DIAGNOSTIC for the silent-fallback
  class: the define loop `log::trace!`s "Node-emitted body" vs "GIR
  body" per kernel — routing verified live in both directions
  (callee `f` Node-emitted; recursive `lp` GIR). Gates: compiler
  115/115, fixtures 1423/1423, fuzz 18/18, regress 17/17, generate
  200→0, CLIF differential 45/45 byte-identical.

  **F0b LANDED** (2026-06-12): recursive callee bodies emit via the
  Node path — NO lambda callee uses its GIR body on the direct mode
  any more. Two pure refactors first, each gated byte-identical
  (45/45): (1) the GIR `TailCall` arm's env-driven core extracted as
  `emit_tail_rebind_jump(b, env, ctx, new_vals, sources)` (everything
  after arg evaluation was already GIR-free); (2) `emit_select_node`
  split into `classify_select_scrutinee` + `emit_select_arms` (the
  pattern chain — tcond/scond/guard/binds/fail-trap — takes an
  arm-body closure) + `emit_select_value_arm` (the merge-widening arm
  emission, verbatim). New tail machinery, mirroring lowering's
  shapes 1:1: `emit_body_tail` (the `emit_tail` twin — root / Block
  last / Select arms / ExplicitParens; **no `in_tail` flag exists
  anywhere**: tail self-calls are intercepted structurally BEFORE
  value emission, so `CallSite::emit_clif` only ever sees self-calls
  in value position), `emit_select_node_tail` (arms TERMINATE — the
  `compile_select_stmt` twin; refuses possibly-bottom scrutinees, no
  merge validity channel to poison), `emit_self_tail_call` (eval args
  → `emit_tail_rebind_jump`), and `emit_block_stmt` (extracted from
  `emit_block_node`, shared with the tail block walk). Return-path
  leak-safety is structural: `emit_kernel_return` drops ALL owned
  locals at any depth, so nested-scope returns through tail contexts
  can't leak. Self info threads as `CalleeBody { body, self_call:
  Option<(BindId, LambdaCallInfo)> }` from discovery →
  `BodyEmitter::self_call()` → `LowerCtx.self_call`; the
  `needed`-FuncRef set imports the kernel's OWN FuncRef for recursive
  kernels only (no renumbering of existing kernels). VALUE-position
  self-calls reuse `emit_lambda_call_node` against the own FuncRef —
  captures forward from the kernel's own params for free. TRAP
  (found by probe, cost one cycle): the inner self-callsite is
  #203-UNRESOLVED, so `self.function` is None — the self check must
  live OUTSIDE the resolved-Apply block in `CallSite::emit_clif`
  (tail calls worked immediately because the walker doesn't consult
  `self.function`; value-position rec/fib silently lost fusion until
  the check moved). The Node-emitted `lp` loop came out TIGHTER than
  the GIR one (fewer dead validity consts). Non-recursive bodies
  keep the value-position emission deliberately — per-arm returns
  would be equivalent codegen but churn every existing kernel's
  CLIF. Gates: compiler 115/115, fixtures 1423/1423, fuzz 18/18 (the
  5M deep-tail probe now exercises the NODE-emitted loop), regress
  17/17, generate 200→0, CLIF differential 45/45 byte-identical.
  Remaining GIR-body consumers on the direct mode (F0c): per-slot
  HOF template kernels (`fuse_callsite` → `jit_compile_split_kernel`)
  and body-split sub-kernels (`build_body_split`).

  **F0c LANDED** (2026-06-12): per-slot kernels route by mode —
  `ExecCtx::direct_node_jit` (the `jit_enabled` pattern: set by
  `compile()` from the flag, read by the outside-`fuse()` JIT sites)
  selects `compile_kernel_with_callees_direct` (body Node + self-call
  info + the split region's cloned `apply_sites`) vs the classic GIR
  entry. `compile_kernel_with_callees_direct` gained
  `parent_self_call` (the per-slot "parent" IS a lambda kernel;
  regions pass None). The `.ok()` that swallowed per-slot compile
  errors now logs — the silent-fallback rule. VERIFICATION
  ASYMMETRY, noted honestly: the split branch is proven live
  (`__split_*` under DirectJit traces "Node-emitted body"; classic
  stays GIR), but the WHOLE-BODY template branch has no live direct
  probe — D2's MapQ inlining subsumes nearly its entire trigger
  space (pure-but-uninlineable callbacks: select bodies, composite
  elems, qop bodies all INLINE on direct; impure bodies take the
  split). The branch is code-uniform with the proven split branch
  (same fn, same routing, different args) and the failure-trace
  line is the watchdog. With F0a+b+c, NO kernel on the direct mode
  compiles from a GIR body — the GIR body build itself
  (`emit_body`, classic) becomes sig-only dead weight at the flip.
  Gates: fuzz 18/18, fixtures 1423/1423, compiler 115/115, CLIF
  differential 45/45, regress 17/17, generate 200→0. Also dies
  at F (Eric, 2026-06-10): `Update::splice_child` + `fusion::splice_into`
  (the parent-swap protocol never searches by ExprId; the impure-HOF
  split splice is re-expressed as `template.jit()` in Stage D), and the
  `Expr.typ` typed-AST OnceCell — its only post-F reader is the lambda
  default-arg type check (callsite.rs), re-homed onto `LambdaDef.argspec`;
  with the cell gone, `Update::typecheck`/`typecheck_inner` collapse into
  one method (the wrapper existed only to propagate into the cell — a
  fossil of the pre-node-walk JIT-an-Expr era). `clone_rebind` STAYS:
  it's the runtime per-slot impure-HOF instantiation (fresh BindIds per
  slot's async residue, shared kernel Arcs), not fusion plumbing — the
  distributed design leans on it harder (template.jit() once, then
  clone_rebind per slot preserving spliced FusedKernels).

  **F1 (in progress, 2026-06-12)**: generate-5000 soak CLEAN. The
  mutation soak SIGSEGV'd the whole fuzzer (no stderr — native-frame
  crash); `GRAPHIX_FUZZ_ECHO` per-mutant forensics isolated
  `{ let v = str::concat(); i64:0 }` → **#214, pre-existing at HEAD**:
  String DynCall results rode the SCALAR convention (no site-level
  pre_pending branch), so a pending dispatch's sentinel-zero flowed
  into owned-ArcStr drops (`graphix_arcstr_drop(0)` → SIGSEGV; the
  untested String-`let` + scope-exit drop shared the seam, both
  paths). FIXED: String results branch at the site like
  composite/Value via the new shared `emit_dyncall_pending_branch`
  (one helper, six arms across both paths — was 4 duplicated copies
  + 2 missing). Defense-in-depth: all five JIT drop helpers
  (`valarray`/`value`/`arcstr`/`value_buf`/`string_buf`) null-check
  and PANIC (panic at an `extern "C"` boundary aborts with the
  message printed — a loud deterministic failure instead of UB).
  The two niche-carrying helpers (`graphix_value_drop`,
  `graphix_arcstr_drop`) retyped to raw words (bit-identical ABI) so
  the check runs BEFORE an invalid `Value`/`ArcStr` materializes;
  `Value` disc 0 is unambiguous (discriminants are bitmasks from
  0x1). EXPOSED by the fix (the crash was masking it): a pending
  DynCall in DEAD position bottoms the whole fused kernel (interp =
  0, jit = Timeout) — whole-kernel pending is coarser than the
  canonical per-node bottom. Membership: variadic calls with zero
  varargs (`str::concat()`, `str::join(#sep:)`) — zero-input nodes
  that never fire canonically; used-position agrees
  (Timeout==Timeout). Decision pending (de-fuse degenerate dyncalls /
  liveness / per-value pending taint — related: "dead-elim dropped
  in V1" in the decisions log). Repro stored as
  `findings/dyncall-jun2026/*.gx.pending` (excluded from regress
  until the divergence resolves).

  **F1 COMPLETE (2026-06-12)** modulo two pending decisions. The
  post-fix soak then died at iter ~1000 on the OTHER process-killer
  (runaway-recursion mutant → node-walk stack overflow → SIGABRT),
  proving in-process campaigns can't converge → built **fuzzer
  subprocess crash isolation**: campaign checks run via the hidden
  `check-one` worker (program on stdin, VERDICT line on stdout);
  child signal-death → `crash_NNNNNN.gx` finding with wait status +
  stderr tail (the overflow-vs-silent-SIGSEGV triage signal);
  DIVERGE → in-process re-check of the proven-non-crashing program
  feeds the existing record pipeline; minimization ALSO isolated
  (`minimize-one` — a REDUCTION of a benign divergence can itself
  be a crasher, e.g. a dropped base case), child death → record
  unminimized. `GRAPHIX_FUZZ_INPROC=1` opts back in-process.
  Throughput: 100 mutants/5s wall (resolver spin-up dominates).
  The 3000@777 mutation soak then COMPLETED (first time ever): 1
  divergence (the dead-pend class, isolated-minimized to
  `{str::concat(); i64:0}`) + 1 crash (runaway recursion —
  accepted: infinite recursion can't produce a value; the
  overflow-abort is the node-walk's artifact). ZERO unexplained
  findings. FuseExpect audit (`GRAPHIX_FUSE_AUDIT=1 cargo test -p
  graphix-tests -- jit --nocapture`): 612 fixtures → 450 OK,
  **155 GAINS** (None→Jit — the recurse-on-failure + D2-inlining
  dividend), **6 LOSSES** in two closable clusters: map literals
  (no direct emit_clif — #143 unported) and abstract types in
  composites (resolve_abstract not applied on the direct freeze —
  #145 unported); values agree everywhere (losses node-walk).
  Before F2: the dead-pend decision, close-vs-accept on the two
  loss clusters (recommend close), and the one-time FuseExpect
  re-annotation of the gains.

  **Dead-pend RESOLVED (Eric, 2026-06-12)** — at the language level,
  upstream of fusion: a sync variadic builtin called with no
  positional arguments has no data inputs and can never fire, so it
  is now a COMPILE ERROR (`reject_dead_variadic_call` in
  callsite.rs; labeled args are config, not data — `join(#sep:)` is
  caught; positional-formal builtins keep their arity errors;
  first-class-value calls escape to the safe runtime bottom).
  `never()` is the sanctioned intentional bottom and was
  reclassified `EffectKind::Async` (the "later, autonomously, or
  never" contract's limiting case) — exempting it from the error
  and making it a fusion boundary instead of an always-pending
  fused dyncall (zero FuseExpect fallout). Both dyncall-jun2026
  findings promoted to the regress corpus as CompileErr-agreement
  guards. Gates: 115/115, 1429/1429 (3 new fixtures), regress
  19/19, generate 300 clean, fuzz 500@42 completed (one new
  accepted-class recursion crash recorded by isolation). F1's
  remaining pre-F2 items: close the two audit loss clusters
  (approved) + the one-time FuseExpect re-annotation.

  **Audit loss clusters CLOSED (2026-06-13)** — the full audit now
  shows ZERO Jit→None losses (242 Jit→Jit, 207 None→None, 165
  None→Jit gains awaiting the flip's re-annotation). Maps (#217):
  `Map::emit_clif` → `emit_map_new_node` const-folds through the
  shared `const_map` into `emit_const_node`'s Value-constant arm —
  classic's exact contract (all-constant entries or de-fuse).
  Abstract types (#218), four seams found via the new
  FUSEAUDIT-BLOCKER diagnostics: (1) `resolve_abstract` gained a
  TVar-deref arm — INFERRED binding types are TVar-wrapped, unlike
  the declared signature types classic resolves; (2)
  `collect_region_inputs` resolves before freezing (region inputs
  crossing module-level binds were silently skipped — slot kinds
  from the resolved type, feeder Refs keep the type-system view);
  (3) try_fuse's return-type gate retries freeze through
  resolve_abstract (an abstract-returning region silently logged NO
  attempt); (4) emit-time resolution: `BodyEmitter::type_env()` →
  NodeBodyEmitter carries `&Env` → `LowerCtx.type_env` →
  `resolve_node_typ`/`freeze_node_typ` (retry-on-failure — the
  common path pays nothing), applied at tuple/struct elem reads +
  arith prim derivation. `emit_lambda_call_node` now types arg
  slots from the CALLEE's signature (`LambdaCallInfo.arg_types`) —
  classic's discipline; caller-side node types never freeze.
  Classic ALSO gained: abstract_type_in_typedef now fuses on
  classic (annotation upgraded per the bidirectional check).
  Permanent diagnostics: trait-default emit error includes spec
  text; audit dumps per-region blockers. Gates: 115/115, 1429/1429,
  regress 19/19, generate clean, fuzz 800@99 complete (3
  accepted-class crashes: 2 runaway recursion + 1
  `array::init(i64::MAX)` HANG — the isolation's outer deadline
  caught what would have WEDGED the in-process harness, a native
  loop that can't yield). NEXT: F2 flip.

Final layout: `kernel_abi.rs`; `jit/{mod,scaffold,helpers,intern}.rs`;
`fused_node.rs` (GirNode → FusedNode); `fusion/{mod,analysis,builder}.rs`;
emission shims distributed through `node/*.rs` next to their `update`s.

## Risks being tracked

1. `by_kernel` poisoning on failed define — evict `to_define` keys on
   compile Err (compile-Err is the COMMON no-fuse signal post-flip).
2. Fusability discovery costs a cranelift attempt per non-fusable
   subtree, multiplied by recurse-on-failure — accept for V1, measure;
   pre-filter only if compile times regress.
3. Trait defaults are silent (a new node type silently doesn't fuse) —
   correct degradation; FuseExpect + the fusion-gap metric guard it.
4. `Ok(None)`-after-emission unenforceable — debug-assert block
   instruction count unchanged across a None return.
5. `infer_body_rtype`'s emit-to-learn-type fallback disappears — those
   regions node-walk; detect the population via the Stage-F audit.
6. jit()/splice interplay: parent must `delete(ctx)` before swap — one
   helper on `Node` so no impl can forget half of it.

**Oracle caveat (don't chase as a JIT bug):** an infinite PURE tail
recursion shows `interp = value` (the reactive node-walk advances it
per-cycle) vs `jit = Timeout` (the native tail-loop can't yield). This is
an accepted, intended divergence — see `final_jit_architecture.md` Part 2.

## Decisions log

- emit_clif over a closed declarative HofShape set: the old emit_gir
  "modularity" was thin (packages picked from 7 compiler-owned GirOp
  scaffolds); raw cranelift + the scaffold library gives packages real
  power while the compiler stays builtin-agnostic. (Eric, 2026-06-10)
- Distributed `jit()` IS the transition, not a follow-up restructure —
  build the final structure directly, use the old guts as library code,
  delete what goes unused. (Eric, 2026-06-10)
- `Node::is_sync` + MapQ eager per-element fast path: deferred until
  after the flip (pure perf for the non-fused tier).
- Sink/dead-elim passes dropped in V1; only inline scrutinee
  stabilization kept (oracle-gated by the `sink_*` fixtures).

## F2 — the flip (2026-06-13)

`CFlag::DirectNodeJit` is GONE: `ExecCtx::direct_node_jit` is always
true (field dies in F3), `fuse()` is `jit_node` recursion
unconditionally (the classic planner body deleted; its helpers are
F3's worklist), `Mode::DirectJit` is an alias of `Mode::Jit` (probe
suite prunes in F3), the audit harness runs default flags. 168
FuseExpect upgrades landed (scripted from the suite's own
bidirectional-check messages); 5 GirOp-tag NodeShape pins neutralized
with F4 markers (#213 — direct kernels carry no GIR body to
tag-match).

The flip surfaced SIX live defects, all root-caused and fixed — each
was invisible pre-flip because classic never fused the triggering
shape:

1. **Handler-ful `?` fused** (catch4/is_err timeouts): error delivery
   is a VARIABLE WRITE to the catch's BindId — an effect a kernel
   can't perform; fusing swallowed the error. Gated in
   `Qop::emit_clif` AND classic's `NodeView::Qop` lowering (per-slot
   GIR builds still use it).
2. **Feeder `ref_var` keyed by region id** (connect-fed regions saw
   one update then nothing): `Rt::ref_var/unref_var` are keyed
   `(BindId, top_id)`; feeders registered under the region's interior
   ExprId stranded the real top expression at count zero once the
   spliced original's Refs unref'd. `ExecCtx::fuse_top_id` (set per
   `compile()`) now keys feeders. Latent in classic's planner too.
3. **First dispatch ≠ init** (net_list_table): a DynCallSlot's
   freshly-constructed inner Apply saw the outer cycle's
   `init=false`, so labeled-default Nodes (Constants) never fired —
   `sort(#dir, #numeric, a)` pended forever when the kernel's first
   fire was post-init (async-fed). `dispatch` now forces the init
   view for each fresh Apply's first update (the `CallSite::bind`
   contract).
4. **Update-history-sensitive builtins fused** (fs watch): the
   dispatch protocol re-delivers EVERY arg as a fresh update each
   dispatch — `skip(#n:1, e)` saw `n` "update" every cycle and never
   passed an event. Once/Take/Skip/Count/Uniq reclassified
   `EffectKind::Async` (same honesty as Never): "Sync" for dispatch
   means per-cycle REPLAYABLE, which stateful counters aren't.
5. **Lock-discipline deadlocks** (the parallel-suite wedge — two
   edges): (a) recursion inside `with_deref` closures held TVar read
   guards across `ABSTRACT_REGISTRY` reads and further TVar
   acquisitions (`resolve_abstract`'s TVar arm, `abi_kind`,
   `freeze_concrete_d`, `nullable_inner`) — all now CLONE the
   deref'd type out and run guard-free; (b) `match
   REGISTRY.read().get(..).cloned() { .. r(&c) .. }` kept the read
   guard alive as a MATCH TEMPORARY through the recursion — fatal
   under parking_lot's fair lock the moment a writer (`check_sig`)
   queued between two reads. Suite went from 2/3-wedging to 4/4 ×
   1429 green.
6. **Dead bottoms poison kernels** (post-flip generate findings,
   `findings/flip-jun2026`): classic's prune pass removed dead
   statements; the direct path emitted them, and a dead div0 inside
   a discarded composite aborted the whole kernel. Dead-statement
   elimination now runs at the `emit_block_node` seam — a Bind is
   dead iff no later sibling/tail references its bound ids; a bare
   expression statement's value is always unread — gated on a
   conservative `stmt_subtree_effect_free` walk (no Connect /
   ConnectDeref / CallSite / Qop / OrNever anywhere): skipping an
   effectful statement would convert "de-fuse and node-walk the
   effect" into silently DROPPING it (the #164 env-accounting probe
   caught exactly that with a skipped `counter <- v`).

The flip's defect profile vindicates the staging: every bug was in
the NEWLY-REACHABLE fusion surface, none in the ported emission
itself, and the canonical node-walk caught all of them differentially.

## Semantic contracts for emit work (consolidated from the F2 defects)

Every future `emit_clif` impl, and the F3/F4 work, must preserve these.
They are the distilled form of the six flip defects — each was obvious
in hindsight and invisible in advance.

1. **Three different things masquerade as "pure":** *Sync* (output on
   the trigger's cycle — the EffectKind contract), *effect-free* (no
   variable writes / logging / IO), and *replayable* (a pure function
   of currently-cached args). The DISPATCH protocol requires
   replayability: it re-delivers EVERY arg as a fresh update on every
   dispatch, so any builtin whose semantics depend on WHICH arg
   updated WHEN (once/take/skip/count/uniq) must be `Async` even
   though it is Sync by the letter. When auditing a builtin for
   fusability ask: "is its update a pure function of its cached
   args?" — not "does it produce on the same cycle?".

2. **Effects fuse never.** The known effect carriers in stmt position:
   `Connect`/`ConnectDeref` (variable writes), handler-ful `?` (writes
   the catch's variable — gated in `Qop::emit_clif`), `$` (logs), any
   CallSite to an effectful builtin. The SAFE failure mode is a
   build-time `Err` (de-fuse → the node-walk performs the effect);
   the FATAL failure mode is skipping/eliding the node (the effect is
   silently dropped). Dead-statement elimination is therefore gated
   on `stmt_subtree_effect_free` — conservative by construction (ALL
   CallSites count as effectful; we cannot consult `builtin_effects`
   at emit time). If you widen the eliminable set, the
   env-accounting probe (#164) and the connect fixtures are the
   tripwires.

3. **First dispatch IS init.** A `DynCallSlot`'s freshly-constructed
   inner Apply must see `event.init = true` on its first update
   (labeled-default Nodes are init-firing Constants). Any new
   dispatch-like seam (F4's EmitTags work, future eager paths) must
   preserve this — the symptom of forgetting is a kernel that works
   when first fired at startup and pends forever when first fired by
   an async input.

4. **Runtime wake-ups are keyed `(BindId, top_id)`.** Anything that
   creates Refs at fusion time (feeders today; anything F3 touches)
   must register under the REAL top expression id
   (`ExecCtx::fuse_top_id`), never an interior ExprId. The symptom:
   exactly one update delivered, then silence.

5. **Lock discipline for `Type` and the registry:** never recurse or
   take another lock inside a `with_deref` closure — clone the
   deref'd type out first (`t.with_deref(|r| r.cloned())`); never
   hold `ABSTRACT_REGISTRY.read()` as a match/expr temporary across a
   recursion (`let c = REG.read().get(..).cloned(); match c {..}`).
   parking_lot's locks are fair and non-reentrant: a queued writer
   blocks new readers, so guard-across-recursion deadlocks the whole
   process the moment compiles run concurrently. Registry VALUES
   carry live TVar cells shared across every ExecCtx in the process —
   treat them as cross-thread state.

6. **Dead bottoms must not poison kernels.** Canonically a value (or
   bottom) flows only to its consumers; the kernel's composite
   producers abort on bottom elements (no validity channel in
   arrays), so dead statements must be eliminated before emission
   (emit_block_node) — that is classic's prune semantics re-homed.
   The remaining documented gap is #219: a kernel INPUT that never
   fires bottoms the kernel even where the canonical output doesn't
   consume it (the JIT aborts on missing inputs). Full fix = fire
   kernels at init + per-param validity at the JIT entry.

## F3 execution notes (the delete, in dependency order)

- **`build_lambda_kernel` still LOWERS GIR bodies** (the parallel-
  period `CachedKernel` carries sig + body; nothing READS the body on
  the direct mode, but the build can still FAIL on body lowering —
  de-fusing things the direct emitters could handle, e.g. the
  fold-over-abstract-elem callee). F3 step one: make the kernel build
  sig-only (drop `emit_body`), THEN delete the emit_* family. Expect
  a small coverage GAIN when body-lowering failures stop gating
  discovery.
- Dead now, delete freely: the classic `fuse()` helpers (walk /
  CandidateKind / RegionPlan / build_region), `jit_compile_split_
  kernel`'s `compile_kernel_with_callees` else-branch + the
  `ec.direct_node_jit` field and its gates, `Mode::DirectJit` + the
  redundant third probe runs in graphix-fuzz, `GirEmitter` +
  `ApplyView::FusedBuiltin` + every package `emit_gir` impl +
  `EvalCached::FUSABLE`/`emit_gir`.
- `GirKernel.body` empties → `GirStmt`/`GirExpr`/`GirOp` delete →
  `compile_body`/`compile_expr`/`compile_scalar` family in gir_jit.rs
  falls out; the scaffolds' GIR-arm callers go with it (the emit_*
  loop arms — scaffolds themselves STAY, they serve Apply::emit_clif).
- `KnownFusedFn.body_fn_name` and friends: re-examine after the
  body delete; when #203 lands, `known_fns` must re-key by BindId.
- The five GirOp-tag NodeShape pins are parked in fixtures with
  literal `F4 (#213)` markers — grep `F4 (#213)` to restore them as
  EmitTag assertions.
- The fixture corpus now encodes the POST-flip coverage map (168
  upgrades). Any F3-induced FuseExpect regression is a real coverage
  loss — the bidirectional check will name it.

## F3 — the delete (landed 2026-06-12)

The GIR IR is gone. ~12,500 lines removed across three gated chunks
(every chunk: full suite + compiler tests + fuzz probes green; zero
FuseExpect drift end to end — the delete was behavior-preserving by
construction and by measurement). Final gates: 1429/1429 ×2, 92/92
compiler (the ~23 hand-built-GIR-kernel tests died with the IR; their
scenarios live on as graphix-source fixtures from Stage E), 18/18
probes, regress 22/22, generate 600 → 0 divergences 0 crashes,
mutation 1500 → see ledger.

**Chunk 1 — sig-only kernel builds.** `build_lambda_kernel` no longer
lowers a body: it derives the `KernelSig` (+ `KnownFusedFn`) from the
resolved FnType and capture scan, exactly the gates that used to
precede `emit_body`. The body is validated by the compile attempt
itself ("is it fusable IS the compile attempt"). `sig_from_inputs`
(fusion/mod.rs) became the single sig builder for all three paths —
try_fuse regions, lambda callees (formals carry `bind_id: None`),
and body-split sub-regions — returning `arg_types` for the
cross-kernel marshalling authority. Two behavioral seams to know:

- **`fuse_callsite` falls through to the split path on a FAILED
  whole-body JIT compile**, not just on a failed build. Pre-F3 an
  async callback body failed GIR lowering → split path; sig-only
  builds always succeed on shape, so the async-ness now surfaces at
  the compile attempt — without the fall-through, impure HOF
  callbacks would have silently lost split fusion.
- **`build_body_split` gained try_fuse's gates** (freeze_region_return
  + the non-scalar duplicate-basename refusal) since it no longer
  inherits them from the GIR build.

**Chunk 2 — the dead families.** lowering.rs 5278→1316 lines (the
whole emit_* node→GIR family, FusionCtx, prune/sink/stabilize,
infer_body_rtype, ensure_lambda_kernel, RegionInput/Source,
KernelParams/populate_kernel_inputs/build_kernel/finish_kernel);
walker.rs deleted; builder.rs is just the FusedKernel carrier;
`GirEmitter` + `ApplyView::FusedBuiltin` + `EvalCached::FUSABLE` +
every package `emit_gir` deleted (MapFn/FoldFn keep only
`emit_clif`); `ec.direct_node_jit` and `Mode::DirectJit` gone
(fuzz probes are two-mode: `agree`/`agree_fused`/`agree_fused_clean`,
test fns renamed `jit_*`). One REAL replacement hid in the deletes:
`walk_node_for_builtin_calls`'s catch-all arm fell back to an
EXPR-based walker (`walk_for_builtin_calls`) that still read the
typed-AST cell. It now rides `for_each_node` — the canonical
full-coverage walker — so every discovered site registers through
the CallSite's RESOLVED FnType (the Expr fallback was the half that
still carried the latent unresolved-FnType class).

**Chunk 3 — the IR and the wrapper.** gir.rs 1415→~70 lines (the
kernel_abi re-export, BinOp/CmpOp/BoolOp, KnownFusedFn — which lost
its reader-less `body_fn_name`); gir_jit.rs 11645→~6500 (the whole
compile_expr/compile_body GIR→CLIF family, the GIR loop arms, the
GirExpr marshal/classify twins, collect_strings/values prewalks —
lazy interning is the only constant-table source now — and the
~1700-line hand-built-IR test module); node_shape.rs lost
GirOpTag/visit_ops (GirMatcher matches sig facts only — returns +
params — until F4's EmitTags). **`GirKernel` dissolved**: the
`{ sig, body }` wrapper became `Arc<KernelSig>` everywhere — the
Arc IS the compiled-callable handle, and `Jit::by_kernel` keys its
pointer identity. `BodyEmitter` is mandatory in the define loop
(a kernel without a recorded body emitter is a hard error, not a
silent fallback — the invariant the F0a trace used to watch).

**What stayed, deliberately:** the scaffolds (gir_jit_scaffold.rs —
they serve `Apply::emit_clif`), `emit_tail_rebind_jump`,
`CompositeSource` + `node_composite_source`, the pending/taint
machinery, gir_jit_helpers.rs, gir_interp.rs's `GirNode` (the runtime
Apply wrapper — JIT dispatch, arg packing, DynCall slots; nothing in
it ever read a body). File RENAMES (gir_jit → jit/, gir_interp →
fused_node.rs, GirNode → FusedNode per the plan's final layout) were
deferred — pure churn, better as their own commit if wanted.

**Follow-ups parked here:** the split path passes empty lambda-site
maps (a lambda call inside a split sub-region de-fuses — pre-existing,
now visible in one place: `jit_compile_split_kernel`); known_fns
re-keys by BindId when #203 lands; `static_resolve::
collect_lambda_binds` could migrate onto `for_each_node` (one
exhaustive NodeView match instead of two).
