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
  class as #203 — likely fixed together in Stage E; a pin probe
  documents it. Next: per-HOF `emit_clif` (flat_map → filter_map →
  find → find_map → array::init), preserving may-bottom map body ⇒
  runtime bottom-abort vs may-bottom fold/filter/find/flat_map
  bodies ⇒ build-time de-fuse. (#202 turned out NOT to be
  Apply::emit_clif work: `cast<T>(x)` is a TypeCast NODE whose
  emit_clif relay exists — the gap is `emit_cast_node` refusing the
  union `[T, Error]` result shape; an independent graphix-compiler
  item, after D2.) Then the owned-array-arg widening
  (pending-cleanup registration; flips the owned-slice probes) and
  D3 destructured `|(k,v)|` leaves.
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
