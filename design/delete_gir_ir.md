# Deleting the GIR IR — `Expr → node graph → CLIF`

Status: **SUPERSEDED by `distributed_jit.md`.** The GIR removal is being
executed, but around a *distributed* architecture (`Update::emit_clif` +
`Update::jit` trait methods, the delete/sleep/refs pattern) instead of the
central `compile_node` walker this document plans. The scoping analysis,
KernelSig design, risk list, and oracle caveat below remain valid and are
folded into the successor.

Original goal: the JIT walks the node-graph subtree
directly (via `NodeView`) and emits CLIF; `GirKernel`/`GirOp`/`GirExpr`/
`GirStmt` deleted. (Part 1 of `design/final_jit_architecture.md`; the eager
node-walk is Part 2, deferred.) The GIR interpreter is already gone (#188);
the JIT is the sole GIR consumer.

## Verdict (from the scoping map)

Clean merge of two passes — `fusion/lowering.rs::emit_node` (node → GIR) and
`gir_jit.rs::compile_expr` (GIR → CLIF) — into one `compile_node(&Node) ->
CompiledExpr` matching `NodeView`. **No fundamental blocker.** ~90% of the
NodeView↔GirOp↔CLIF correspondence is direct relay: every CLIF emission site
already keys off `e.typ` (netidx `Type`) + `gir::abi_kind`, NOT off GirOp
structure, so the GirOp is a pass-through label. The taint/pending/ownership
machinery (`Scalar2`, `ensure_owned_*`, `compile_bin/cmp/cast/concat/ifchain`,
`compile_element_read`) is reusable verbatim. `AbiKind`/`abi_kind`/
`freeze_concrete`/`PrimType`/`AbiParamKind`/`AbiReturn` STAY (Type-based).

**Buildable alongside the GIR path, gated, switched incrementally — NOT
big-bang.** The 2-mode oracle (node-walk interp vs JIT) validates each slice.

## The two real risks (front-load these)

### A. The kernel ABI descriptor → `KernelSig`
`GirKernel` is two things: (1) the op-list body — collapses into the Node;
(2) the ABI contract — must survive. `abi_params()` (the kind-grouped wire
layout: scalars, array/tuple/struct pointers, string, then 2-word variant/
nullable/value), `abi_param_wire_slots()`, `abi_return()`, `has_tail_loop` +
`tail_call_slots`, `is_identity_passthrough()`. These are derived from the 8
per-kind param vectors and are load-bearing for THREE separately-compiled call
sites: `compile_into_function` (callee entry-bind), `define_wrapper` (adapter
unpack), `GirNode::update` (runtime arg-pack + return decode). Plan: a lighter
`KernelSig { params (8 kind vecs in ABI order), return_type, tail }` produced by
refactoring `populate_kernel_inputs` to emit `KernelSig` WITHOUT a body list;
the runtime node holds `(body_node, KernelSig, wrapped_jit)`. `collect_region_inputs`
(free-var identification) + `type_to_region_input_kind` survive unchanged (pure
Node+Type analysis). The cross-kernel callee closure (`compile_kernel_with_callees`
compiles parent + transitively-reachable callees in dependency order, keyed by
`(LambdaId, FnType)`) re-roots on a cacheable "compiled-callable" handle instead
of `Arc<GirKernel>`.

### B. The select sink/dead-elim passes — DECISION: drop in V1
`stabilize_scrutinee`, `prune_dead_stmts`/`prune_dead_lets`, `sink_into_select_*`
are GIR→GIR list rewrites run by `emit_do`. With no `GirStmt` list, there's no
intermediate to prune/sink. **Decision: drop them in V1.** Rationale: CLAUDE.md
("Sinking + dead-elim are now pure OPTIMIZATIONS, not correctness") + the map
both conclude the JIT's per-value taint/validity (a bottom is *representable*
and flows harmlessly to the boundary, not *poisoning*) makes them unnecessary
for correctness; and a select arm is already lazily compiled (per-arm `brif`
block), so an un-taken arm's bottom is never computed. The scrutinee
stabilization (bind a non-idempotent scrut to a temp once) is the one piece
that may still be needed for correctness on a non-`Local` scrutinee — keep it
as a tiny inline temp-bind in `compile_node`'s select arm, not a list pass.
**Validate by the oracle**: the sink/scrutinee fixtures (`sink_*`,
`fused_select_scrutinee_evaluated_once`) must stay green through the new path;
if any regresses, re-add the minimal transform inline. Re-add full
sink/dead-elim later as a CLIF peephole only if a perf case demands.

## HOF inlining (the safest part)
The callback body is already a live `Node`: `cs.resolved_apply() → Lambda(g) →
g.body()` (exactly what `MapQ::emit_gir` reads today). The array-HOF arm of
`compile_node` emits the same CLIF loop scaffold, binds `elem_local` in the
`JitEnv`, and calls `compile_node(g.body(), env, ctx)` for the loop body. The
`MapFn`/`FoldFn::emit_gir` trait (package-owns-its-HOF-codegen — the compiler
doesn't know builtin names) SURVIVES, repurposed from "return a `GirExpr`" to
"emit CLIF into a `FunctionBuilder` given the body Node + loop context." A phase
shift (runs at CLIF-emit time, not region-build time), mechanical. The impure-HOF
per-slot `clone_rebind(template)` runtime path is unaffected (operates on Nodes +
`WrappedKernel`/`FusedKernel`, not GirOps).

## Other consumers
- `node_shape.rs` (`GirMatcher`/`GirOpTag`/`gir_op_tag`/`visit_ops`) — the test
  shape harness, built on GirOp. Must be RE-EXPRESSED on Node shape (it already
  walks `FusedKernel → feeders` for `Contains`). ~6 ported fixtures + scattered
  `; shape:` clauses. Bounded test churn.
- Bottom/pending (`pending_exit`, `DYNCALL_PENDING`, `QopUnwrap` taint,
  `Scalar2`) — survives verbatim (CLIF+Type level).
- `FusedKernel` (runtime carrier, feeders + inner node) + `clone_shared` —
  survive.

## Staging (each checkpoint oracle-gated; node-walk untouched = divergences loud)

**Smallest end-to-end first slice:** a scalar-only, single-kernel, no-callee,
no-tail region — `let x = subscribe(...); x + 1` (one scalar region-input param,
one `Bin` body). Exercises `collect_region_inputs` → `KernelSig` →
`compile_into_function` entry-bind → scalar `compile_node` → wrapper →
`GirNode::update` pack → `abi_return` decode. One fixture, both modes agree.

1. **Add `compile_node` (scalar arms only)** alongside `compile_expr`, gated by
   a new `CFlag::DirectNodeJit` (off by default). Calls the SAME CLIF helpers
   `compile_scalar_impl` calls. Build stays green (GIR path untouched). Oracle
   the scalar slice.
2. **`KernelSig` from Node + region inputs** — refactor `populate_kernel_inputs`
   to emit `KernelSig` (no body list); the node holds `(body_node, KernelSig,
   wrapped)`. Riskiest checkpoint (the ABI boundary) — keep GIR `GirNode` in
   parallel, switch one fixture at a time.
3. **Extend `compile_node` shape-by-shape**: value-shape (Variant/Nullable/
   Const-value/MapRef/slice/bytes-index), composite producers/accessors, String,
   Block/Select (with the §B decision), `?`/pending. Each shape = one oracle
   fixture-class.
4. **HOF arms** — repurpose `MapFn::emit_gir` to emit CLIF; recurse `compile_node`
   into the callback body. One HOF at a time.
5. **Cross-kernel Call + tail-loop** — re-root the callee closure on the new
   handle. Last (most entangled).
6. **Flip default; delete** `GirOp/GirExpr/GirStmt/GirKernel` + `compile_expr`/
   `compile_scalar_impl`/`compile_value_expr` + `emit_node`/`emit_body`/`emit_do`/
   all `emit_*` + the GirOp half of `node_shape`. Rework the shape harness.

The differential oracle + `graphix-fuzz` campaigns gate every checkpoint.

**Oracle caveat (don't chase this as a JIT bug):** an infinite PURE
tail-recursion whose result is independent of the loop shows `interp = value`
(the reactive node-walk advances it per-cycle and keeps running other code) vs
`jit = Timeout` (the tail-loop kernel hangs). This is an *accepted, correct*
divergence — native code can't yield mid-loop, so the JIT must hang; the
node-walk's per-cycle "continue" is the artifact. It's closed by Part 2 (eager
node-walk hangs too). See `final_jit_architecture.md` Part 2 "Why it's safe —
with exactly one intended exception." If a fuzz finding is an
infinite-pure-tail-recursion `interp=value / jit=Timeout`, it's this, not a bug.
