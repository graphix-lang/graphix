# Distributed JIT: `emit_clif` + `fuse` per node

Status: built 2026-06
Pins: `stdlib/graphix-tests/src/lang/fusion.rs`, `stdlib/graphix-tests/src/lib_tests/native.rs`, `stdlib/graphix-tests/src/lib_tests/lift.rs`, `graphix-shell/tests/check_mode_parity.rs`, `graphix-fuzz` `detcheck`, `graphix-fuzz/findings/dyncall-pending-taint-jul2026/`
Supersedes: fusion_lowering_split, composite_hof_fusion, clone_rebind_testing

## The architecture

Fusion/JIT is two trait methods on `Update` and one on `Apply`,
completing the pattern `update`/`delete`/`sleep`/`refs` already follow:
each node owns its case; there is no central walker, no region planner
and no builtin side-trait.

```rust
trait Update<R, E> {
    /// Emit this node's computation into the open kernel and return
    /// its SSA result. Default: Err — this node doesn't fuse, which is
    /// correct for every async node with no churn. CLIF is not
    /// store-and-combine data (cranelift has no inliner; SSA values and
    /// blocks live in one open FunctionBuilder), so the combinable unit
    /// is emit-into-the-open-function and recursion is
    /// `child.emit_clif(cx)`.
    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr>;

    /// Fuse this subtree. `Some(replacement)`: the subtree compiled —
    /// the parent (or `compile()`, for a root) deletes the original and
    /// swaps the replacement in. `None`: no replacement at this level;
    /// the impl already recursed into its own children.
    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>>;
}

trait Apply<R, E> {
    /// The builtin emission hook. Ok(None) = shape not handled — the
    /// call site falls back to its next strategy (a fast fn, or no
    /// fusion); the impl MUST NOT have emitted before returning None.
    /// Err = abort the kernel build; the subtree node-walks (partial
    /// emission is fine — the function is discarded).
    fn emit_clif(&self, cs: &CallSite<R, E>, cx: &mut BodyCx)
        -> Result<Option<CompiledExpr>> { Ok(None) }
}

/// Mechanics only, no policy: identity suppression, region inputs →
/// KernelSig, compile under the jit lock (emit_clif recursion from
/// `node`), Kernel + feeders. Ok(None) = didn't compile.
pub fn try_fuse<R, E>(node: &Node<R, E>, ctx: &mut ExecCtx<R, E>)
    -> Result<Option<Node<R, E>>>;
```

Policy lives in each node's `fuse`. A sync-capable node tries
`try_fuse(self, ctx)` and, on `None`, recurses into its children; an
async node just recurses. Maximality falls out of top-down order, and a
failed compile falls through to child recursion — finer granularity
than a central region planner, where a non-emittable root lost the
whole region. Case-specific logic lives with the case: collection
nodes emit their loops and keep the interpreted slot graph when
emission fails; callee-kernel handling lives with `CallSite` and the
lambda `Apply`. Builtin discovery rejects known effects before input
collection and emission. It also checks for a builtin's fast-call entry
before freezing its argument types. Emission remains the authority for
all other supported shapes; admission does not duplicate those checks.
The `ctx.fusion.enabled` check runs once in `compile()`, not per recursion.

Block liveness uses a backward pass over the statements, accumulating
later reads and connect targets. A statement can be discarded only if
none of its bindings are needed later and it is effect-free. Each
statement's references are collected once per block emission.

Startup measurements and the profiling interface are described in
[`jit_startup.md`](jit_startup.md).

`emit_clif` takes no `&mut ExecCtx`: emission runs inside the jit lock.
Everything needing the context (callee kernel cache, capture lookup)
happens in the analysis phase, before the builder opens.

**`BodyCx`** (`fusion/emit/body.rs`) bundles the emission triple —
`pub b: &mut FunctionBuilder` (the raw CLIF escape hatch) plus the
private `JitEnv` and `LowerCtx` — behind a small method set: `helper`/
`call_helper`, the scope `mark`/`truncate`, the `bind_*` family, the
context words (`init_flag`, `quiet_flag`, `state_ptr`,
`claim_state_word`), and the interners (`interned_str`,
`interned_value`, `interned_type`, `interned_qop_site`). Interning is
lazy at the emit site: a prewalk mirroring emission coverage would be a
silent-drift dangling-pointer hazard. graphix-compiler re-exports
`cranelift_codegen`/`cranelift_frontend` so packages stay in version
lockstep without a direct dependency.

**The emitter is split per area** under `fusion/emit/`: `body.rs`
(BodyCx, blocks, binds), `scalar.rs`, `select.rs`, `call.rs`
(cross-kernel and fast-fn calls), `flow.rs` (statements, `?`,
dead-statement elimination), `nodes.rs`, `abi.rs` (operand forcing and
seam conversions), `scaffold.rs` (the HOF loops), `lower.rs` and
`jit.rs` (the wrapper and module). One file per concern rather than
one emitter file, so a change to select emission does not touch the
call path.

**Scaffolds** (`fusion/emit/scaffold.rs`): the HOF loop shapes
(map/filter/filter_map/flat_map/find/find_map/fold/init) as
`emit_*_loop` functions that own the mechanics — length and buffer
calls, the counter, block creation and sealing order, per-iteration
element binding and dropping — and take a body closure over `BodyCx`
that owns the policy. Elements may be scalar, composite, String or
bare Value; fold accumulators may be composite or String. A may-bottom
body or predicate is routed through `emit_or_abort_on_taint` (a runtime
bottom-abort of the HOF), so it fuses; there is no build-time
may-bottom de-fuse.

**`KernelSig`** (`fusion/kernel_abi.rs`): the ABI contract — name, the
unified param list in source order (`abi_params` groups by kind:
scalars, then array/tuple/struct pointers, then strings, then 2-word
variant/nullable/value), return type, `has_tail_loop`, and the
skipped/invariant formal positions. Built once per kernel and shared by
`Arc`: the runtime `Kernel` node and the JIT cache key off the same
allocation, so **the `Arc<KernelSig>` is the compiled-callable
handle**. `PrimType`/`AbiKind`/`abi_kind`/`freeze_for_abi` live beside
it — the durable, body-free half of the boundary. There is no kernel
*body* type; a kernel build is signature derivation
(`sig_from_inputs`, the one builder for regions, lambda callees and
body-split sub-regions). Region inputs are sorted by `BindId` so the
signature is source-order-stable across processes (`detcheck` pins
this).

**Tail loops carry any kernel param kind.** A self-tail-call rebinds
the formals and jumps to the entry block; String and Value formals
rebind through the clone/drop protocol (clone the new value before the
old is dropped, drop above the param mark at the epilogue), so a hand
List fold or a string accumulator loops natively. An invariant formal
(passed through unchanged by every self-call) keeps its slot and is
never rebound.

## Emit contracts

Every `emit_clif` impl must honor these. Each is the distilled form of
a live defect surfaced when the distributed path became the only path;
each was obvious in hindsight and invisible in advance.

1. **Same-cycle is not enough.** A builtin fuses iff its
   `Effect::Stateless` carries a `FastCall` (`strict_fusion.md`):
   `Sync` builtins whose result depends on cross-invocation state
   (`once`/`take`/`count`/`uniq`) or on WHICH args were delivered (the
   partial-delivery producers) node-walk. A kernel is a pure function
   of its inputs; a builtin inside one must be too.

2. **Effects fuse never, and are never skipped.** `Connect`/
   `ConnectDeref`, any effectful builtin, an fn-formal argument with an
   effect: the safe failure is a build-time `Err` (the node-walk
   performs the effect); the fatal one is eliding the node. Handler-ful
   `?` is the one effect that fuses, because its delivery is a pure
   function of the kernel's inputs (the raise queue,
   `strict_fusion.md`). Dead-statement elimination
   (`stmt_subtree_effect_free`, `flow.rs`) is conservative by
   construction — every CallSite counts as effectful — and a
   statement binds whatever its subtree binds. The error DIAGNOSTICS of
   `$`, handler-less `?` and unchecked arith are not effects: the
   node-walk logs when it swallows an error, a kernel produces the same
   bottom silently (`--no-fusion` shows them).

3. **First call is init.** A cross-kernel call site forces the callee's
   init view on its first call ever (the first-call words in the
   per-call-site block, `kernel_instance_state.md`). The symptom of
   forgetting it is a kernel that works when first fired at startup and
   pends forever when first fired by an async input.

4. **Runtime wake-ups key on `(BindId, top_id)`.** Feeders register
   `ref_var` under the REAL top expression id (`ExecCtx::fuse_top_id`),
   never an interior `ExprId`: the runtime wakes a top only while its
   (id, top) ref count is nonzero, and an id no installed expression
   matches strands it at zero once the spliced original unrefs. The
   symptom: exactly one update delivered, then silence.

5. **Lock discipline for `Type`.** Never recurse or take another lock
   inside a `with_deref` closure — clone the type out first. TVar cells
   are shared RwLocks across every ExecCtx in the process and
   parking_lot's locks are fair and non-reentrant; a guard held across
   recursion deadlocks the process the moment compiles run
   concurrently.

6. **Dead bottoms must not poison kernels; a missing input must not
   abort.** Composite producers abort on bottom elements (arrays have
   no validity channel), so dead statements are eliminated before
   emission (`emit_block_node`). A kernel input that never fired rides
   the taint channel (`representable_bottom.md`): a taint-marked,
   helper-safe placeholder (`Value::Null`, an empty `ValArray`/
   `ArcStr`) flows through pure ops, and the kernel emits bottom only
   if the taken output path consumes a tainted value — never a
   whole-kernel abort. Per-param STALE tracking beside it is what lets
   a kernel replicate the node-walk's firing. A BOTTOM IS A PRODUCTION:
   its STALE bit follows the same trigger fold as a value
   (`nodes::emit_bottom_placeholder` takes the governing discs), while
   the absent-delivery placeholders of an unmatched select arm are
   standing by construction.

7. **Owned arm binds drop at every arm exit.** Non-scalar pattern binds
   (payload clones, list head/tail) drop on the value-position taken
   path, guard-false, tainted-take and undetermined edges, and in the
   guard prologue (`emit_scope_drops` before each truncate); tail
   position is covered by the whole-env drop at return. The `leakcheck`
   witnesses (`select-payload-bind`, `select-list-binds`) pin it; run
   leakcheck whenever a change adds a new owned-local class.

8. **Cache keys carry resolution.** A kernel cache key includes the
   instance body's catch coverage and a resolution fingerprint — the
   same types with different callbacks are two kernels. A pass the
   fusion gate owns must never change what the typechecker sees
   (`Env::seed_typedef_refs` runs in both modes;
   `check_mode_parity` pins mode-identical `--check`).

## What was ruled out

- **A central region planner** (the classic path): it lost the whole
  region when the root did not emit, and duplicated the per-node
  knowledge the nodes already held. Top-down `fuse` with fall-through
  gives strictly finer granularity at no extra vocabulary.
- **Per-op emit tags** as a shape oracle: they would have resurrected
  the second vocabulary the GIR deletion removed — every new op taught
  twice. The shape oracle is the differential value check plus
  `#[native]`, which asserts zero node-walk residue at a source
  location (a no-op under `--no-fusion`, so it works in `run!`
  fixtures and bench programs); the decision is recorded in
  `node_shape.rs`.
- **Emitting under `&mut ExecCtx`:** emission holds the jit lock, and
  interleaving context mutation with an open builder is how a
  half-built function leaks state; the two-phase split keeps a
  discarded function free of consequences.
