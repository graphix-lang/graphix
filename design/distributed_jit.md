# Distributed JIT: `emit_clif` + `fuse` per node

Status: **complete.** The GIR IR is deleted; fusion/JIT is distributed across the
node graph as two trait methods. This doc is retained for the **current-state
architecture** and the **emit contracts** every `emit_clif` impl must honor. The
staged execution history (the distributed-emit build-out, the F2 flip, the F3
delete) is summarized at the end and lives in full in `git log`.

## The architecture

Fusion/JIT is two trait methods on `Update` (and one on `Apply`), completing the
pattern `update`/`delete`/`sleep`/`refs` already follow — each node owns its case;
there is no central walker and no builtin side-trait:

```rust
trait Update<R, E> {
    /// Emit this node's computation into the open kernel; return its
    /// SSA result. Default: Err — this node doesn't fuse (correct for
    /// every async node, free of churn). CLIF is NOT store-and-combine
    /// data (cranelift has no inliner; SSA values/blocks live in one
    /// open FunctionBuilder), so the combinable unit is emit-into-the-
    /// open-function, and recursion is `child.emit_clif(cx)`.
    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr>;

    /// Fuse this subtree. `Some(replacement)` = "I fused myself:
    /// delete me and swap this in" — the parent (or the compile-time
    /// driver, for roots) calls `child.delete(ctx)` then
    /// `*child = replacement`. `None` = no replacement at this level;
    /// the impl already recursed `fuse` into its own children via
    /// &mut self and swapped any that returned Some.
    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>>;
}

trait Apply<R, E> {
    /// The builtin emission hook. Ok(None) = shape not handled →
    /// DynCall fallback; the impl MUST NOT have emitted instructions
    /// before returning None. Err = abort the kernel build → the region
    /// node-walks (partial emission is fine — the function is discarded).
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

Policy lives in each node's `fuse` — that's the point. A sync-capable node tries
`try_fuse(self, ctx)` then recurses children; an async node just recurses.
Case-specific fusion logic goes in the node implementing the case: MapQ's per-slot
template fuse is `template.fuse(ctx)` inside MapQ; callee-kernel handling lives with
CallSite / the lambda `Apply`. Maximality falls out of top-down order; a failed
compile falls through to child recursion — better granularity than a central region
planner (a non-emittable root no longer loses the whole region). The flag check
(`ctx.fusion.enabled`) is promoted into `compile()`, so it runs once rather than on
every recursion.

`emit_clif` deliberately takes no `&mut ExecCtx`: emission runs inside the jit lock
(`ctx.fusion.jit.lock()` held by try_fuse's compile phase). Everything needing
ExecCtx (callee kernel cache, capture lookup) happens in the analysis phase, before
the builder opens.

**`BodyCx`** bundles borrows of the emission triple — `pub b: &mut FunctionBuilder`
(raw CLIF escape hatch) + private `env: &mut JitEnv` + `ctx: &LowerCtx` — with a
small public method set: `helper(name)`, `mark()/truncate()`, `bind_scalar/
_tainted/bind_composite/bind_string/bind_value`, `read_elem`, `require_valid`
(may-bottom → bottom-abort), `interned_str` (lazy stable-address interning; a
prewalk mirroring emission coverage would be silent-drift dangling-pointer UB), and
`registry()` (the per-`ExecCtx` abstract registry borrow, independent of `&self` so
a reader coexists with `&mut cx`). graphix-compiler re-exports `cranelift_codegen`/
`cranelift_frontend` so packages stay in version lockstep without a direct dep.

**Scaffold library** (`fusion/scaffold.rs`): the 8 HOF loop scaffolds
(map/filter/filter_map/flat_map/find/find_map/fold/init) behind `emit_*_loop` fns
taking body closures over `BodyCx`; they serve the HOF `Apply::emit_clif` impls. The
body-result semantics that matter:
- a may-bottom **map** body → `emit_forced` runtime bottom-abort (kernel-wide
  pending);
- a may-bottom **fold/filter** body/predicate is also routed through `emit_forced`
  (runtime abort), so it fuses too — there is no build-time may-bottom de-fuse.

**`KernelSig`** (`fusion/kernel_abi.rs`): the kernel ABI contract — fn_name, the
kind-grouped param vecs + fn_params, tail_call_slots, return_type, has_tail_loop,
with `abi_params()`/`abi_param_wire_slots()`/`abi_return()`. Built once per kernel
and shared by `Arc`: the runtime dispatch node (`Kernel`) and the JIT cache key off
the same allocation — **the `Arc<KernelSig>` IS the compiled-callable handle**
(`Jit::by_kernel` keys its pointer identity). `PrimType`/`AbiKind`/`abi_kind`/
`freeze_for_abi`/the Input family/`AbiParamKind`/`AbiReturn`, and the per-`ExecCtx`
`AbstractRegistry`, all live in `kernel_abi.rs` too — the durable, body-free half of
the boundary. There is no separate kernel *body* type: "is it fusable" IS the
compile attempt, and a kernel build is pure signature derivation (`sig_from_inputs`,
the single sig builder for try_fuse regions, lambda callees, and body-split
sub-regions).

## Semantic contracts for emit work

Every `emit_clif` impl, and any future dispatch-like seam (F4's EmitTags, a possible
eager path), must preserve these. They are the distilled form of the six defects the
F2 flip surfaced — each was obvious in hindsight and invisible in advance.

1. **Three different things masquerade as "pure":** *Sync* (output on the trigger's
   cycle — the `EffectKind` contract), *effect-free* (no variable writes / logging /
   IO), and *replayable* (a pure function of currently-cached args). The DISPATCH
   protocol requires replayability: it re-delivers EVERY arg as a fresh update on
   every dispatch, so any builtin whose semantics depend on WHICH arg updated WHEN
   (`once`/`take`/`skip`/`count`/`uniq`) must be `Async` even though it is Sync by
   the letter. When auditing a builtin for fusability ask "is its update a pure
   function of its cached args?", not "does it produce on the same cycle?".

2. **Effects fuse never.** The known effect carriers in stmt position:
   `Connect`/`ConnectDeref` (variable writes), handler-ful `?` (writes the catch's
   variable — gated in `Qop::emit_clif`), `$` (logs), any CallSite to an effectful
   builtin. The SAFE failure mode is a build-time `Err` (de-fuse → the node-walk
   performs the effect); the FATAL failure mode is skipping/eliding the node (the
   effect is silently dropped). Dead-statement elimination is therefore gated on
   `stmt_subtree_effect_free` — conservative by construction (ALL CallSites count as
   effectful; we cannot consult `builtin_effects` at emit time). If you widen the
   eliminable set, the env-accounting probe (#164) and the connect fixtures are the
   tripwires.

3. **First dispatch IS init.** A `DynCallSlot`'s freshly-constructed inner Apply must
   see `event.init = true` on its first update (labeled-default Nodes are init-firing
   Constants). Any new dispatch-like seam must preserve this — the symptom of
   forgetting is a kernel that works when first fired at startup and pends forever
   when first fired by an async input.

4. **Runtime wake-ups are keyed `(BindId, top_id)`.** Anything that creates Refs at
   fusion time (feeders) must register under the REAL top expression id
   (`ExecCtx::fuse_top_id`), never an interior ExprId. The symptom: exactly one
   update delivered, then silence.

5. **Lock discipline for `Type`:** never recurse or take another lock inside a
   `with_deref` closure — clone the deref'd type out first
   (`t.with_deref(|r| r.cloned())`). TVar cells are shared RwLocks across every
   ExecCtx in the process; parking_lot's locks are fair and non-reentrant, so a
   guard held across recursion deadlocks the whole process the moment compiles run
   concurrently. (The abstract registry is no longer a hazard here: it is a per-
   `ExecCtx` lock-free `IntMap`, written during typecheck and read during fusion —
   disjoint phases of a single-threaded `compile()` — so it needs no lock at all.)

6. **Dead bottoms must not poison kernels; a missing input must not abort.**
   Canonically a value (or bottom) flows only to its consumers, but the kernel's
   composite producers abort on bottom elements (no validity channel in arrays), so
   dead statements are eliminated before emission (`emit_block_node` — classic's
   prune semantics re-homed). A kernel *input* that never fires is handled by the
   **taint channel** (#219): it feeds a taint-marked, helper-safe placeholder
   (`Value::Null` / empty `ValArray`/`ArcStr`), taint propagates through pure ops,
   and the kernel emits bottom only if the taken output path *consumes* a tainted
   value — never a whole-kernel abort. Combined with per-param STALE (fired-this-
   cycle) tracking, this is what lets a fused kernel replicate the node-walk's
   firing. `design/representable_bottom.md`.

## Execution history (complete)

The migration ran staged, each stage gated by the full differential suite + the
fuzz regression corpus with zero FuseExpect drift:

- **Distributed emit-out.** Each node kind's `emit_clif` and the HOF scaffolds were
  ported off the (then-live) GIR emitter, one construct family at a time (scalars →
  strings/values → composites → select/qop/checked-arith → the eight HOFs →
  cross-kernel lambda calls, incl. recursive self-calls).
- **The F2 flip.** The dual-path compile flag was removed; `fuse` became
  unconditional recursion and the classic region planner was deleted. The flip
  surfaced six live defects — all in the *newly-reachable* fusion surface, none in
  the ported emission, all caught differentially by the node-walk — which are the
  six emit contracts above.
- **The F3 delete.** The GIR IR (`GirExpr`/`GirOp`/`GirType`/`GirKernel` + the GIR
  interpreter, ~12.5k lines) was removed. Kernel builds became sig-only
  (`sig_from_inputs`); `GirKernel` dissolved into `Arc<KernelSig>`; the `gir_*`
  files were renamed into `fusion/` (`emit.rs`, `kernel.rs`, `scaffold.rs`,
  `emit_helpers.rs`, `intern.rs`), and `GirNode → Kernel`.

Remaining: **F4/#213 (EmitTags)** — attach per-op tags to kernels so `NodeShape` can
assert *what fused into what* (today it matches signature facts only). This is test
infrastructure; it makes no new program fuse.
