# The final fusion/JIT architecture: `Expr → node graph → CLIF`

Status: **implemented and current.** This is the architecture the compiler runs
today. Part 1 (eliminate GIR — the pipeline is `Expr → node graph → CLIF`,
distributed as `Update::emit_clif` + `Update::fuse` per node) landed as the F3
GIR-IR deletion; the node graph is the single IR. Part 2 (a fast, eager
node-walk) remains a **future** idea — it is described below and clearly marked
as not-yet-built.

## The thesis

The compiled pipeline is exactly **`Expr → node graph → CLIF`**, with:

1. **No GIR** (done). The GIR (`GirKernel`/`GirOp`/`GirExpr`/`GirStmt`) was a
   *same-altitude re-encoding* of the node-graph subtree, not a genuine
   lowering. It has been eliminated; the JIT walks the node-graph subtree
   directly (via `NodeView`) and emits CLIF, doing the fusion transformations
   inline — distributed as `Update::emit_clif` + `Update::fuse` per node.
2. **A fast, eager node-walk** (future). The node-walk *would* evaluate *pure*
   subtrees eagerly in-cycle (and pure tail calls as a loop), removing the
   one-cycle-per-call overhead that was the GIR interpreter's *entire* reason
   to be faster. This would make the node-walk both the canonical model and a
   fast interpreter, with the JIT as a native accelerator on the hottest pure
   subtrees. Not yet built.

The winding path that got us here (a GIR interpreter, a GIR IR) was worth
building — it's how we learned the fusion design, the purity boundary, the
HOF-callback lowering, the value/bottom semantics — but **the final product
carries none of that scaffolding.** A reviewer who had never seen the code does
not have to ask "why is there a second copy of the node graph?"

---

## Part 1 — Eliminate GIR (done)

This landed as the F3 GIR-IR deletion. The record below is why GIR was baggage
and how its work was relocated; it is kept as rationale, not as a plan.

### Why GIR was baggage

GIR fed two consumers: the GIR interpreter (a flat evaluable form) and the JIT.
Once the interpreter was gone, GIR fed only the JIT — and it sat at the *same
abstraction level* as the node graph:

- Its ops mirrored the node ops one-for-one (`Bin`, `Cmp`, `Select`/`IfChain`,
  `TupleNew`, `ArrayGet`, …). It was not lower-level.
- The genuinely-lower IR already existed *downstream*: **CLIF is SSA.** The real
  lowering step is `node graph → CLIF`. GIR-in-the-middle was a third
  representation at node-graph altitude.

The classic "separate lowering from codegen via an IR" principle only justifies
an IR that is *below* the source. GIR wasn't, so the principle didn't apply —
it was path dependence: GIR was the interpreter's input form, and the JIT was
built to consume it.

### The honest nuance: GIR wasn't *pure* re-encoding

GIR also did real transformation work that had to live somewhere:

- **Normalization** — const-folded map/array/tuple literals to one
  `Const(Value)`, collapsed the three option representations
  (`[T,null]` Set / collapsed `T|null` prim / `[T,Error]` Set) into one
  `Nullable` shape, etc.
- **HOF lowering** — inlined a callback lambda's body into a single loop op
  (`ArrayMap`/`ArrayFold`/…) instead of a `CallSite` + lambda.
- **The kernel boundary** — identified the fused region's inputs/captures (the
  fusion analysis result).

Eliminating GIR meant relocating this work, not deleting it — which is what
happened:

- Normalization → handled inline in the CLIF walk (more arms, un-normalized
  forms) and via the `abi_kind`/`freeze_for_abi` type classifiers.
- HOF lowering → the CLIF walk inlines the callback when it hits an
  `array::map` CallSite to a pure static lambda; the emit path does this via
  the `fusion::emit::scaffold` loop scaffolds, driven straight from the node
  graph — no GIR.
- The kernel boundary → the fusion analysis identifies the node-graph subtree +
  its free variables (the `KernelSig` / input list); no GIR materialization.

### How it was done

- The emitter (now `fusion/emit.rs`, née `gir_jit.rs`) stopped being
  `compile_expr(GirExpr)` matching `GirOp` and became per-node emission:
  `Update::emit_clif` / `Apply::emit_clif` walk the node graph via `NodeView`,
  doing normalization + HOF inlining during the walk.
- `fusion/lowering.rs` shrank from "emit a GIR" to "identify the sync subtree +
  its free-var inputs" (the analysis), feeding the node subtree + input list to
  the emit walk.
- `GirKernel`/`GirOp`/`GirExpr`/`GirStmt` were deleted. The `abi_kind`/`Type`
  classification (already on `Type`, not GIR) stayed — it's the JIT's
  type-shape oracle and is orthogonal.
- The spliced fused node + JIT dispatch + arg-packing (now `Kernel` in
  `fusion/kernel.rs`, née `GirNode` in `gir_interp.rs`) stayed in spirit, but
  its "kernel" became the compiled CLIF + the input feeders, with arg-packing
  driven by the analysis's input list (an `Arc<KernelSig>`), not a `GirKernel`.

### Steelman for keeping GIR (and why it loses)

- *"NodeView is indirection; a flat enum is a cleaner match target."* — True but
  minor; `NodeView` is already the downcast and the JIT match is the same shape.
- *"GIR's normalization simplifies the JIT."* — Real, but the normalization is a
  bounded transformation that moves into the walk or a node-graph pass; it
  doesn't need a whole parallel IR + its construction + its maintenance.
- *"The node graph is stateful (Cached); the JIT wants static shape."* — The
  JIT walks the static spec/type via `NodeView` and ignores runtime state; it
  compiles the template once (shared CLIF), not per-slot.

None of these is a *fundamental* reason — they're effort and habit.

---

## Part 2 — A fast, eager node-walk (future — not yet built)

> This part is a **future** design idea; it captures the real reason the GIR
> interp was faster. The eager node-walk described here does not exist yet — the
> node-walk today is purely reactive. What *is* already true (and current) is
> the JIT behavior called out below: an infinite pure tail recursion hangs the
> JIT. The rest of this section is the plan for closing the node-walk's
> per-cycle overhead.

### The limitation

The reactive node-walk schedules **one cycle — one trip through the IO/event
loop — per function call**, including a *pure* tail call. An iterative/recursive
pure computation (mandelbrot's inner loop, a `let rec` fold) becomes N cycles =
N IO-loop trips. That per-cycle overhead, not the dispatch, is why the GIR
interpreter (which evaluated the whole pure computation in a single in-cycle
kernel) was faster.

### The key realization

The GIR interpreter **conflated two things**:

1. **Eager in-cycle evaluation of pure computation** — the genuine win.
2. **Centralized `match` dispatch** — a *loss* vs the node-walk's distributed
   vtable dispatch (megamorphic indirect branch with poor prediction; see the
   CLAUDE.md GIR-interp-deletion entry).

The better design **separates them**: keep the node-walk's fast distributed
dispatch, and *add* eager in-cycle evaluation for pure subtrees. That
**dominates** the GIR interpreter on both axes — faster eval (eager) *and*
faster dispatch (vtable) — while remaining the single canonical model. This is
the final proof that deleting the GIR interpreter was correct: its one virtue
is recoverable in the node-walk, better.

### The enabler is already built

The purity/effect analysis fusion needed (`program_effect_map`,
`EffectKind::Sync`/`Async`, the unstable-Ref / async-edge boundary) is exactly
the "is this pure?" oracle. Surface it as:

- **`Node::pure(&self) -> bool`** (trait method). Transitive: pure iff the node
  and all its children have no unstable Ref read and no async edge — the same
  sync boundary fusion draws. Every builtin reports its purity (a `const PURE`
  or via `EffectKind::Sync`).

### The optimizations it unlocks

- **Eager pure tail calls in `CallSite`.** On a tail call to a *pure static*
  callee (statically resolved + `pure()`), evaluate the callee's body
  **eagerly, in-cycle**, instead of scheduling a reactive cycle. This removes
  the per-call IO-loop trip — the N-cycle mandelbrot becomes ~1 cycle.
- **Tail calls as a loop, not recursion.** Implement the eager tail call as a
  `loop` in `CallSite` (rebind args, re-enter the body) rather than native
  recursion — so a big interpreter loop doesn't stack-overflow. (The fusion
  `TailCall` concept already proves the rebind-and-loop pattern; bring it to the
  node-walk.)
- **Pure args handled optimally everywhere.** Any node/builtin with a pure arg
  can evaluate it eagerly in-cycle rather than waiting for a reactive update.

### Why it's safe — with exactly one intended exception

This is a **pure optimization** in all but one case: a pure call's eager result
equals its reactive result, just computed sooner (a pure function is
deterministic in its args). The node-walk stays the canonical model; eager eval
is an evaluation-order optimization within it.

**The one case where observable semantics change — intentionally, and
correctly: a non-terminating PURE tail recursion** (`let rec f = |n| f(n)`, no
async). Today the reactive node-walk advances it one step per cycle, so it never
settles but every *other* node keeps firing — the program runs on with `f`
perpetually pending. Under eager evaluation (and in the JIT *already*), the
in-cycle loop never returns → **hang**. This change is correct, not merely
forced:

1. **Forced for the JIT, and consistency requires the interp to match.** Native
   code can't preempt itself to yield to the scheduler mid-loop without yield
   points that defeat the tight loop — so the JIT *must* hang. If the node-walk
   and JIT disagree here, the differential model breaks.
2. **The honest result of eager evaluation.** A pure function's value *is* its
   result; a non-terminating pure computation has value ⊥. Eager eval asks
   "what's the value now" → ⊥ → hang. The reactive "advance one step, never
   settle, keep running" behavior silently launders non-termination into a
   permanently-pending node and masks the bug; the hang surfaces it.
3. **The purity boundary is the right place to draw the line.** An *async/impure*
   infinite recursion (timer/poll-driven) genuinely *should* advance per-cycle —
   it's event-driven; that's reactivity. A *pure* infinite recursion has no
   external driver; per-cycle scheduling was only ever an artifact of running
   pure computation reactively. So: pure-infinite → hang (eager); impure-infinite
   → per-cycle continue (reactive). Drawing the eager/reactive split at purity is
   what makes both correct.

**Oracle caveat (Part 1, before the eager interp exists):** the node-walk is
still reactive, so an infinite-pure-tail-recursion program whose *result is
independent* of that loop shows `interp = value` (continues) vs `jit = Timeout`
(hangs) in the 2-mode oracle. This is NOT a bug — it's exactly this accepted gap,
which Part 2 closes by making the interp hang too. Don't chase it as a JIT bug.

### Subtleties / open questions

- The node-walk's `update` is *reactive* (fire-on-input). Eager pure evaluation
  is a second mode (compute-now-given-current-inputs). Adding that mode for
  pure subtrees is the real implementation work — it's essentially a
  tree-walking eval of the pure subtree, in-cycle (which is what the GIR interp
  did, but with vtable dispatch and no separate IR).
- The eager/reactive boundary is `Node::pure`: a pure subtree with an impure
  leaf (a Ref to a reactive value, an async op) is reactive; eager eval stops at
  that boundary and the impure part schedules normally. Same boundary as fusion.
- Interaction with collection Nodes: eager eval should compose with their
  explicit callback slots, while direct Array CLIF loops remain the native fast
  path. This is potentially a *large* win for interpreted HOFs over reactive
  collections.

---

## Part 3 — The end-state, and why it's clean

```
Expr ──parse/typecheck──▶ node graph ──┬─(node-walk: reactive, canonical;
                                        │   *future*: eager for pure subtrees,
                                        │   loop tail-calls)
                                        │
                                        └─(JIT: hottest pure subtrees,
                                            walk node graph via NodeView ─▶ CLIF)
```

- **One IR people see: the node graph.** It's the canonical model (and, once
  Part 2 lands, also a fast interpreter).
- **CLIF is the low-level IR** for the JIT'd hot paths. The lowering is
  `node graph → CLIF`, directly.
- **No GIR.** No second interpreter. No same-altitude re-encoding.
- A fresh reviewer sees `Expr → node graph → CLIF` and asks no "why does this
  exist twice?" question.

The JIT earns its keep: native code beats even an eager tree-walk for the
hottest pure kernels (mandelbrot inner loop). Once the eager node-walk (Part 2)
lands, the node-walk baseline is fast for *all* pure computation and the JIT
becomes a targeted accelerator rather than a correctness-or-speed necessity.
Correctness is structural today regardless (node-walk is canonical;
JIT-can't → node-walk).

---

## Staging

The GIR-deletion half (steps 4–5 below) is **done** — it landed as the F3
GIR-IR deletion (`compile_node`/`emit_clif` over `NodeView`, HOF inlining
inline, normalization relocated, `GirKernel`/`GirOp`/`GirExpr`/`GirStmt`
removed, `fusion/lowering.rs` reduced to subtree+input identification),
validated via the 2-mode oracle (node-walk vs JIT) — unchanged contract, GIR
just gone.

The eager-node-walk half (steps 1–3) is **future**. Rough order, each
independently validatable, node-walk stays canonical & green throughout:

1. **`Node::pure(&self) -> bool`** + builtin purity, derived from the existing
   effect analysis. Pure addition; assert it agrees with the fusion sync
   boundary on the corpus.
2. **Eager pure tail calls + loop in `CallSite`.** Validate: mandelbrot / a
   `let rec` fold produces the same value, in dramatically fewer cycles
   (measure cycles via the runtime); no stack overflow on a big loop. Differential
   vs the current reactive node-walk (same values).
3. **Eager pure-arg handling** in nodes/builtins (incremental, op by op).

The fuzzer (2-mode interp-vs-jit) is the safety net for step 2 (and was for the
now-landed GIR deletion) — both are "same values, different mechanism."
