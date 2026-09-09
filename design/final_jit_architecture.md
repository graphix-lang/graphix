# The fusion/JIT architecture: `Expr → node graph → CLIF`

Status: current principle
Pins: `stdlib/graphix-tests/src/lang/fusion.rs`, `stdlib/graphix-tests/src/lib_tests/native.rs`, `graphix-shell/tests/check_mode_parity.rs`, `graphix-fuzz/findings/` (the `regress` gate)

## The pipeline

The compiled pipeline is exactly `Expr → node graph → CLIF`. There is
one intermediate representation people see — the node graph — and two
evaluators of it:

- the **node-walk** (`node/*.rs`, the `Update` graph): the canonical
  execution model and the universal fallback; it must always be
  correct;
- **fusion → cranelift JIT** (`fusion/`): compiles pure sync subtrees
  to native kernels. Success splices the kernel in and deletes the
  originals; failure leaves the originals to node-walk.

```
Expr ──parse/typecheck──▶ node graph ──┬─ node-walk (reactive, canonical)
                                        └─ JIT: walk the subtree via
                                           NodeView, emit CLIF directly
```

CLIF is the low-level IR for the JIT'd paths, and the only lowering
step is `node graph → CLIF`, distributed across the nodes: each node's
`Update::emit_clif` emits its own CLIF (`Apply::emit_clif` for
builtins, the `fusion/emit/scaffold.rs` loops for HOFs), and
`Update::fuse` decides per subtree whether to try
(`distributed_jit.md`). A fusion bug can lose fusion, never produce a
wrong answer, and the differential fuzzer (`graphix_fuzz.md`)
enforces bit-for-bit agreement between the two evaluators.

## Why there is no separate typed IR

An earlier design put a third representation — the GIR
(`GirKernel`/`GirOp`/`GirExpr`/`GirStmt`) — between the node graph and
CLIF, with its own interpreter. Both were deleted, and the rule stands:
**no parallel typed IR**.

The reasoning:

- GIR sat at the *same abstraction level* as the node graph. Its ops
  mirrored the node ops one-for-one (`Bin`, `Cmp`, `Select`,
  `TupleNew`, `ArrayGet`, …); it was a re-encoding, not a lowering.
  The genuinely lower IR already existed downstream — CLIF is SSA. The
  "separate lowering from codegen via an IR" principle justifies only
  an IR that is *below* the source, so it did not apply; GIR was path
  dependence from having been the interpreter's input form.
- A second vocabulary taxes every change twice. Each semantics fix had
  to be written for the node-walk, for GIR construction, and for the
  GIR consumer; every new node shape had to be taught to the second
  vocabulary before it could fuse. The same reasoning retired per-op
  emit tags (`node_shape.rs`) unbuilt.
- The GIR interpreter's one real virtue — evaluating a whole pure
  computation inside one cycle instead of one reactive cycle per call
  — did not need a second IR. The node-walk now evaluates a pure tail
  loop in-cycle (`node/lambda.rs`, the tail driver) and evaluation is
  atomic within a cycle on both engines; the centralized `match`
  dispatch GIR added on top of that was a loss against the node-walk's
  distributed vtable dispatch.

The work GIR did was relocated, not dropped: literal normalization and
the option-shape collapse happen inline in the CLIF walk and in the
`abi_kind`/`freeze_for_abi` type classifiers (`fusion/kernel_abi.rs`);
HOF lowering is the scaffold loops driven straight from the node
graph; the kernel boundary is the region's free-variable input list
(`KernelSig`), materialized by the compile attempt itself
(`sig_from_inputs`) rather than by a separate analysis that could drift
from the emitter.

Steelmen for keeping a middle IR, and why they lose: a flat enum is a
cleaner match target than `NodeView` (true, minor — `NodeView` is
already the downcast); GIR's normalization simplified the JIT (real,
but a bounded transformation that fits in the walk, not a whole IR with
its construction and maintenance); the node graph is stateful and the
JIT wants static shape (the JIT walks the static spec and types via
`NodeView` and compiles the template once, ignoring runtime state).
None is fundamental; all are effort and habit.

## Non-termination is the same on both engines

A pure infinite recursion (`let rec f = |n| f(n)`) spins inside one
cycle on both evaluators. This is the honest result of in-cycle
evaluation: a pure function's value *is* its result, and a
non-terminating pure computation has no value, so neither engine
returns from the cycle. The old reactive behavior — advance one step
per cycle while every other node keeps firing — laundered
non-termination into a permanently pending node and masked the bug.
An *async* infinite recursion (timer- or poll-driven) still advances
per cycle, because it is event-driven; drawing the in-cycle/reactive
split at purity is what makes both correct. Containment is the
cooperative interrupt (`GXHandle::interrupt`, polled by the interp's
tail driver and every emitted loop head) and the stack budget, never a
depth limit.
