# General Value-Returning Loops

> **SUPERSEDED (2026-07-13):** This plan was not implemented. The temporary
> `sync`/`for` language was removed, and collection HOFs became compiler-owned
> Nodes. See `design/collection_intrinsics.md`.

Status: planned, not implemented.

## Summary

Add `loop`, `break`, and `continue` to the sync subset, make both `loop` and
`for` value-returning expressions, and lower them onto one generalized loop
implementation. Do not add `return`, `while`, labels, or global control
propagation in this phase.

## Language and Lowering

- Add `loop { ... }`, `break`, `break value`, and `continue`. They are legal
  only inside the nearest loop belonging to the current `sync` block.
- Restrict `break` and `continue` to block-statement and complete select-arm
  positions. Their value expressions remain ordinary Graphix expressions.
- Treat loop-body fallthrough as `continue`; bare `break` yields `null`.
- Infer a `loop` result from the union of its break values. A loop with no
  reachable break has `Bottom` type.
- Make `for` an ordinary expression. Collection exhaustion yields `null`, so
  explicit breaks of type `T` produce `[T, null]`.
- Refactor sync desugaring into source-ordered `(prefix, value)` lowering. A
  loop embedded in an operand is evaluated into a temporary, its carried
  mutable bindings are rebound, and the surrounding expression uses its
  result.
- Preserve lazy boundaries: select arms, nested sync blocks, and lambda bodies
  are lowered independently and never hoisted outward. A lambda inside a sync
  block does not inherit sync syntax; it needs its own `sync`.
- Retain the existing rule that only mutable bindings in the current sync scope
  may be assigned.

## Runtime and Fusion

- Replace the specialized `For` node with a generalized `Loop` node using an
  exhaustive driver enum:
  - `Repeat` for source `loop`.
  - `ForEach` for array/map `for`, retaining direct O(1) array indexing and
    slicing and direct map iteration.
- Functionalize assigned mutable locals as loop-carried state. Internal
  continue transfers carry state; break transfers carry final state plus the
  loop result.
- Represent pending loop transfers in `ExecCtx` with a typed `LoopId` and an
  exhaustive `Continue`/`Break` enum. Blocks and selected arms stop immediately
  when a transfer is pending; the matching loop consumes it. `Update` and
  `Apply` APIs remain unchanged.
- Recompile async per-iteration bodies under the owning `LoopId`, preserving
  correct control targets without copying compiled trees.
- Preserve the current async elaboration:
  - Sync bodies reuse one body instance.
  - Async collection loops retain per-ordinal instances and replay under taint.
  - Repeat loops advance only when the preceding iteration supplies its next
    state.
  - Loops containing early break conservatively avoid executing unreachable
    later iterations.
- Generalize the current fold-loop CLIF emitter. Maintain a loop-target stack
  in `BodyCx`; continue jumps to the header with carried values, break jumps to
  an exit block with state and result, and array `for` retains its existing
  indexed loop.
- Keep map loops as the existing node-walk fallback unless separately
  representable by the JIT. Ensure owned composite/string/value state is
  transferred or dropped exactly once on every continue, break, interrupt, and
  bottom edge.
- Rewrite `array::find`, `array::find_map`, and the corresponding list
  implementation to use value-returning `for` with early break, removing their
  explicit found-state workarounds.

## Test Plan

- Add parser, printer, serialization, resolver, property-generator, mutator,
  and diagnostic coverage for all new syntax and invalid placements.
- Add differential interpreter/JIT tests for scalar and composite results,
  bare and valued breaks, continue, nested loops, loops in expression operands,
  mutable state after exit, array/map iteration, and break-type unions.
- Add async tests proving pending iterations resume correctly and callbacks
  after a definite early break are not invoked.
- Add `#[native]` tests for repeat loops and early-breaking array loops;
  preserve every existing sync `for` fusion expectation.
- Verify infinite sync loops are interruptible in both node-walk and JIT modes
  and that the runtime remains usable afterward.
- Run compiler tests, the full Graphix test suite, fusion audit fixtures,
  release fuzz regression corpus, and HOF benchmarks before launching a fresh
  managed soak.

## Assumptions

- `return`, `while`, labeled break/continue, and a public iteration protocol are
  deferred.
- Array traversal remains allocation-free apart from values the loop body
  itself constructs.
- Correct structured-loop semantics and preservation of current fusion come
  before the later pass that recovers additional HOF fusion.
