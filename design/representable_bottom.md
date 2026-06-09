# Representable value-bottom in the fused backends

## Problem

In the node-walk, every binding/statement is its own node and "bottom" is
`None` returned from that node's `update` — **local**. When fusion collapses
N nodes into one kernel, bottom is signalled by the global thread-local
`DYNCALL_PENDING` flag, which aborts the **whole** kernel. A bottom in an
intermediate the output never consumes (an un-taken select arm, a dead let,
a let-bound select used downstream — "finding 5", an arm guard) wrongly
aborts. The select-arm *sinking* + dead-statement-elimination passes are a
structural band-aid with a long edge-case tail.

**Fix:** make a *value-level* bottom (integer div/mod-by-zero, signed
MIN/-1, `?` (QopUnwrap) on an error value) a **representable value**, exactly
like `None`-from-`update`. It flows through data dependencies; a select
propagates only the taken arm's value (dropping an un-taken arm's bottom);
the kernel returns `None` **iff its OUTPUT value is bottom**. Sinking +
dead-elim stay as *optimizations* that elide a provably-dead bottom (so it's
never even represented), no longer load-bearing for correctness.

**Async-pending stays a whole-kernel abort.** A DynCall whose callee
produced no value this cycle genuinely means "no value possible, re-run next
cycle." `DYNCALL_PENDING` is reserved for *that* meaning only (rename →
`ASYNC_PENDING`). The four value-bottom sites stop touching it.

## Interp representation (Phase 1–2)

- `EvalResult::Bottom` — payload-free absorbing value (like node-walk `None`).
- Producers: `eval_bin` Div/Mod `checked_*`→None and `QopUnwrap` error branch
  emit `EvalResult::Bottom` instead of `set(DYNCALL_PENDING)+None`.
- Env gains a 7th slot list `bottoms: Vec<ArcStr>`; `push_local` routes
  `Bottom`; `GirOp::Local` of a bottom name returns `Bottom`; `GirOp::Block`
  scope-exit truncates it. A let may bind a bottom; `GirStmt::Let`/`Discard`
  **no longer short-circuit** the body on a bottom value.
- **Absorb:** a pure op (Bin/Cmp/BoolBin/Not/Cast/ValueArith/ValueEq/Concat/
  producers/accessors) with a `Bottom` operand yields `Bottom`. `BoolBin`
  short-circuit is preserved (`false && ⊥ = false`; only *evaluated*
  operands can taint).
- **Select/IfChain** already evaluate only the taken arm → an un-taken arm's
  bottom is never computed. The taken arm's `Bottom` flows out (absorb). A
  **guard** that evaluates to `Bottom` → arm **does not match** → fall
  through (closes the guard gap; byte-for-byte the node-walk `is::match`).
- **Boundary:** `eval_kernel_full`'s `Return(v)` arm uses a new
  `into_value_checked() -> Option<Value>` (`Bottom => None`). The plain
  `into_value` stays infallible (`Bottom => unreachable!`). `None` from
  `update` now means *exactly* async-pending OR output-is-bottom.

## DynCall / Call argument bottom = async-pending (review fix #2)

A value-bottom **as an argument** to a DynCall/Call must NOT absorb. The
node-walk's bottom arg-edge doesn't fire but the call still dispatches; the
faithful fused approximation is "no fresh value this cycle" = async-pending.
So in the DynCall/Call arg loop, a `Bottom` arg → `return None` (abort),
mirroring the existing arg-not-ready None. This keeps the two None-reasons
disjoint by construction: pure ops absorb, an impure consumer fed a bottom
arg pends.

## JIT representation (Phase 3–4)

- **Scalars:** `CompiledExpr::Scalar2 { value, valid: i8 }` for *tainted*
  scalars only; non-tainted stay `Single` (zero cost — the taint
  optimization). `scalar_with_validity()` treats `Single` as always-valid.
- **Composites / strings:** NULL pointer = bottom (real ValArray/ArcStr are
  non-null). Drop helpers gate on `ptr != 0`.
- **Value-shape `(disc,payload)`:** a reserved `BOTTOM_DISC` **above the
  netidx disc space** (review fix #3 — the space is packed; `0x4000` is
  `Bool`). Use e.g. `0x1_0000_0000` with a compile-time assert no `Value`
  variant uses it, and a `disc == BOTTOM_DISC` check at **every** decode site
  BEFORE the `transmute` (a bad disc transmute is UB). Drop helpers gate on
  `disc != BOTTOM_DISC`.
- **Div guard → sentinel:** `bad = is_zero | (MIN&&-1)`; `safe_r =
  select(bad,1,r)` so the hardware op never traps; result `Scalar2 { value:
  sdiv(l,safe_r), valid: !bad }`. No block/flag/jump. `QopUnwrap` similarly
  (`valid = !is_err`). The async-DynCall `pending_exit` jump STAYS.
- **IfChain merge** threads validity: a scalar phi gains an `i8` valid param
  iff a taken arm is tainted; value-shape/composite thread in-band. Widen
  `compile_ifchain`'s `is_value_shape` to the full `GirType::is_value_shape()`
  set (review fix #3c). A bottom guard branches on `band(value, valid)` (an
  invalid guard → false → next arm).
- **Boundary:** the wrapper's `if pending` check fires for async only; the
  decode tests the OUTPUT's marker (`valid==0` / null / `BOTTOM_DISC`) → None.
- **Cross-kernel `GirOp::Call` (review fix #3b):** the callee's scalar param
  ABI is one word — no validity slot. V1: **bail Call fusion when any scalar
  arg or the scalar return is tainted** (`compile_call_clif_args` returns Err
  on a `Scalar2`). Whole-program taint to widen the ABI is a follow-up.

## Phasing (interp-first, validate each)

1. **Interp substrate** + DynCall-arg-pending. Validate: a {div0,`?`}×{output,
   dead-let, un-taken-arm, taken-arm, guard} truth-table asserting fused-interp
   == node-walk; full suite (sink stays on so the differential fixtures stay
   green — new finding-5/guard fixtures wait for the JIT); oracle on the div/`?`
   corpus + `fuzz/crashes`.
2. **Interp async separation** — rename `DYNCALL_PENDING`→`ASYNC_PENDING`;
   grep-audit the four value-bottom sites no longer touch it.
3. **JIT scalar validity** (Scalar2, taint pass, div→sentinel, IfChain phi,
   guard→band, boundary decode, Call-tainted bail).
4. **JIT composite/value/string validity** (NULL/BOTTOM_DISC in-band, guarded
   decode, drop guards, remove value-bottom `pending_exit`).
5. **Demote sink/dead-elim to pure optimizations** (suite green with sinking
   DISABLED proves it's no longer correctness-critical, then re-enable for
   perf); close #177 (value-shape overflow error flows as bottom); add the
   finding-5/guard/`async_call(x/0)` differential fixtures (now all-modes
   green); fuzz campaigns + a Source-E hunt on the bottom-flow boundary.

## Inherited limitation (not a regression)

`QopUnwrap → Bottom` reproduces the node-walk only when **no catch handler**
is in scope (the common bare `?`). A `?` inside a `try/catch` whose recovered
value feeds the kernel output would need a side-channel to write the catch
BindId before signalling bottom (mirroring `Qop::update`'s `ctx.set_var`).
That cross-`?`/catch routing doesn't fuse end-to-end today; gate it at fusion
time if it ever does.
