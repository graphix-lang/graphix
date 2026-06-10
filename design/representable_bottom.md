# Representable value-bottom in the fused backends

## CORRECTION (Jun 2026): be the node-walk — `Option<Value>`, `None` = bottom

The first cut (below) invented `EvalResult` with a `Bottom` variant and a
separate async-pending channel. Two user corrections collapse both:

1. **Async-pending and value-bottom are the SAME thing** — "no value this
   cycle." A never-fired input, a pending DynCall, and a div-by-zero are all
   bottom. A kernel emits `None` IFF its OUTPUT consumes a bottom — never
   because some unrelated input pends. (`gir_interp.rs:3429`'s
   `self.args[i].as_ref()?` whole-kernel abort on any missing input is the
   exact bug: `select c { 0 => x, 1 => never_fired }` with `c=0` must yield
   `x`.) "Resolves later vs. never" is not a this-cycle distinction — the
   runtime re-fires on input-change (subscribe firing is an input update →
   re-fire; the arg cache at `self.args[i]` already gives `x + subscribe` a
   value once subscribe has *ever* fired), and a div0 simply never triggers
   a re-fire.

2. **Don't invent `EvalResult` — use `Option<Value>`.** `Value` is already
   the universal `#[repr(u64)]` tagged union (16 bytes, no heap); the
   node-walk's `update` already returns `Option<Value>`. The fused interp
   only runs when the JIT is off, so the unboxed-scalar micro-opt isn't
   worth a parallel type + its `into_*` conversions.

**The corrected interp model (`gir_interp.rs`):** `eval_expr -> Option<Value>`,
`None` = bottom. The env is one `Vec<(name, Option<Value>)>` (a binding may
be `None`). `?` IS the absorb (a bottom operand short-circuits the op to
`None`) — the `ev!` macro and `EvalResult::Bottom` are deleted. Unchecked
div/mod map their div0 to `None` (extract scalar, `checked_div`, `None` on
failure); `?` on `Value::Error` → `None`. `error(v)` → `Some(Value::Error)`
(a value, not bottom). Select/IfChain return the taken arm's `Option<Value>`;
a `None` guard → arm doesn't match → fall through. The boundary IS the
identity: `update` returns the kernel's `Option<Value>` (`None` = emit
nothing). A missing kernel input feeds `None`; a DynCall returning `None`
flows as `None`. `DYNCALL_PENDING` / `BodyResult::Pending` are **deleted** —
there is no separate pending channel; the runtime's input-change re-fire is
the only re-run mechanism.

The JIT half (per-value validity / sentinel, below) is unchanged in spirit:
the JIT's analogue of `Option<Value>` is "value + validity," and a missing
input / pending DynCall / div0 all set `valid=false`; the kernel returns
`None` iff the OUTPUT is invalid. The "async vs bottom" separation in the
original JIT plan is likewise dropped.

### STATUS (Jun 2026): interp phases 1–2 LANDED

The `Option<Value>` rewrite of `gir_interp.rs` is done and validated:
- `EvalResult` (+ its `Bottom` variant, `into_*` conversions, the `ev!`
  macro, `wrap_value_shape`/`normalize_to_nullable`/`extract_composite_or_scalar`),
  `BodyResult::Pending`, and the interp's use of `DYNCALL_PENDING` are all
  **deleted**. `eval_expr -> Option<Value>`, `None` = bottom, `?` = absorb,
  the env is one `Vec<(ArcStr, Option<Value>)>`, `eval_kernel_full` takes a
  single `&[Option<Value>]` and returns `Option<Value>`. (`RegValue` stays —
  the JIT still uses it; only the interp eval stopped.)
- **The line-3429 async-unification fix is in**: `GirNode::update`'s interp
  path feeds a missing input as `None` (bottom) instead of `?`-aborting the
  whole kernel — `select c { 0 => x, 1 => never_fired }` with `c=0` yields
  `x`. A DynCall returning `None` flows as bottom. There is no separate
  pending channel on the interp path.
- **Bottom-scrutinee poisoning** (#178): a `select` whose *scrutinee* is
  bottom poisons the whole select (the node-walk's Select doesn't fire
  without a scrutinee value), whereas a bottom *guard* falls through. The
  scrutinee was folded into the arm conds, so the fix added an explicit
  `scrut` field to `GirOp::IfChain` + `GirStmt::Select`, evaluated once
  up front; bottom → `None`. (Found by the differential oracle, NOT the
  suite — the suite has no `select <bottom> {…}` fixture.)
- Validated: the {div0,`?`,missing-input,bottom-scrutinee} × {output,
  dead-let,un-taken-arm,taken-arm,guard,scrutinee} truth table all agree
  (fused-interp == node-walk); **~4000 fuzz programs, zero `interp != fused`
  divergences**; 132 compiler + 2033 graphix-tests green.

**Still to do (the JIT half, phases 3–5):** the JIT still aborts on bottom
via the `DYNCALL_PENDING` flag, so `interp == fused != jit` for any program
whose JIT'd kernel produces an *eager* bottom (bottom guard, bottom in an
un-taken arm, bottom scrutinee). The JIT validity codegen (Scalar2 / NULL /
BOTTOM_DISC, div→sentinel, IfChain phi, guard→band, boundary decode,
Call-tainted bail — below) is the next milestone. Then phase 5: demote
sink/dead-elim to pure optimizations and add the now-all-modes-green
differential fixtures.

---

## Original design (superseded by the correction above for the representation choice; the JIT validity mechanics still apply)


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
