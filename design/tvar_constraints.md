# TVar cell constraints

Status: PROPOSED (2026-07-02, Eric + Claude). Not yet built. Slotted after
the fuzzer-v2 phase-2.0 JIT bottom/firing fixes unless the unannotated-gate
fusion story moves it up.

## The problem

A type variable cell today is `TVarInnerInner { id, frozen, typ:
Option<Type> }` — a name and an optional binding, nothing else. The only
constraint machinery in the language lives on `FnType.constraints:
Vec<(TVar, Type)>` (the explicit `'a: Number |x: 'a| -> 'a` form), and it
is checked post-hoc at call sites (`CallSite::typecheck0`, after argument
unification: "whatever `'a` ended up bound to must be contained by the
constraint"). The constraint is a property of the *function signature*,
not of the *variable*.

So a node that learns something partial about a type — arith knowing its
operands are numbers, `never()` knowing nothing at all — has exactly two
tools: bind the cell NOW, or leave it unconstrained. Arith picks the
former (`node/op.rs` `arith_op!` typecheck0: "init types that aren't known
by now to Number" — `contains(Number, operand)` binds an unbound operand
cell to the wide Number PRIMITIVE SET). That choice is load-bearing: the
arith result-type computation (the `ut` table) bails "type must be known"
on an unbound operand, so typecheck0 force-feeds it.

The wide binding is where three real problems come from, all found or
sharpened by the fuzzer-v2 work:

1. **Bare lambdas are "monomorphic wide"** (fuzz/triage-fuzzer-v2/
   typecheck_observations.md #3). `let f = |a| a + a` infers
   `fn(a: Number) -> Number` with the wide set BAKED into the cells; the
   call site's `reset_tvars` (callsite.rs — per-site freshening that
   already works) has no tvars left to freshen. Every call passes
   positionally (`Number ⊇ f64`), but the result is Number-wide, so
   `let x: f64 = f(f64:1.0)` rejects — infectiously, through any op that
   touches the result. The explicit `'a: Number` form works precisely
   because its return IS the param cell.
2. **The `never()` wide-Number pollution de-fuses unannotated gates**
   (the `gated_scalar_unannotated` ASPIRE fixture). `never()`'s tvar
   flows into arith, gets bound to the wide set, and the wide set
   genuinely denotes multiple register classes — no freeze can soundly
   pick one, so the whole region silently node-walks. Annotating one
   `let` fixes it, which is exactly the kind of unpredictable
   performance cliff graphix exists to avoid.
3. **Bad error locality.** The wide set is legal at the site that bakes
   it and only collides with reality later, so the reject fires
   somewhere downstream of the actual mistake, printing a wide-set
   mismatch instead of the constraint that was violated.

## The design

Give the CELL a constraint:

```rust
pub struct TVarInnerInner {
    pub(crate) id: TVarId,
    pub(crate) frozen: bool,
    pub(crate) typ: Arc<RwLock<Option<Type>>>,
    /// Everything this variable is ever bound to must be contained by
    /// this type. None = unconstrained.
    pub(crate) constraint: Option<Type>,
}
```

with ONE enforcement point: the bind sites in the contains/unify walk.

- **Binding** an unbound cell to `t` requires
  `constraint.check_contains(t)`; violation is a type error AT THAT SITE
  ("'a: Number cannot be bound to string").
- **Aliasing** two unbound cells intersects their constraints; an empty
  intersection is a type error.
- **Arith** (and any future partially-informed node) ADDS the Number
  constraint to an unbound operand cell instead of binding it wide. The
  information "the user did arithmetic on it, so it's a number" is
  preserved exactly, without foreclosing which number.
- **Same-cell result aliasing**: for `a + a` (both operands the same
  cell), same-type arithmetic is type-preserving, so the result type IS
  the operand cell. `|a| a + a` then infers literally
  `fn(a: 'a) -> 'a, 'a: Number` — identical to the explicit form — and
  `reset_tvars` does the per-site instantiation it already knows how to
  do (constraints copy to the fresh cells).
- **The explicit `'a: Number` syntax becomes sugar** that seeds the cell
  constraint at lambda compile time. `FnType.constraints` stays for
  printing/interface files (and the callsite post-check can remain as a
  belt-and-suspenders assert), but the semantics move to the cells.

### Deferred arith result typing (the typecheck1 piece)

With operands constrained-but-unbound, arith's `ut` table can't run at
typecheck0 — and must no longer bail. The node instead defers: its result
is the operand cell (same-cell case) or a fresh Number-constrained cell
(distinct-operand case), and **typecheck1 re-runs the `ut` table** once
bindings have settled, erroring "type must be known" only if the operands
are still undetermined then. typecheck1 already exists as the settle-up
pass; this is a natural residence. This is the sound, cheap kernel of
"re-do inference later" — NOT per-call-site re-inference of lambda bodies
(true polymorphic instantiation), which would duplicate typechecking work
per site and is explicitly out of scope.

### The two-distinct-operands question (`|a, b| a + b`)

A subset constraint cannot express the relation `result = arith_join(a,
b)`. Options considered:

- **(i) Unify the operand cells.** Simple, makes two-param bare lambdas
  fully polymorphic — but a semantic TIGHTENING: today `f(i64:1,
  f64:2.0)` passes (both params wide); unified cells would reject
  mixed-type calls.
- **(ii) Fresh constrained result cell + typecheck1 consistency check.**
  The result is a fresh `'r: Number` that use sites may narrow (e.g. an
  annotation binds `'r := f64`); typecheck1's deferred `ut` run then
  verifies the narrowed result contains the join of what the operands
  became. Keeps today's mixed-operand acceptance; the result narrows
  when the program pins it.
- **(iii) Keep the result wide for the multi-cell case only.** Smallest
  change; two-param bare lambdas keep the current annotation-reject
  behavior.

RECOMMENDATION: (ii). It is strictly more expressive than today, changes
no currently-accepted program, and reuses the deferred-`ut` machinery the
same-cell case already needs. (i) can be revisited later as a deliberate
language decision if mixed-type arith turns out to be rare in practice.

## What gets better

- `let f = |a| a + a; let x: f64 = f(f64:1.0)` compiles (observation #3).
- The `never()` gate idiom fuses without annotation: the gate's cell
  stays narrowable, the freeze sees one register class, and
  `gated_scalar_unannotated` flips from ASPIRE/None to Jit — a direct
  predictable-fusion win. The `freeze_for_abi_normalized` third rung
  (resolve_tvars for the never-Set pollution) may also simplify.
- Constraint violations report at the violating site with the
  constraint named.

## Ripple points (implementation checklist)

- `typ/contains.rs`: every site that binds an unbound cell gains the
  constraint check; cell-aliasing sites gain constraint intersection.
  This is the heart and the main review surface.
- `typ/tvar.rs` + the copying walks: `reset_tvars`, `alias_tvars`,
  `resolve_tvars`, `copy`/`normalize` must carry constraints to fresh
  cells. `would_cycle` should consider constraint types.
- `node/op.rs` `arith_op!` (and `Neg`, checked ops): constrain instead
  of bind; defer `ut` to typecheck1. Audit other `contains(Number, …)`
  "init to" sites (comparisons init operands similarly? verify).
- Printer: `'a: Number` display already exists for FnType constraints;
  cells print their constraint when unbound.
- Pack/serialization: verify whether TVars cross the Pack boundary
  (packed-AST pattern type predicates) and whether constraints must
  serialize.
- `frozen` interaction: freezing a constrained-unbound cell — decide
  whether freeze binds to the constraint (current wide behavior as the
  terminal fallback) or refuses.
- Fusion: `freeze_for_abi` treats a constrained-unbound cell as
  unfreezable (as today for unbound); the win comes from cells being
  BOUND narrow more often, not from freezing constraints.

## Testing

- Fixtures: the three bare-lambda probes from observation #3 (single
  site annotated, two-site unannotated, infectious-op variant), a
  constraint-violation error-message fixture, mixed-operand `a + b`
  acceptance (pins decision (ii)), and flipping
  `gated_scalar_unannotated` to `FuseExpect::Jit`.
- The fuzzer is the regression net: gen-check's compile rate (bare
  lambdas are generated organically), the differential campaign for
  value semantics, and the `run!` corpus + FUSE audit for fusion drift.
