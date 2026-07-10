# Declared-tvar lambdas: concrete body escapes the rigid annotation (pending ruling)

Soak jul09c fuzz findings 000000/000001 (deterministic, reproduce on
sync-subset-proto debug build too). Minimal:

```graphix
{
  let f = 'a: Number|x: 'a| -> 'a i64:0 + f64:0.;
  let a = f(i64:3);
  let b = f(f64:2.5);
  cast<f64>(a)$ + b
}
```

interp: f64:0. — jit: NO EMIT (kernel aborts).

## Mechanism

The def-time body check unifies the concrete body type (f64 — the
body ignores `x`) against the DECLARED `'a`. `'a` is then generalized
and re-instantiated per callsite from the ARGS ONLY, orphaning the
body's contribution — the same orphaned-cell family as the
"polymorphic let rec" admission that was ruled unsound on 2026-07-06.
The static result type at the i64 callsite is visibly broken:

```graphix
let a = f(i64:3);
select a { i64 as _ => "int", f64 as _ => "float" }
// => "pattern f64 will never match []" — a : [] (UNINHABITED),
//    yet the node-walk delivers F64(0.) at runtime.
```

The JIT trusts the static signature (`KERNEL BUILT f: ret=i64`), the
body delivers an F64-classed value, the return-class check aborts the
kernel, and the region never emits — a no-emit divergence. The
node-walk is dynamically typed underneath and sails through.

## Proposed ruling (the let-rec precedent extended)

Declared lambda tvars (`'a: Number |x: 'a| -> 'a`) should be RIGID
during the def-time body typecheck: `contains('a_rigid, f64)` is a
def-time error ("the body's type f64 is not `'a` for arbitrary
`'a`"), not a binding. This is standard skolemization; the annotation
is a contract. It rejects programs like the above at the DEF, which
is where the lie is.

Alternative (weaker): keep admitting the def, and have the kernel
freeze verify the def body's static type is contained in the
instantiated return before building (de-fuse on mismatch). Restores
differential agreement but leaves `a : []` — the type system still
believes an uninhabited type is inhabited.

NOT promoted to the regress corpus: the expected verdict flips from
AGREE-on-value to compile-error under ruling 1.

## Related noise (no action)

jul09c fuzz 000002 (`lp(500)` recursion per fold slot + throttle in a
dead catch arm) flaps only on the loaded campaign machine and AGREEs
deterministically unloaded — the known interrupt-grace wall-clock
class (a8a2332e).

## Residual: arith promotion hides the escape (jul10a fuzz 000002)

`'a: Number |x: 'a| -> 'a x + f64:0.` PASSES the rigid gate: the mixed
op's result is a fresh Number-constrained cell (the `ut` narrowing
defers to typecheck1, which never runs at the def), so the acceptance
check sees only an aliasable fresh cell — but at 'a=i64 the runtime
promotes to F64 and the JIT (ret=i64) no-emits. Repro parked as
rigid_arith_promotion_escape.gx (AGREEs only under --no-fusion...
i.e. interp 5.5, jit nothing).

Proposed direction: the arith op's typecheck0, when ONE operand is a
RIGID tvar and the other a known concrete type, should require
contains('a, other) as an ACCEPTANCE (rigid) check — "x + f64:0. under
-> 'a demands f64 ⊆ 'a" — the promotion-aware version of the same
contract. Needs Eric (interacts with the deferred ut design).
