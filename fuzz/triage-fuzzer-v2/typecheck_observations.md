# Typechecker observations from fuzzer-v2 gen-check (2026-07-02)

Not differential divergences — both modes reject identically, so the
oracle says AGREE; only the generator's compile-rate instrument
(`gen-check`) surfaced them. Status after Eric's ruling (all three are
bugs) and the fix session later the same day:

## 1. FIXED — guarded arm before bind-all final broke exhaustiveness

```graphix
{ let v: [`A(i64), `B] = `A(i64:1); select v { `A(x) if true => i64:1, y => i64:0 } }
```

Rejected "missing match cases". Root cause: the coverage unions fed the
bind-all's INFERRED type predicate — a fresh TVar — into
`check_contains`, a GREEDY unifying walk, which bound it to the first
scrutinee union member and reported the rest missing. Fix
(node/select.rs typecheck0): an unguarded wildcard arm (irrefutable
structure + inferred predicate) makes the select exhaustive BY
CONSTRUCTION — its raw tvar stays out of the coverage unions and the
coverage checks are skipped; a side-effect-only walk of the informative
arm-type union against the scrutinee preserves the narrowing that used
to ride the coverage check (`|n, acc| select n { 0 => .., _ => .. }`
still learns n: i64 — load-bearing for fusion). Bonus principled fix:
bind-all arm types now narrow BY POSITION (aliased against the
scrutinee minus earlier unguarded irrefutable arms' coverage — the same
diff the dead-arm walk computes), so
`select opt { null as _ => "", s => s }` gives s: string on purpose
rather than by greedy accident. Fixtures: lang/select.rs
guarded_arm_then_bindall, guarded_other_tag_then_bindall,
bindall_narrows_by_position.

## 2. FIXED — arm-union never collapsed tvar-equal composites

```graphix
{ let v0 = select i64:100 {
    42 => { b: f64:1.0, y: cast<i64>(u8:2)$ },
    _  => { b: f64:0.0, y: i64:42 } };
  v0.y }
```

Rejected "expected struct not [{..}, {..}]": the arm union was built
while the `$`-result tvar was unbound, and a Set never re-collapses on
its own once the tvar binds. Fix (node/mod.rs deref_typ!): before
bailing on a Set, normalize it — flatten_set's merge sees through bound
TVars; if the Set collapses, keep dereferencing the merged type. Fixes
every accessor (field/tuple/variant/array) in one place. Fixture:
lang/select.rs arm_union_tvar_collapse.

## 3. OPEN (root-caused, needs a design ruling) — bare-poly annotated results

```graphix
{ let f = |a| (a + a); let x: f64 = f(f64:1.0); x }   // rejects
{ let f = |a| (a + a); let x = f(f64:1.0); x }        // OK
{ let f = |a| (a+a); let x = f(i64:2); let y = f(f64:1.0); y }  // OK
```

Mechanism (node/op.rs arith_op! typecheck0): "init types that aren't
known by now to Number" — arith on an unannotated param BINDS the
param's tvar to the wide Number PRIMITIVE SET at def time. The def's
FnType then has no tvars left for the call site's `reset_tvars` to
freshen (callsite.rs:929 already freshens per site — that machinery is
fine, which is why the explicit `'a: Number |x: 'a| -> 'a` form works:
its return IS the param cell). So a bare lambda is "monomorphic wide":
every call site checks fine positionally (Number ⊇ f64) but the RESULT
is Number-wide, and any annotation narrower than Number rejects —
infectiously, through ops.

Design fork, Eric's call:
(a) keep 'a as a Number-CONSTRAINED tvar at def time instead of binding
    it to the primitive set, and make arith result cells alias their
    operand cell when both operands share one tvar (`a + a`) — bare
    single-param lambdas then behave like the explicit form; `a + b`
    (two cells) still needs a rule (unify a==b? forces same-type
    operands);
(b) full per-site re-inference of arith results (deferred checks) —
    heavy;
(c) accept as documented semantics: bare lambdas' derived results are
    Number-wide; annotate params or use the explicit 'a-form (docs +
    better error message).

## Observation 4 (soak 2026-07-03): explicit 'a-form body unsoundness

`{let f = 'a: Number|x: 'a| -> 'a f64:0.; {let a = f(i64:3); let b = f(f64:2.5); cast<f64>(a)$ + b}}`

`--check` ACCEPTS this, and the interp runs it: `a` is statically `i64`
but carries `F64(0.0)` at runtime. The def-time body check unifies the
body's concrete `f64` INTO the 'a cell; the per-callsite `reset_tvars`
freshen then loses that lower bound, so the `i64:3` site instantiates
'a=i64 without ever re-holding the body's `f64:0.` against it. The
static/dynamic mismatch is invisible to the node-walk (Values are
dynamically tagged — it just flows the f64) but the JIT derives kernel
slot types from the STATIC types and panics at marshal
(`pack_value_to_u64: I64(1) isn't a F64 scalar`, emit.rs:1090, killing
the runtime — corpus-fuzz/divergence_000003).

This is the dual of Observation 3: there a def-time binding was too
sticky (return cell bound to the primitive set); here the freshen isn't
sticky ENOUGH (a body-derived binding must survive as a lower-bound
constraint on every instantiation). Both want the same machinery:
tvar CELLS carrying accumulated constraints through reset/freshen —
design/tvar_constraints.md (task #20). Until then any program in this
class that fuses can kill the runtime at marshal time.
