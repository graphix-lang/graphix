# Typechecker observations from fuzzer-v2 gen-check (2026-07-02)

Not differential divergences — both modes reject identically, so the
oracle says AGREE; only the generator's compile-rate instrument
(`gen-check`) surfaced them. Each needs an Eric ruling: intended
semantics, accepted wart, or bug.

## 1. Guarded arm before bind-all final breaks exhaustiveness (BUG?)

```graphix
{ let v: [`A(i64), `B] = `A(i64:1); select v { `A(x) if true => i64:1, y => i64:0 } }
```

Rejects "missing match cases: type mismatch [`A(i64), `A(unbound)] does
not contain [`A(i64), `B]" — but the final `y` bind-all is plainly
exhaustive, and guards are documented as never subtracting from
coverage. Guard-first arms over variant scrutinees are idiomatic
(exactly the shape `select k.code { k@`Up if cond => ..., _ => ... }`
from the TUI examples). Same failure with a guarded `B arm, where the
computed coverage comes out `A(i64) — the guarded arm seems to poison
the arm-union rather than contribute nothing.
Generator impact: ~0.2-1% of programs; not worked around (left in so
the reject rate reminds us).

## 2. Annotated binding of a bare-poly call result rejects (intended?)

```graphix
{ let f = |a| (a + a); let x: f64 = f(f64:1.0); x }   // rejects
{ let f = |a| (a + a); let x = f(f64:1.0); x }        // OK
{ let f = |a| (a+a); let x = f(i64:2); let y = f(f64:1.0); y }  // OK!
```

A bare lambda's params share one WIDENING tvar; `f64 does not contain
'_N: Number` — and the wideness is infectious (`let z: f64 = (x +
f64:1.0)` also rejects). Two unannotated sites at different types work.
The explicit `'a: Number |x: 'a| -> 'a` form freshens per site and
annotates fine. Consistent with the widening-tvar model, but the
asymmetry (an annotation that merely RESTATES the argument type is an
error) will surprise users. Generator works around it (bare-lambda
results never enter annotated contexts).

## 3. Select arm-union doesn't collapse tvar-equal composites

```graphix
{ let v0 = select i64:100 {
    42 => { b: f64:1.0, y: cast<i64>(u8:2)$ },
    _  => { b: f64:0.0, y: i64:42 } };
  v0.y }
```

Rejects "expected struct not [{b: f64, y: i64}, {b: f64, y: '_N:
i64}]" — the two arms differ only by an unresolved `$`-result tvar, the
union keeps both members, and the field access then fails. Same family
as the never()-tvar Set pollution the fusion freeze fix (45e60e3e)
handled — here it's the typechecker's arm-union, pre-fusion.
Annotating the let works around it. ~0.5% of generated programs.
