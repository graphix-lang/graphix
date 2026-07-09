# Multi-mut sync block: arm-tuple union vs TupleRef (pending ruling)

## Repro (compile error today)

```graphix
sync {
  let mut res = [];          // unannotated empty init
  let mut s = "";
  for v in [1, 2, 3, 4] {
    select v % 2 { 0 => { res = array::push(res, v); s = "[s]." }, _ => null }
  };
  (array::len(res), str::len(s))
}
```

```
expected tuple not [('_a: Array<'e: i64>, string), (Array<'_u: unbound>, string)]
```

## Mechanism

The multi-mut desugar rewrites each select arm to yield the assigned
set as a tuple, then destructures with `let res = __acc.0; let s =
__acc.1` (TupleRef accessors — single binds + TupleRef are in the
fused vocabulary; a destructuring `let (..)` is not). The select's
rtype is the UNION of the arm tuple types (post-typecheck arm unions,
finding 37). The assign arm's tuple carries the push result
(`Array<i64>` once `v` binds the elem); the no-assign arm's tuple
carries the acc PARAM components, whose instantiated element tvar is
still UNBOUND when the union forms. Unbound cells never conflate
(union_identical, soak items 11/18 — sound), so the union survives,
and `deref_typ!`'s normalize escape can't collapse it → TupleRef
refuses.

## What was already fixed (2026-07-09, on sync-subset-proto)

`union_identical` now derefs a BOUND tvar against a bare type (the
(TVar, TVar) arm already deref'd two bound cells; the mixed arm fell
to `_ => false`). With that, the ANNOTATED form (`let mut res:
Array<i64> = []`) collapses, compiles, and fully fuses. Unbound cells
still never conflate.

## The residual question for Eric

For the unannotated form, options:

1. **TupleRef projects through a union of same-arity tuples**:
   `[(A, B), (C, B)].0 : [A, C]` — structurally sound (every member
   HAS the field), mirrors how patterns match through unions. StructRef
   has the same question. Risk: the projected union then flows onward;
   emission shape-classification of such unions is untested.
2. **Unify (not just collapse) select arm tuples when the arms are
   containment-compatible** — over-narrows legitimate heterogeneous
   selects; probably wrong.
3. **Accept the annotation requirement** and give a better error
   (`let mut` of an empty composite in a multi-mut loop must be
   annotated).

Option 1 seems most principled; option 3 is the cheap stopgap (the
error today at least points at the desugared `.0`).
