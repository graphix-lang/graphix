# Type-copy discipline and single instantiation

Status: current principle
Pins: `graphix-fuzz/findings/instance-copy-skew-jul2026/`, `graphix-fuzz/findings/bound-cell-cycle-accepts-aug2026/`

Eric's principle: **be extremely skeptical of every deep copy of a
type. A copy made while any type checking is still pending forks live
inference state — facts recorded after the fork land on one side only,
and no later check can see both.** Every copy site must justify itself
against a quiescence boundary: the source's inference is finished, its
facts are frozen (bindings plus cell conjuncts), and the copy is a
fresh instantiation that will never be confused with its sibling
copies. `type_operation_scaling.md` and `env_independent_typerefs.md`
are its applications.

## Why

The instance-elaboration acceptance family (null through an i64 slot,
cross-type compares, mixed-numeric accumulators — ~90% of one soak's
findings by volume, one of them an address leak) all reduced to forked
type state: a call site held SEVERAL instantiation copies of the
callee's ftype, the source's `elem := null` fact landed in one copy's
cells and the predicate's `cmp := i64` fact settled another, and no
cell ever held both. Strict enforcement in any position cannot catch a
contradiction that never materializes in one place.

## The fork vectors

### Frozen×frozen unification merges cells

`contains`' TVar×TVar arm with both cells unbound and both frozen used
to answer `Ok(true)` WITHOUT linking the cells. `frozen` means "this
var already joined a name-alias group" (`TVar::alias` self-marks and
refuses re-entry) — it was never a statement about unification. Two
settled groups whose types are equated must share one cell from that
moment on, or they fork. The arm now takes `Act::CellMerge` →
`TVar::alias_cells` (`typ/contains.rs`, `typ/tvar.rs`): a cell merge
that bypasses the frozen gate while keeping `alias`'s occurs checks,
committed under `AliasTVars` like every other aliasing act. This is
what makes the site's facts collide in one cell so the strict
argument-boundary check fires, and it makes the null witnesses reject
in both modes, consistent with their direct forms.

### `reset_tvars` — the legal deep copies

| site | why it is legal |
|---|---|
| `node/callsite.rs` site instantiation | the def→site boundary: the def gate has closed, body facts live as cell conjuncts (`constrain_known`), bindings are solved facts. This is THE instantiation. |
| `node/genn.rs` builtin generic ftype | the same boundary |
| `typ/fntyp.rs` settle witness snapshot | copies the constraint store's type; the store is finished by construction, and the copy is what keeps live inference from writing into the store |
| `replace_auto_constrained` | display only |

A new `reset_tvars` call must name its quiescence boundary the same
way.

### Binding copies

Copying a BINDING is copying a fact — safe iff the fact is final and
the receiving cell's conjuncts admit it. `TVar::copy` at a bind site
runs after the containment check has verified the incoming binding;
the instantiation path (`reset_tvars_int` writing a fresh cell's
binding) runs no such check. Latent and unwitnessed since the cell
merge above; the hardening, if a witness appears, is validating copied
bindings against the fresh cell's seeded conjuncts at the
instantiating call site.

## The enforcement stack

1. **`('a, 'a)` comparisons** probe both directions unbound, commit the
   widening one, and reject disjoint operands — killing cross-type
   compares under discriminant order and the emitter's mixed-primitive
   hole.
2. **Strict argument-boundary instance checks**: each argument's type
   must be contained by the instance's resolved formal. `UnresolvableRef`
   (name resolution under a dead (env, scope) context) is the one
   structured artifact class still swallowed.
3. **The cell merge** makes the facts collide so (2) actually fires.

Arithmetic is homogeneous (`fn('a: Number, 'a) -> 'a`): `i64 + f64`
is a type error, not a `[i64, f64]` union that the runtime silently
promotes — the union was a static lie in the conservative direction and
the direct cause of the mixed-accumulator witnesses surviving the copy
chain.

## The end state

One instance per (site, callee), owned by the `CallSite`, read by
init, analysis, freeze and fusion alike; an instantiation snapshots its
def's `LambdaIds` (`LambdaIds::instantiate`) so def-body facts carry
while a site's inflows land on the site's copy. The cell merge de-fangs
skew when copies MEET; copies that are never unified against each
other still drift, which is why there must be exactly one.
