# Must-reject mutation

Status: PROPOSED, not built (2026-09-25). An exception to this folder's
as-built rule until it is built; then this document is rewritten as
built, or folded into `graphix_fuzz.md` §8.
Pins: none yet.

## The gap

`typemorph` (`graphix_fuzz.md` §8) tests the ACCEPTANCE plane: a program
the checker accepts, transformed so it still must be accepted. The
other direction, a program the checker accepts but must not, has no
lane of its own. Today it is caught only when the lie runs and goes
wrong in a type-shaped way in the runtime lanes, and a lie in an arm
that never runs, or whose values happen to fit, is never caught.

Must-reject mutation fills it: take a program the checker accepts,
apply ONE mutation that must be rejected by construction, and require
the rejection. An accepted mutant is the finding.

The mutations of interest are not shallow ones (a fresh field name, a
dropped argument): those test name resolution and arity, which rarely
break. They are mutations that only unification, instantiation,
rigidity and coverage can catch, built without annotations, using what
the base's check already knows: the type of every expression.

## Principle: a rigid consumer

Inference re-infers around a change, so "must reject" needs a reason
that no inference can repair. Every mutation below names a RIGID
CONSUMER: a use whose required type is fixed independently of the
mutated value, by a rule the language states (CLAUDE.md, "The
semantics both engines implement" and "Language features"). The
mutation puts a value the consumer cannot take in front of it. The
certainty argument is always the pair (the consumer's rule, the type
the base's check gave the value), and a candidate whose argument does
not hold is skipped, never guessed.

Stacking comes from the base, not from the mutation: bases are
typemorph's subjects and its ACCEPTED probes, which are already far
from anything written by hand, and each base yields one mutant per
chosen site. A mutant is never mutated again.

## The type map

The base is checked once with a type map: every expression's type,
resolved (`resolve_tvars`, so later unification cannot move it) and
keyed by its preorder index in `mutate::preorder`, the index space
typemorph's sites already use. It is a new `Ide` sink, filled by one
walk over the checked root nodes after typecheck and only when a check
asks for it; a check without it pays nothing. The binds and references
`Ide` already records give the variable-level view (a binding's type,
its use sites).

A candidate uses the map to pick its site and its disjoint type. A
type is DISJOINT from `T` when it is a primitive `T` does not contain
and that contains nothing of `T`; a site whose `T` holds `Any`, an open
cell or an abstract type is skipped (nothing is disjoint from `Any`,
and an open cell has not decided).

## Families

Each family: the site, the mutation, the rule that must refuse it, and
what disqualifies a site.

**1. Monomorphic reuse (instantiation).** Site: `let f = |..| ..` whose
uses the map shows instantiated at two disjoint types. Mutation: wrap
the lambda so the binding is not generalized, `let f = { let z = ..;
|..| .. }`. Rule: only a `let` of a lambda, or a `let` forwarding a
generalized binding, generalizes; a non-generalized binding holds one
instance, so the two uses conflict. Skip: `f` also reached through a
reference (`&f`) or a callback that may re-instantiate it.

**2. Rigid variables.** Site: a lambda with declared variables (`'a`,
`'b`) whose parameters `x: 'a`, `y: 'b` are in scope in the body.
Mutation: add a statement that equates two of them (`x == y`) or binds
one to a concrete type (`x == 1`). Rule: a def's declared variables are
rigid in its body check: none binds to a concrete type and no two
unify. Skip: a variable with a constraint the concrete type satisfies
only through the constraint (`'a: Number` against `1` is still refused,
but keep the first cut to unconstrained variables).

**3. Call conflicts.** Site: a call argument. (a) The callee's resolved
parameter type is concrete: substitute a literal of a disjoint type.
(b) The parameter is a variable shared with another argument whose
type is concrete: substitute a literal whose type neither contains nor
is contained by that argument's. Rule: containment for (a); for (b) the
widest-argument rule refuses when no argument's type contains all the
others, and a variable a callback or reference holds keeps the first
argument's type, so both paths refuse (`callsite.rs::Widening`). Skip:
labeled defaults, variadic positions, a parameter typed `Any`.

**4. Widening into a rigid consumer.** Site: a use of a value `v: T`
whose parent is one of:
- an arithmetic operand (exactly one numeric type, each operand
  containing the other; `[i64, string] + 1` is refused);
- a select scrutinee with no catch-all arm (a bind or `_` at the top,
  or a type-test arm that `U` fits), the coverage rule;
- a struct field read (a union with a non-struct member is refused);
- a call argument with a concrete parameter type;
- the value of a writer to a binding typed by its initializer or an
  annotation.

Mutation: replace the use with `select c { true => v, false => u }`,
`u` of a disjoint type `U`, `c` a fresh `bool` that is never a
constant. Rule: the consumer's. Skip: any of the parent forms above
that can absorb `U`.

**5. Variant widening (the common case of 4).** Site: a value whose
type is a set of variants `` [`A, `B(..)] `` consumed by a select that
covers exactly those tags. Mutations:
- a new tag into the flow: `` select c { true => v, false => `Fresh } ``,
  or a new arm `` .. => `Fresh `` added to the select or function that
  PRODUCES the set, so the set grows where it is built;
- a payload retyped: `` `B(e) `` → `` `B(u) `` at one construction site,
  `u` disjoint from the payload type the consumer's pattern binds and
  uses rigidly.

Rule: select exhaustiveness is enforced; an or-arm narrows only the
alternatives it names. Skip: a consumer with a catch-all, a tag-only
arm over a payload-free set that the new tag would match, a consumer
reached only through `Any`.

**6. Retyping at a distance.** Site: an unannotated `let v = e` with a
non-⊥ initializer and a use pinned by a literal (`v + 1`, `v == "s"`).
Mutations: the initializer replaced with a value of a disjoint type, or
a writer `v <- u` added. Rule: an unannotated `let` takes its
initializer's type, and a later writer must fit it
(`node/mod.rs::write_mismatch`); the pinned use then conflicts.

## Hops

Families 4, 5 and 6 follow the value from the mutation to the consumer.
A hop is allowed only through a form that passes the type unchanged: a
`let` without annotation (its binding's uses), parentheses, a block's
last expression, a select arm's value. The type map must show each hop
with the same type as the value; any other form (a polymorphic call, a
select that builds a union, a callback, a reference) ends the chain and
the site is skipped. Distance is capped (a few hops): the certainty of
the argument falls with each one, and false findings cost triage time
(sep25a: 42 of 49 typemorph flips were transform assumptions, not
checker bugs).

## Verdicts

Every mutant is checked like a typemorph probe (`typemorph-one`: a
warm runtime, `--check` semantics, a fresh process to confirm).

- **Rejected at the right site: pass.** The refusal's `ErrorSite` (the
  innermost expression an error is attached to) lies inside the mutated
  node or the rigid consumer. The mutated node's span is found in the
  printed mutant by its preorder index, like every typemorph site.
- **Rejected elsewhere: a finding of its own class.** Either the
  mutation broke something the argument did not account for, or the
  checker attaches the error to the wrong place; both need a look, and
  neither counts as a pass.
- **Accepted: the finding.** Adjudicated by running the mutant through
  the differential engines: a run that goes wrong in a type-shaped way
  (a runtime type error, a JIT ABI mismatch) confirms unsoundness; a
  clean run most likely means the certainty argument was wrong, and the
  catalog is corrected.

A finding is written once per class (family + consumer kind +
normalized head), as `typeleak_N.gx` for an accepted mutant and
`typemisplaced_N.gx` for a rejection elsewhere, the mutant under a
comment header that names the base and the site, so it reproduces as it
stands.

## Integration

The families run inside the typemorph source: a subject yields its
must-accept probes as now and its must-reject probes on the same base;
an accepted must-accept probe is itself a base for must-reject probes.
The cap per family per subject is typemorph's (`TM_CAP`). No new soak
source: the typemorph share covers it, re-weighed from findings per
CPU-second once it runs.

## Order of work

1. The type map sink, and the right-site check.
2. Families 1 (instantiation) and 5 (variant widening): the two areas
   with the most history (reference instantiation, `poly_binds`,
   coverage).
3. Families 4 and 6 (the hop rule is shared with 5).
4. Families 2 and 3.

Each family lands with a unit test in the style of typemorph's
(`extract_skips_param_type_reads`): sites it must take, sites it must
skip, and the text of the mutant.

## Risks

- **Rules move.** A family's argument cites a rule; when the rule
  changes (the widening rule changed this week), the family changes in
  the same commit, or its findings are false.
- **Coverage of the argument.** A family is only as good as its skip
  list. The first runs will mostly find skip-list holes; they are fixed
  in the catalog, the same way typemorph's preconditions were.
