# Must-reject mutation

Status: BUILT (2026-09-25): the type map (`Ide::expr_types`,
`GXHandle::check_with_types`), the right-site check, every family
(`graphix-fuzz/src/mustreject.rs`, run by every typemorph subject), the
labeled/optional generation and its typemorph transforms. Each family
says what of it is built; what it lists as open is not.
Pins: `graphix-fuzz` `must_reject_families_are_refused_where_their_rules_say`;
`graphix-tests` `lib_tests::expr_types`.

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

A seventh family, and the generation it needs, covers labeled and
optional arguments; that generation also feeds the regular soak.

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

**1. Monomorphic reuse (instantiation).** Site: `let f = |..| ..` passed
as a VALUE (a reference that is not a call's callee) at two uses whose
instances the map shows taking different primitive first parameters.
Mutation: wrap the lambda so the binding is not generalized, `let f = {
let tm__0 = null; |..| .. }`. Rule: only a `let` of a lambda, or a
`let` forwarding a generalized binding, generalizes, and a value
reference to a binding that is not generalized holds the binding's own
cells, so the two uses meet in one instance. A CALL instantiates its
callee whatever the binding (`CallSite::typecheck0`), so calls are not
sites: `f(1); f(1.5)` over the wrapped `f` is accepted, rightly. Right
site: the definition or either use's statement.

**2. Rigid variables.** Site: a lambda with declared variables (`'a`,
`'b`) whose parameters `x: 'a`, `y: 'b` are in scope in the body.
Mutation: add a statement that equates two of them (`x == y`) or binds
one to a concrete type (`x == 1`). Rule: a def's declared variables are
rigid in its body check: none binds to a concrete type and no two
unify. Skip: a variable with a constraint the concrete type satisfies
only through the constraint (`'a: Number` against `1` is still refused,
but keep the first cut to unconstrained variables). Built: a first
statement comparing a parameter `x: 'a` with a literal, or calling a
parameter `f: fn(x: 'a) -> ..` with one (`'a` not `f`'s own
quantifier: each call picks that anew), right site the definition. Open: equating two declared variables.

**3. Call conflicts.** Site: a call argument. (a) The callee's resolved
parameter type is concrete: substitute a literal of a disjoint type.
(b) The parameter is a variable shared with another argument whose
type is concrete: substitute a literal whose type neither contains nor
is contained by that argument's. Rule: containment for (a); for (b) the
widest-argument rule refuses when no argument's type contains all the
others, and a variable a callback or reference holds keeps the first
argument's type, so both paths refuse (`callsite.rs::Widening`). Skip:
labeled defaults, variadic positions, a parameter typed `Any`. Built:
(b), two positionals of one variable, the first a concrete primitive,
right site the call; (a) is family 4's argument case.

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

Mutation: replace the use with `select (i64:1 == i64:1) { true => v,
false => u }`, `u` a literal of a disjoint type (typing never evaluates
the scrutinee). Rule: the consumer's. Skip: any of the parent forms above
that can absorb `U`. Built: the value directly under an arithmetic
operator, a comparison opposite a literal, a field read, or an argument
whose parameter is a concrete primitive; right site the consumer. A
comparison holds two operands of one type, so any other opposite side
can take `U` too: one that derives from `v` (`v < v`, a capture of it)
or an open cell the comparison binds. One hop is built (see Hops).

**5. Variant widening (the common case of 4).** Built: the widened
scrutinee, directly under a select with no catch-all and no type-test
arm, right site the select. Open: the new arm at the producer and the
retyped payload. Site: a value whose
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
(`node/mod.rs::write_mismatch`); the pinned use then conflicts. A `let`
over ⊥ is skipped (its type is a cell the first use decides, so an
inserted writer decides it; `ExprTypeSite::cell` marks it). Right site:
for the writer, the writer or the `let`; for the retyped initializer,
the `let` and every later statement that reads or writes `v` or a `let`
built from it, since every one now meets the new type and the checker
reports whichever it reaches first.

**7. Labeled and optional arguments.** The rules, as the checker has
them:
- a call supplies every required label, names no label the callee
  lacks, and may omit a label with a default; labels match by name,
  never by position;
- a default is checked at the definition against its parameter's type
  (or a declared variable's constraints, `lambda.rs::check_defaults`)
  and again at each site that omits it, where it may narrow that site's
  cells;
- `F ⊇ G` for function types (`fntyp.rs::align`): the same number of
  positionals, paired in order; every label of `F` present in `G`;
  every label of `G` that `F` lacks optional in `G`; and never `?#x` in
  `F` against a required `#x` in `G` (a caller of `F` may omit `x`).

Mutations, shallow first: a label the callee lacks; a required label
dropped; a label passed twice. Deep, from the type map: a labeled
argument of a type disjoint from the parameter's resolved type; an
omitted default made explicit with a disjoint value (the site then
narrows differently from the definition); a labeled function passed
where the expected function type fails `align`, by a label the
expected type has and the value lacks, or a required label where the
expected type says optional. Skip: a parameter typed `Any`, a
polymorphic labeled parameter whose other uses are not concrete. Built,
as three families so their ids stay distinct: `label-unknown` (a label
the callee lacks) and `label-missing` (a required label dropped), right site the call; `label-default` (a labeled lambda passed as a value
loses a default the expected type lets a caller omit), right site the
definition and every statement using the lambda. Open: a label passed
twice, the disjoint labeled argument, the explicit disjoint default.

## Labeled and optional arguments across the fuzzer

Family 7 needs call sites to work on, and today the generators have
almost none: they pass the required labels of a few stdlib functions
(`str::sub(#start: .., #len: ..)`, `take(#n: ..)`), always all of them,
always in one order, and never define a function with a labeled
parameter; about a dozen corpus pins have labeled lambdas, and no
mutation knows about labels. The logic is soaked only by accident, in
either direction, and it has runtime semantics as well as typing ones
(a default is born with the binding and delivers FIRED at a fresh
callee's first dispatch, `wake_catchup.md`), so the work serves
the regular soak, not only this lane.

**Generation (every lane).** The generators define functions with
labeled parameters, required and defaulted, and call them:
- defaults are literals, expressions over earlier bindings, and
  occasionally a value a stream supplies, so the born-with-the-binding
  delivery is exercised under the schedules;
- some labeled parameters are polymorphic (`#x: 'a = ..`, with the
  default checked against the variable) and some are function-typed
  (a labeled callback);
- each call supplies or omits each default at random and orders the
  labeled arguments at random;
- labeled functions travel as values: bound, passed to a function that
  calls them (supplying some labels, omitting others), and stored in
  arrays or structs whose element type names the labels, so function
  type containment with labels runs on every such site;
- the reactive generator adds labeled calls inside select arms and seq
  steps, so a default's first dispatch meets sleep and wake.

Program generation stays type-correct by construction: a generated call
always satisfies the rules above, and `gen-check`'s compile rate must
stay at its current level.

**Must-accept transforms (typemorph).**
- `label-permute`: reorder the labeled arguments of a call. Labels bind
  by name, so this is graded SOUND: a flip is a compiler bug.
- `default-materialize`: write an omitted default out explicitly, as
  the default expression itself; and its reverse, `default-elide`:
  drop an explicit argument whose text is the callee's default. Graded
  EXPECTED: the omitting site's check may narrow cells differently from
  a supplied argument, which is a rule to learn, not necessarily a bug.

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
checker bugs). Built: one hop for family 4, an unannotated, non-⊥,
concrete top-level `let w = e` whose `w` a reached use puts under an
arithmetic operator, a comparison opposite a literal, or a field read
gets `e` widened;
right site the `let` and every statement `w`'s new type meets (family
6's closure). Open: longer chains, and hops for 5.

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

A mutant's line in a typemorph report is `REJECT <family>#<site> ok`,
or `FLIP <id> LEAK: accepted` / `FLIP <id> MISPLACED: <head>`; a FLIP is
confirmed in a fresh process and recorded like any typemorph flip, as
`typeflip_N.gx` once per class (the id without its site + the
normalized head), so both lanes share confirmation and dedup. Open: the
differential run that adjudicates a LEAK.

## Integration

The families run inside the typemorph source: a subject yields its
must-accept probes as now and its must-reject probes on the same base;
an accepted must-accept probe is itself a base for must-reject probes.
The cap per family per subject is typemorph's (`TM_CAP`). No new soak
source: the typemorph share covers it, re-weighed from findings per
CPU-second once it runs.

## Tests

Each family has cases in the lib test in the style of typemorph's
(`extract_skips_param_type_reads`): sites it must take, sites it must
skip, and the text of the mutant.

## Findings

The first runs (150 generated programs and the 507 corpus pins, about
1,400 right-site refusals) found no LEAK. One MISPLACED was a checker
bug: a select's scrutinee and guard errors were not wrapped, so the
error sat on the whole select (`node/select.rs`, pin
`lang::select::a_guard_or_scrutinee_error_is_placed_there`). The rest
were skip-list holes (a retyped `let` meets its other uses first; a
`let` over ⊥ is a cell), fixed in the catalog.

## Risks

- **Rules move.** A family's argument cites a rule; when the rule
  changes (the widening rule changed this week), the family changes in
  the same commit, or its findings are false.
- **Coverage of the argument.** A family is only as good as its skip
  list. The first runs will mostly find skip-list holes; they are fixed
  in the catalog, the same way typemorph's preconditions were.
