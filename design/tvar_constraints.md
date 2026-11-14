# TVar cell constraints

Status: built 2026-07-12
Pins: `stdlib/graphix-tests/src/lang/select.rs` (`gated_scalar_unannotated`), `stdlib/graphix-tests/src/lang/types.rs` (`concrete_targets_are_known_where_they_settle`), `stdlib/graphix-tests/src/lang/functions.rs` (`same_named_tvar_in_callback_arg`), `stdlib/graphix-tests/src/lang/functions.rs` (`unannotated_three_level`, `inlang_map`), `stdlib/graphix-package-rand/src/test.rs` (`rand_float_default`), `graphix-types/src/expr/test.rs` (the round-trip proptest), `graphix-fuzz/findings/{settle-order-jul2026,infinite-type-jul2026,tvar-alias-sever-jul2026,bound-cell-cycle-accepts-aug2026}/`

## The problem this solves

Before this design a type variable cell was a name and an optional
binding. The only constraint machinery was `FnType.constraints`, a list
on the *signature* checked post-hoc at call sites after argument
unification. A node that learned something partial about a type —
arith knowing its operands are numbers — had two tools: bind the cell
now, or leave it unconstrained. Arith bound the cell to the wide
`Number` primitive set, and the wide binding caused three real
problems:

1. **Bare lambdas were monomorphic-wide.** `let f = |a| a + a`
   inferred `fn(a: Number) -> Number` with the wide set baked into the
   cells, so per-site freshening had nothing left to freshen and
   `let x: f64 = f(f64:1.0)` rejected — infectiously, through any op
   touching the result. The explicit `'a: Number` form worked only
   because its return IS the param cell.
2. **Wide sets de-fused regions.** A wide primitive set genuinely
   denotes several register classes, so no freeze could pick one and
   the region silently node-walked; annotating one `let` fixed it —
   exactly the unpredictable performance cliff Graphix exists to avoid.
3. **Bad error locality.** The wide set was legal where it was baked
   and collided with reality downstream, printing a wide-set mismatch
   instead of the violated constraint.

## The design

The cell carries the constraint. `TCell` (`typ/tvar.rs`) holds
`binding: Option<Type>` and `constraints: SmallVec<[Type; 1]>` — a
CONJUNCTION every future binding must satisfy. There is one
enforcement point: the bind sites in the `contains` walk
(`cell_constraints_ok`, `typ/contains.rs`). Binding an unbound cell to
`t` requires every conjunct to contain `t`; violation is a type error
at that site, naming the constraint.

Why a conjunction rather than a single `Option<Type>`: aliasing two
unbound cells must merge their constraints, and alias sites have no
`Env` to intersect types with. Merging conjunctions is infallible; an
unsatisfiable conjunction is detected at SETTLE time by the witness
rule — `TVar::settle` binds an unbound constrained cell to the
narrowest conjunct every other conjunct contains, and "no witness" is
the "unsatisfiable constraints on 'a: i64 & string" error. `Singleton`
refuses a set, not its members, so a conjunct only it refuses
(`Number & Singleton`) is no refusal: the cell stays open for what later
decides it, and ends at ⊥ if nothing does.

A cell may also be bounded from below: `TCell::lower`, a list every
binding must contain (`cell_constraints_ok` checks it beside the
conjuncts). A constructed abstract value's parameters get one
(`ContainsFlags::Lower`, outside function arguments: the payload's part
becomes the bound instead of the binding), and so do the open parts of
what such a cell binds to: every bind of the cell commits `t ⊇ l` for
each bound under `ContainsFlags::Lower` (`commit_cell_bounds`), so `'p`
above `Array<i64>` and `Array<string>` bound to `Array<'a>` puts `'a`
above both, whatever their order, and `⊥` holds no bound but `⊥`. The
bounds commit together, as their union, on copies first
(`commit_lower`): a union takes a bound that is an open cell after the
others, from the members they left, as a union holding a union does
(`[i64, Array<i64>]` above `'x` and `Array<'x>` makes `'x` `i64`), and
bounds a probe admits apart but no binding holds together (`(i64,
string)` above `('x, 'x)`) refuse the bind and bind none of their cells:
a refusal another walk ignores (a select's settle against its arms)
leaves nothing behind. The trial is made once, by the outermost commit: the
commits below it, in the trial and in the commit that follows it, choose
between binding the union and `IntoMember` by a probe, so a chain of
bounds, each link's bound an open cell of the next, costs a polynomial
of its length, not a doubling per link (`deep_nesting`'s
`chainboundsarrays`). An open cell of a bound below a union binds to
the union where that holds every bound, else the union's open member
takes it as a bare cell would (`ContainsFlags::IntoMember`): a slice's
element set `['h, 'r]` that must hold `'x` and `Array<'x>` puts `'h`
above both, where `'x := ['h, 'r]` would hold itself under a
constructor.
Neither choice is complete without an upper bound on `'x`: the
member loses the union's decided members (`[i64, 'a]` above `'x`, then
`'a := string`, would refuse `'x <- 1`), and the binding makes `'x` one
type with the union's open cells (`[h, ..]` over a payload above
`Array<Array<'x>>` and `Array<'y>` binds `'y` to the element set, which
the other bound makes an array, and refuses a later `y <- "s"`).
The bounds a select commits are those built before it: `[h, ..]` over
`B([x])` makes `x` the element set, so a later `b <- B([[x]])` would
hold `x` under itself and is refused, though the payload's union
declared where `b` is bound (`B<[Array<'x>, Array<Array<'x>>]>`, `'x`
written as `x`'s type) holds both. The refusal names that declaration
where a bound cell of the held type keeps lower bounds and what is
written there waits for or holds a value of no cell but a built value's
binding (`Type::decided_part`, `write_mismatch`). It holds both only
where what is written does not read the target, which the types do not
tell: reading the payload through a helper, a lambda, a reference,
another binding or a later write of what was built (`x <- t ~ v`, `v`
selected from it) is a cycle. So the refusal says both, the declaration
first, then that a value reading the target needs a named recursive
type; a write whose own references name its target is told the
recursive type alone. Any other refused write is told the union of
the two types alone, except one whose own references name its target
and whose type holds the target's, or the target's part at its place,
under a constructor (`src <- [0, src]`, `Type::nests`): it is told the
recursive type alone. An annotated element union
binds the same way with no select:
`B<Array<[i64, Array<i64>]>>` built from `[x]` makes `x` the whole
union and refuses `B([[x]])`, which `x := i64` would hold. A module
whose check commits bounds into a cell the code around it left open
decides that cell, which it may not (`tvar::decided`): a module's
select over `B([x])`, `x` an outer `let x = never()`, is refused for
an annotation of `x`. Each
conjunct must contain a lower bound when it is added, and a predicate
conjunct (`Concrete`, `Data`, `JsonData`, `Discernible`, ...) is handed
to the open cells of each lower bound, whichever came first
(`TVar::add_cell_constraint`, `TVar::add_lower`): a writer's
`JsonData` reaches the variable a payload bounds `Box`'s parameter by
in `|x| json::write_str([Box(x), `A])`, so each call judges it; a merge joins
both cells' lists after each side's conjuncts admit the other's (a
rigid cell admits none but itself, ⊥ and what one of its lower bounds
holds, `contains::rigid_holds_by_lower`, and an open cell below a rigid
survivor merges into it, since it can be no other type: `W<'c> ⊇ W(x)`
under a rigid `'c` makes `x` `'c`; one whose merge is refused, the
occurs check's case, stays below it, a lower bound); a copy, an image
and a level claim carry the list. A rigid cell so takes on the
conjuncts of an open cell merged into it, which its definition's
signature then carries; a judgment against a signature whose bounds are
fixed (`Type::implies`, an interface's bounds: `ContainsFlags::Sealed`)
merges an open cell into a rigid one only where the open cell's own
conjuncts admit it, and there a rigid cell holds a predicate only where
one of its own conjuncts implies it (`Ordered` implies `Discernible`,
`Singleton` implies `OneNumber`), a member of a union as well as a cell
alone: `NoError` and `OneNumber` read such a member as its caller's pick
(their sections below). The witness
of a cell with lower bounds is their union, so every settle binds it
there, one a ⊥ was produced into as well (`CellFact::BottomFed`)
included; a probe of a trait bound reads it so, and a commit settles it.
A settle there is a bind, and commits the cell's conjuncts over the
witness as a containment's bind does (`TVar::commit_conjuncts`): a lower
bound is only probed against a conjunct when it is added, so the settle
is where the open cells a conjunct holds take their part. Under `'a0:
['a1, null], 'a1: ['a2, null]` an `x0: (i64, i64)` puts `'a1` above the
pair, an `x1` narrower than it settles `'a1` there, and that puts `'a2`
above the pair too, so an `x2: null` is refused, where an `'a2` left
free took it and no instance held the three. A conjunct that does not
hold the witness leaves the cell open, "unsatisfiable constraints"; a
call's settle of the variables a formal holds drops that error, and the
argument that prompted the settle is the mismatch. A bound that holds one
variable twice takes one type for it there as at any bind: `'a1: ('a2,
'a2)` above `(i64, string)` does not settle, where the same pair
annotated `([i64, string], [i64, string])` does. The
settles that bind so: `TVar::settle_witness`, and a containment whose
wider side holds the witness (`'a ⊆ W ⊆ t`, below). A settle so decides
more than its own cell, and a run of them over a set of cells (the
variables a call's formal holds, `Widening::settle_bounded`; the cells
of a call's result no argument holds; the cells a trait bound's probe
settles) takes a cell after those whose conjuncts reach it, and by name
where neither reaches the other (`settle::settle_order`): the verdict
turns on no map's order. Pins:
`lang::types::a_bound_chain_{hands_on_what_a_settled_link_took,refuses_what_a_settled_link_does_not_hold,over_undecided_elements_builds,refuses_null_below_a_settled_link,in_one_argument_settles_in_the_chains_order}`,
`a_result_settled_at_its_lower_bounds_hands_them_to_its_bound_refused`,
`a_settle_under_a_bound_holding_a_variable_twice{,_refused}`,
`a_result_settled_under_a_bound_holding_a_variable_twice{,_refused}`,
`typ::settle::test::{a_settle_at_lower_bounds_commits_the_conjuncts,a_run_of_settles_takes_a_bounds_holder_first}`.
Two lower bounds that are the same type but for open cells nothing
bounds (no conjunct, no lower bound, not rigid, not a scheme's) are one
bound, and those cells one cell: a settle welds them before it takes the
witness (`TVar::weld_lower`, `setops::lower_collapses`), so `B([never()])`
met by `B([])` settles the payload to one array, where the union
`[Array<'_1>, Array<'_2>]` is two members nothing decides between, which
the next type to go into it is refused for. A later decision of one cell
is a decision of both. Two cells earlier compile tasks left open stay
two, and a probe of the witness welds nothing (Eric 11-12; pins
`lang::types::payload_bounds_apart_by_undecided_cells_are_one`,
`typ::settle::test::a_settle_{welds_lower_bounds_apart_by_unbounded_cells,keeps_lower_bounds_apart_by_more_than_unbounded_cells}`).
So where a settle comes before the writers of two bindings whose cells
it made one (a call whose result holds the payload and whose arguments
do not settles it at the call), a writer of one that the other's does
not fit is refused, naming the binding to declare, and two parameters
made one in a body take one type at a call, as the writers of a `let`
over `never()` do. A declared binding or parameter stays apart
(pins `lang::types::{payload_cells_made_one_are_decided_together,declared_payload_cell_stays_apart,parameters_made_one_by_a_payload_take_one_type,declared_parameters_keep_two_payloads}`).
A bound cell welds nothing, and of an open cell's lower bounds only
those that hold an open cell are compared: a cell many writers bound
from below is settled once for each, and decided bounds have nothing to
weld (pin
`typ::settle::test::a_settle_compares_only_undecided_lower_bounds_of_an_open_cell`).
A decided lower bound that holds ⊥ in a part, and that another decided
lower bound contains, is no member of the witness: it adds no value to it
(`settle.rs::joined_lower`). `B([])` met by a definition's `B([z])` at a
call that decides `z` as `i64` settles the payload to `Array<i64>`, the
call's settle having bound the empty array's element to ⊥ first; the
union `[Array<_>, Array<i64>]` holds no other value. A bound
that holds an open cell stays a member, and so does one nothing contains
(pins `lang::types::undecided_payload_met_by_a_decided_one_at_a_call_builds`,
`typ::settle::test::a_witness_leaves_out_a_bound_holding_bottom_that_another_contains`).
A cell binds only to a type that holds each of its lower bounds, so one
above `[i64, string]` does not bind to `i64`. A call's argument that
misfits such a variable of its formal settles it first, at the union of
its lower bounds, where it stands whatever else it meets
(`callsite::Widening::settle_bounded`): a declared bound's applications
leave a variable so (`'a0: (T<'a1>, T<'a1>)` met by a `(T<[i64,
string]>, T<[i64, string]>)`), and a later argument inside what they
gave fits it, as it fits the bound written out; a wider one widens it,
as it widens any (pins
`lang::types::an_argument_a_bound_variables_lower_bound_holds_fits`
and its twins, `a_member_of_a_bound_variables_lower_bound_fits`, the
refusal `.._does_not_hold_is_refused`; under `GRAPHIX_NO_SUBST`,
`no_subst::an_instance_checked_again_fits_what_a_lower_bound_holds`).
Only a call does: a select's pattern over such a cell still decides it
(`lang::types::select_arms_at_two_depths_over_two_payloads`).

A cell a conjunct holds (`'a3` in `'a1: ['a3, null]`) is not the cell being
decided: where it stands above lower bounds that hold the type a conjunct's
probe or commit meets, and the type holds not all of them, it holds the type
as it stands and binds nothing (`contains::held_by_lower`,
`ContainsFlags::Conjunct`; `FnType::bounds_hold` asks it too), so `'a1` and
`'a2` over `'a3` take an `i64` and a `[i64, string]` in either order (pins
`lang::types::narrower_argument_under_a_cell_above_a_union_*` and
`narrower_argument_under_a_bound_chain_over_a_recursive_typedef`).

No pattern decides a cell held rigid. A pattern that tests or binds
inside a value of one (a definition's declared variable, a variable its
signature names, an impl's parameter)
needs a written test, which types its binds, and a name or `_` over the
whole value keeps the variable (`StructurePattern::meets_rigid`, judged
as a select, a destructuring `let` or a destructuring parameter is
checked, before any narrowing, and a test written over the variable
itself as its arm compiles).
A written test names the definition's variables as a `let`'s annotation
does, and types a bind inside a rigid cell by its own part
(`Type::matching_parts`), so that part must be closed: a test whose part
over the cell holds `_` (bottom, which types nothing; written or in the
body of a typedef the test names, `Type::holds_bottom`) or a variable no
definition declares (which the arm's body would decide) is refused,
naming the closed test to write (`StructurePattern::meets_rigid_written`,
which walks the written test beside the scrutinee). A name over a value
holding the cell keeps it where a part of the test around the name holds
the scrutinee's part there, as on the same test with no variable inside
(`Type::matching_type`, `PatternNode::type_inside_rigid`): under `'b:
[`A(i64), `B(i64)]`, `` ([`A(i64), `B(i64)], [`A(i64), `B(i64)]) as
(`A(z), v) `` over `('b, 'b)` types `z` as `i64` and `v` as `'b`, while
`` ([`A(i64), `B(i64)], `X) as (v, `X) `` over `` ('b, [`X, `Y]) `` types
`v` by the test. A name that stands inside the cell in one alternative or
member and over a whole value of it in another
(`StructPatternNode::binds_inside_rigid`) is the union of both types. A
name over the cell's value at the top of the written test is typed by
the test: ``[`A(i64), `B(i64)] as v`` over `'b` gives `v` the bound, and
a body returning it as `'b` is refused. A part holding a variable of the
test's own types none of the cell's values (a run-time test passes every
value at that variable), so a name under it keeps the cell
(`Type::matching_type`; `design/select_coverage.md`, Patterns inside a
declared variable; pins
`lang::select::written_test_own_variable_over_a_declared_one_*`). A
name the test types at `_` where the cell stands (`_ as w` over `'a`,
`(i64, _) as (n, w)` over `[(i64, 'a), (string, 'a)]`) would be bottom,
and a whole capture of a test holding such a `_` (`(i64, _) as p`) would
hold it, while the run-time test for bottom passes every value: it is refused,
naming `Any` or the type the test takes (`StructurePattern`'s
`type_inside_rigid`; pins `lang::select::written_bottom_*`). An
abstract pattern names the type its compile resolved, an alias's
included (`CompiledAbstracts`); a name a later statement of the block
declares is that type there, in a function above the declaration too, and
refused in a statement above it (`design/env_independent_typerefs.md`,
Block types).
At a union holding the cell beside other members a part meets it where
it may hold one of its values, by its typed bounds, any value without
one, inside a member the whole pattern may hold (a pattern that cannot
match a member tests nothing in it). A part that may not, a literal beside it, leaves a name beside it
over the whole union: the settle's copy is never a rigid cell's pattern
cell again (`Type::contains_through_copy`), and a slice's elements
narrow one by one over a union element type, never paired with one of
its members (`Type::needs_split`).

Coverage counts a rigid cell's values apart from its neighbours'. In the
literal pool a position holding an open cell has a head of its own,
`Head::Open`, which only a name or `_` there admits, so `(true, _)`,
`(false, null)` over `(bool, ['b, null])` leave `(false, v)` for an arm
of its own, and a tuple or struct member beside the cell is never the
position's only shape. In containment (`Type::covers_by_parts`, which
set coverage and every annotation reach) a rigid cell at the position
the distribution splits is its one typed bound's members, each also
taken by a part holding the cell itself, and with no one typed bound a
member of its own; a rigid cell that bound holds splits the same way, its
members taken also by a part holding either cell, and an open cell there,
which a bound names only as a variable of the same definition, is held
rigid while the split probes, so under `'a: string,
'b: ['a, `X]` the strings at a `'b` position are a member none of
`` [`X, null] `` covers: `[(`A, `X), (`A, `Y), (`B, 'b)] ⊇ ([`A, `B], 'b)`
under `'b: [`X, `Y]`, while `(`B, null)` covers no member of `'b`
beside the null. A written test over a closed type covers the part of
it its structure takes (`StructPatternNode::matched_part`), so tests
that split the bound between them cover the variable.

A select's arms decide a payload's open parameter as they decide an
inferred scrutinee: the settle binds the cell to a copy of their
patterns (`Reach::new`, `Type::contains_through_copy`; only an open-cell
scrutinee takes the patterns' own cells as its parts, so a name is
typed by its arm's narrowing alone, its part of every member the arm
can match), the bounds commit into the copy's cells, and each arm's
narrowing writes its structure into the cells of the others' through
the parameter. A copy the settle leaves open, the part an open cell
inside the scrutinee took (`v` in `select (v, 0) { ([x, ..], _) => ..
}`), is its pattern's cell again, so the arm's binds decide it.
Arms at two depths or lengths over a payload built at two shapes
(`[[h, ..], ..]` beside `[h, ..]` over `B([["a"]])` and `B(["b"])`;
`[h, k]` beside `[h]`; `` [`V(a), ..] `` beside `[h, ..]`) decide a
type their binds cannot hold beside what was built, as a `let x =
never()` they select cannot then take a value of both shapes. Where the
bounds hold an open cell, a literal arm beside them (`[1, h, ..]` beside
`[h, ..]` over `B([[x], [x]])` and `B([x, x])`) is refused too, though
such a let takes the value. The refusal names what was built, which
annotated decides the cell instead (`Reach::untyped_binds`): a cell open
when the arms decide is written as the type of the binding it stands
for (`[Array<'x>, Array<Array<'x>>]`, `'x` the type of `x`), else as
the variable a program wrote for it (`node::Holders`,
`TVar::written_own`), else numbered as the two-member refusal below
numbers one, `'_1`, one number a cell. A call's copy of a variable its
callee declares is no variable the program wrote for the copy
(`TCell::naming`): `[Array<string>, Array<Array<'_1>>]` where `g(never())`
over `|u: 'a| -> Array<'a>` built the inner array. A copy merged with a
cell a written variable names is a copy no more: the cell is the
variable's (`Naming::Taken`), and every handle of it writes the variable's
name, the copy's own handles included. The hint names
no type only where what was built has no written form. Two payloads
of one Graphix-minted abstract type are declared as the union of the
parameter (`B<[P1, P2]>`), read through one application,
unless that is the held type already (a parameter decided apart). Only
a containment refusal of the binds (a
`TypeMismatch` or an `InfiniteType`) is told so; any other passes
through.

A conjunct is an upper bound, so a type wider than the cell's witness
holds the cell without the cell becoming it: `t ⊇ 'a` that the
conjuncts refuse as a binding (`'a := t`) settles `'a` to its witness
`W` and asks `t ⊇ W` (`'a ⊆ W ⊆ t`). This is how an annotation widens
a generalized lambda's partially inferred result (`let f = |a| (a,
u8:1)` returns at most `('x, u8)`; `(i64, [u8, null]) ⊇` it holds).

### Producers

- The explicit `'a: C |...|` form is sugar: the parser
  (`typexp::fntype`) and `Lambda::compile` alias same-named signature
  leaves onto the declared quantifier's cell first, then
  `add_cell_constraint` — the conjunct lands in the one shared cell.
  A declared quantifier is one the signature holds: a position names
  it, or the bound of a quantifier the signature holds does (`'a:
  Array<'b>, 'b: Int |x: 'a|`). Any other is refused where it is
  declared (`FnType::check_quantifiers_held`: the parser for a written
  `fn<..>`, `Lambda::compile` for a definition), since no call could
  choose it. Pins: `lang::types::a_quantifier_only_the_body_reads_is_refused`
  and its neighbours, `parser::test::a_fn_type_holds_each_quantifier_it_declares`.
  A bound may not reach its own quantifier, directly (`'a: 'a`), under a
  constructor (`'a: Array<'a>`) or through another quantifier's bound
  (`'a: 'b, 'b: 'a`): `check_bounds_acyclic` refuses it in the parser
  for a written `fn<..>`, in `Lambda::compile` and in `traits::impl_head`,
  before any cell holds it: checking a call against such a bound binds
  the variable to a type holding itself, a check without end. A
  recursive bound is a recursive type (`type T = [i64, Array<T>]`, `'a:
  T`). That check reads names, before the cells are aliased, and a nest
  of quantified signatures can leave one cell under two names it held
  apart; so `seed_conjuncts` asks the cells too, and refuses a conjunct
  that reaches the cell it would bound (`FnType::declaring`,
  `traits::impl_head`), as an alias that closes a cycle is skipped. So
  no cell the parser builds holds itself, every walk of its cells
  returns (a print, an enclosing signature's parse), and the syntax
  codec refuses one that does rather than write it without its bound.
  Pins: `lang::types::a_quantifier_bounded_by_itself_is_refused` and its
  neighbours, `parser::test::a_bound_reaching_its_own_variable_is_refused`,
  `typ::fntyp::tests::a_seeded_bound_reaching_its_cell_is_refused`,
  `a_nest_parses_to_cells_no_bound_of_which_reaches_its_cell`,
  `serialize::test::a_cell_holding_itself_does_not_pack`.
- A definition's gate keeps what its body bound: the signature is the
  scheme, relations between its positions included (`|x| [x, x]` is
  `fn(x: 'a) -> Array<'a>`), so a call's types follow from the signature
  without elaborating the body. Only a cell bound to ⊥ reopens
  (`unbind_vacuous_tvars`: `throws := ⊥` means nothing was observed). A
  def-time binding is a fact, never erased by instantiation.
- Arith and `Neg` constrain instead of binding: an unbound operand
  cell gets a `Primitive(Typ::number())` conjunct (`node/op.rs`).
  Arithmetic adds `Singleton` beside it (below). Same-cell operands
  (`a + a`) alias the result to the operand cell, so `|a| a + a` infers
  `fn<'a: Number + Singleton>(a: 'a) -> 'a`, identical to the explicit
  form. Distinct-cell operands get a fresh constrained result cell; the
  operand cells settle with the check (`PendingSettle::Operand`), and
  the rule's bounds are their conjuncts, so no settle judges it again.
- An array index or slice bound constrains the same way: an unbound
  index cell gets a `Primitive(Typ::integer())` conjunct
  (`node/array.rs::check_index`), so `|x| a[-x]` keeps `[Real, Sint] &
  Int` on `x` and each call's argument meets both; a known index type
  is checked against the integers directly.

Rejected for the distinct-operand case: unifying the operand cells
(a semantic tightening — mixed-type calls that passed would reject),
and keeping the result wide (leaves two-param bare lambdas with the
annotation-reject behaviour). Arithmetic has since been made
homogeneous (`fn<'a: Number + Singleton>(x: 'a, y: 'a) -> 'a`), which resolves the question
from the other side.

### Instantiation

`reset_tvars` freshens keyed by CELL IDENTITY — one fresh cell per
source cell — not by name, so the source's alias topology carries to
every instance (`|a| a + a` shares one cell between `'a` and the
return type in every instance too), and constraints copy to the fresh
cells. Bound cells stay bound: a def-time unification is a settled
fact, and erasing it made the static type lie about the runtime value
(the JIT marshal-panic class).

### A call's arguments are met in order

A call checks its arguments in the order of the callee's parameters,
each against its formal as the arguments before it left it, then the
labeled defaults the call omits, in the order they are declared. Nothing
waits for a later argument (Eric 11-13, D36: soundness is the bar, and
"the same verdict in every parameter order" is not a requirement). One
exception stands: a formal holding a variable under an abstract type, a
type application or a writable reference is checked before the others
(`callsite::holds_invariantly`), since that position fixes the variable
exactly. It is kept on purpose (Eric 11-13): a label always stands
before the positionals, so `db::cursor::new(#prefix: [db], t)` could not
put the tree that fixes the key type first.

A fresh instance's variable that only data positions hold
(`callsite::widenable`) widens: a decided argument (one holding no
undecided cell outside function types, `Type::has_free_in_data`; a
lambda's open cells are not undecided data) that does not fit its formal
is probed once against the formal with those variables open
(`Widening::widen`), and a variable it holds wider is rebound to its
type. So two decided arguments settle a variable alike in either order
(`f(1, n)` and `f(n, 1)` over `|x: 'a, y: 'a|`, `n: [i64, null]`), with
no state but the variable. One neither wider nor narrower than what
stands is the mismatch where it is written, whatever follows (`f(`A, `B,
ab)` over three `'a` is refused; `f(ab, `B, `A)` runs): no union is
made of two arguments.

A variable also counts as held by data alone only when no bound holds it
otherwise: `widenable` walks each such variable's cell constraints,
transitively, and a variable one of them holds under a function type, a
writable reference or a nominal type does not widen (`'a: fn(q: 'c) ->
i64` with `|v: 'a, c: 'c|` takes `f(|q: string| 0, n)` over `n: [i64,
string]` as the mismatch the function type written in the parameter
gives; pins `lang::types::a_variable_held_by_a_bound_*`). A recursive
typedef's application met inside its own expansion is passed by
(`DataPositions::passing_repeats`), as it holds its variables as the
expansion told them.

A widening leaves every decided cell above its lower bounds. A lower
bound holds its cells live, so widening a variable grows each bound that
holds it, and a cell an earlier argument decided may hold the bound no
more: over `|a, z| a <- B([z])` the payload stands above `Array<'z>`,
`x: B<Array<i64>>` decides it, and `n: [i64, null]` would widen `'z`
under it; the instance would write a null into an `Array<i64>`. So after
a widening every decided cell the instance's signature reaches is probed
against its lower bounds (`Widening::below_bounds`), and one that held
them before it and no longer does undoes the widening and refuses the
argument, naming both types and the other argument to annotate (a
payload is met before an argument written ahead of it: `f(n, x)` over
`|z, a|`). The same holds a payload a REPL input
settled (`let x = B([])`, then `f(x, 1)`) and a payload read back into a
decided parameter (`|a, z| { let b = B(z); a <- b.0; a }` at `x: i64`
and `n`). A payload still open at the call takes both arguments' bounds
and settles to their union. Pins:
`lang::types::{decided_payload_takes_no_wider_argument,decided_payload_written_after_takes_no_wider_argument,empty_declared_payload_takes_no_argument,payload_read_into_a_decided_parameter_takes_no_wider_argument}`,
their annotated twins, and
`lang::repl_inputs::settled_payload_takes_nothing_a_function_builds_into_it`.

A widening binds its variable again, so the variable's conjuncts commit
over the wider type as at any bind (`TVar::commit_conjuncts`): a
variable its bound holds, which the narrower argument left open, takes
its part of the wider one. Under `'a2: [('a3, bool), bool]`, `x1:
Array<'a2>` given `[true]` decides `'a2` as `bool` and says nothing of
`'a3`; an `x2: [(duration, bool), bool]` widens `'a2` and makes `'a3`
`duration`, so `x3: 'a3` given a string is refused. A conjunct that does
not hold the wider type undoes the widening, and the argument is the
mismatch. Pins:
`lang::types::a_widening_hands_its_bound_the_wider_type{,_refused}`,
`a_widening_over_an_undecided_element_hands_its_bound_the_wider_type_refused`.

Before its own check, an argument holding an undecided cell is
pre-unified with a copy of its formal in which the widenable variables
are open and every other cell is the formal's own (`Widening::opening`,
`Type::pre_unify_copy`), so the pre-unification refuses no argument
wider than the ones before it; a formal holding no widenable variable is
pre-unified as it stands. An open variable of the copy stands above no
lower bound either: one a bound's holder put above a type (`'a1: ['a2,
duration]` given a `[null, string]` puts `'a2` above it) would hand the
bound to the cell it merges with, a literal's element, and the element's
own check refuses a `"s"` narrower than the bound, where the argument
checked alone (`["s"]` at `x2: Array<'a2>`) fits the variable at its
meeting with the formal (pins
`lang::types::literal_narrower_than_what_*`,
`literal_outside_what_*_refused`,
`undecided_value_after_a_literal_narrower_than_what_*`). An expression
whose type its own
check has yet to make (a seq step's captured argument, a field read) is
not pre-unified with the copy. Neither is an argument holding no undecided
cell (a declared variable of the definition the call stands in, a
written type), nor a part of one that holds a declared variable and no
undecided cell: a declared variable merges with the open cell it meets
and keeps that cell's bounds, so one that met the copy would stand
within the callee's bound twice, once over the copy's cells, which no
instance has and nothing decides. It meets the formal itself, and stands
within its own bound and the callee's, over the instance's cells (pins
`lang::types::declared_variables_passed_on_through_{a_chain_of,recursive}_union_bounds`,
`declared_variable_passed_on_stands_within_its_callees_bound{,_refused}`,
`declared_variables_passed_on_through_an_inner_function_and_a_callback`,
`declared_variables_passed_on_beside_callbacks{,_take_decided_arguments}`).
What an undecided argument took from the copy is made the formal's own
before the argument meets the formal: each cell of the copy that nothing
decided becomes the formal's cell it copies (`callsite::rejoin`,
`TVar::rejoin`), so no copy stays in the argument's type or in a
wrapper's signature (pin
`lang::types::wrapper_of_a_bound_over_another_variable_infers_no_bound_on_that_variable`).
A copy that the formal's cell reaches stays apart: in `|y| h([y, 1], (y,
2))` over `|a: Array<'a>, b: ('a, i64)|` the two as one cell would be an
infinite type (pin
`lang::types::undecided_argument_inside_what_its_formals_variable_settled_to`).
So does a copy that holds a bound it was not made with beside a decided
cell: the pre-unification made it one cell with the argument's, which
brought the bound (the lower bounds of a binding's open payload, `let v =
x.0`; the conjunct an operator left on a parameter, `p + q`, `a[p]`), and
the check that follows binds it only where what stands fits those bounds.
A copy is made with its formal's cell's own conjuncts (a declared `'a:
Number`), and with its lower bounds unless it is opened, so the count of
each kind when it was made is what tells them apart (`tvar::Bounds`;
pins
`lang::functions::undecided_binding_above_a_lower_bound_{refused,it_holds_joins}`,
`undecided_parameter_within_a_conjunct_{refused,it_fits_joins}`).

A part the formal writes as a union (`[Array<i64>, null]`) or as a union of
primitives (`[i64, null]`, one `Type::Primitive` of two bits) that two of its
members admit is left to the argument's own check: pre-unifying it would bind
an undecided cell to the whole union before the argument's expression is
checked (`|u| f((u, u + 1))` at `f: |y: ([i64, null], i64)|` is accepted and
`u` is decided by `u + 1`; a bound `'x: ([i64, null], i64)` does the same).
Pins: `lang::types::written_primitive_union_leaves_a_*_part_to_its_check`.

A variable does not widen where a function type of the signature holds
it (the first argument decides it, a callback's parameter being typed by
it), nor where it stands in a union beside another undecided member
(`Held::member`): which member takes a type turns on what the cells are,
so the first argument that meets it decides it, and an argument that
needs it wider is the mismatch (`f(1, "s", [n])` over `|y: 'a, z: 'b, x:
P<'a, 'b>|`; write the earlier argument as wide, `let one: [i64, null] =
1`).

An argument or omitted default holding an undecided cell outside
function types (a read target, a `let` over `never()`, `[]`'s element)
at a formal holding a widenable variable takes the formal as it stands
(`Widening::take`): first, it decides the variable with what follows
(`f(str::parse("5"), 1)` runs: the read is the Result that holds the
`1`); after a decided argument it must fit what that left (`f(1,
str::parse("5"))` is refused). Every widenable variable the formal
reaches then widens no more (`Widening::pin`): a cell outside the call
may now be one of them, or be bound to a type holding one, and whatever
read that cell since (a later callback capturing the binding) read it as
the variable stood, so widening it after would type that read wrong.
`f(v, 1, n)` over three `'a` with `v` a `let` over `never()` is refused
at `n`, "an undecided argument before this one met 'a .. pass the widest
argument first, or annotate the undecided one", and `f(n, v, 1)` runs.
This is the one place a wait was closing a hole; it is a refusal now.

A decided argument that two undecided members of its formal's union
could take (`AmbiguousMember`, below: A union's member takes a type by
rule) is refused where it is written, the message naming the remedy:
"declare the parameters that decide them before this one, or write the
type of the parameter" (`callsite::ambiguous_parameter`). With the
deciding parameters first the member they decided takes it, or the one
member still open (`f(1, "s", [1])` over `|y: 'a, z: 'b, x: P<'a, 'b>|`;
`f([1], 1, "s")` over `|x: P<'a, 'b>, y: 'a, z: 'b|` is refused). The
order is the call's whoever writes it: an inferred wrapper, a declared
bound, a seq step, a recursive call. The parts of one argument are
related together: its pre-unification (`Type::pre_unify_arg`) leaves a
part that meets the choice to the argument's own check, by which time
the other parts have decided the members, so `f(([1], 1, "s"))` over
`|x: (P<'a, 'b>, 'a, 'b)|` runs, and a literal's element meets the union
as the arguments before the literal left it (`f("s", {"k" => 1.5})` over
`|y: 'a, x: Map<string, E<'a, 'b>>|`).

Where a refusal is of an undecided argument and some decision of its
cells could fit (`callsite::annotation_may_fit`: on copies, it fits the
formal with the variables fresh and what it gives each holds or is held
by what the call gave it), the refusal says "annotate it", naming the
bindings the argument, or the omitted default, reads whose types are
undecided (`callsite::undecided_note`, `undecided_binds`); `f(1, [])` is
a plain mismatch. At a formal holding no widenable variable, where a
wider decision of a variable an earlier argument of this call decided
could hold the undecided argument, the refusal names that argument,
"annotate that argument as wide as this one"
(`callsite::decided_elsewhere`): `f(1, str::parse(s), |v| 0)` over `|x:
'a, y: 'a, k: fn(v: 'a) -> i64|` is refused so, and
`f(str::parse(s), 1, |v| 0)` runs.

Pins: `lang::functions::{widest_arg_either_order, widest_arg_first,
widest_arg_last_refused, undecided_argument_first_decides,
undecided_argument_last_refused, open_argument_takes_the_variable_as_it_stands,
open_argument_before_a_wider_one_refused,
omitted_undecided_default_takes_what_stands,
omitted_undecided_default_before_a_wider_one_refused}`,
`lang::types::{argument_into_a_typedef_union_is_decided_by_the_parameters_before_it,
argument_into_a_typedef_union_before_its_deciders_refused,
variable_in_a_typedef_union_does_not_widen_refused,
literal_element_into_a_typedef_union_after_its_deciders,
wrappers_of_a_typedef_union_function}`, the shell's
`no_subst::an_instance_checked_again_meets_its_arguments_in_the_same_order`.

### A function held at a function type

A function type that must hold a function (an annotated `let`, a
callback's formal, a field, a write) holds it position by position, in
the order written, then the result and the throws
(`FnType::contains_gated`). A variable of the function still open there
is decided by the first position that meets it, and the later positions
must fit that: nothing waits (Eric 11-13, D36). So `let g: fn(b: [i64,
null], k: i64) -> [i64, null] = f` over `let f = |b: 'y, k: 'y| -> 'y k`
takes `'y` as `[i64, null]`, and `fn(b: i64, k: [i64, null])` is
refused, "does not contain fn(b: 'y: i64, k: 'y: i64) -> 'y: i64": the
written type's earlier position is as wide as its later ones need, or
the function is wrapped in a lambda with its parameters written (`|b:
i64, k: [i64, null]| f(b, k)`), whose call widens (A call's arguments
are met in order). Two parts of one position (`('y, 'y)` held
at `([i64, null], i64)`) are two positions. A position that holds the
variable exactly (a writable reference, a nominal type's or a
constructor application's parameter) decides it as any other does, where
it is written, and a position holding a union of the function's
variables is met where it is written too, as a call's argument is.

A rigid variable (a declared one, or a quantifier of the type) that a
position gives a union holding an open variable as a member is held by
the member whole, or by its bound's parts, as the positions before it
left the member. So with `'b: [i64, null]`, `fn(s: i64, v: 'b)` holds
`|s: 'a, v: ['a, null]|` (`'a` is `i64`, and `[i64, null]` holds `'b` by
its bound) and `fn(v: 'b, w: 'b)` holds `|v: ['a, null], w: 'a|`; `fn(v:
'b, s: i64)` over `|v: ['c, null], s: 'c|` is refused, the union met
before the position that decides its member. A trait's default written
at its bound over `self` (`|s, x: [self, null]|` under `fn<'b: [self,
null]>(self, x: 'b)`) is the first form. A label stands before `self`,
so a default's label is written as the declared variable (`|#k: 'b,
s|`), or left unwritten. Pins:
`lang::functions::written_fn_type_{first_position_decides,
wider_later_position_refused, exact_position_first,
exact_position_after_a_narrower_one_refused,
union_beside_a_declared_variable,
union_before_its_members_position_refused}`,
`lang::traits::default_method_{parameter_written_at_the_declared_bound,
label_written_at_the_bound_before_self_refused}`.

The pre-unification of a callback argument with its formal
(`FnType::pre_unify_params`) meets the positions the same way, but for a
position whose union in the callback's own parameter has two decided
members that could take the formal's: it passes over that one, so a later
position may decide the variable before the callback's own check (A
union's member takes a type by rule).

### A union of variables in a signature

A written function type may not hold a union written with two or more
distinct bare type variables of its own (`['a, 'b]`, `['a, 'b, null]`),
at any part (a parameter, a callback's parameter, the result, the throws,
a bound) and under any constructor as the source writes it
(`Array<['a, 'b]>`, `{v: ['a, 'b]}`, `M<['a, 'b]>`): the walk of a
written type's names (`Type::for_each_written`, which every name check
rides) hands each union it writes in a function type to
`VariableUnion::of` and looks through no typedef, so `E<'a, 'b>` over
`type E<'p, 'q> = ['p, 'q]`, `['a, Id<'b>]` over `type Id<'x> = 'x` and
`['a, M<'b>]` over `type M<'x> = ['x, null]` pass this rule (a typedef's
own body writes a union of its parameters, not of the definition's
variables) and are refused where a value meets them (below, A union's
member takes a type by rule). It is judged
where the type is written: a lambda's signature as it compiles, in the
walk that warns of a later block's names (`warn_written`; a variable an
enclosing definition declares is not its own; a method in an impl or a
trait's default body is judged over the types it writes itself, an impl
`let`'s annotation and every lambda its value writes outside a lambda's
body: in its own lambda's signature, the rest being its trait's
declaration, an impl's head variables are the impl's and a default's
`self` the trait's (an impl's `self` is its target in every type the
impl writes, so `[self, 'c]` there is one variable beside a written
type), and every
other lambda there (one a call that builds it is handed, a labeled
default's, one in a block) names variables of its own, as at the top
level, since no definition encloses a method's scope:
`traits::written_method_unions`), a `.gxi` val in the walk that resolves
its names in the interface (`module::check_interface_names`), a trait
method's declaration in the walk that defers its names (`Trait::typecheck0`,
where `self`, a written type at every impl, is no variable) and a
function type a body's annotation writes
(`lambda::body_annotation`: a `let`'s, a select's type test,
`never<T>`). No value's type says which
variable such a member takes, so whichever relation meets it first would decide (a call's
argument first, an instance's signature last), and the refusal names the
annotation that passes: one variable there, or written types (`[i64,
string]`). One variable beside written types (`['a, null]`) is a member
a value's type picks, and a data union in an annotation (`let v: ['r,
'q] = ..`) is no signature: the value written into it is refused at the
choice (below). An impl's target is every method's `self`, so a union of
two of the head's variables there (`impl<'a, 'b> T for Array<['a, 'b]>`)
is refused as one in a signature (`Impl::typecheck0`).

Two members of one constructor that hold two distinct variables of the
definition ask the same question (`[Array<'c>, Array<'d>]`: a `[1]`
fits either), and are refused the same way, at the same sites, naming
the two members: write one variable in both, or a written type in place
of one (`VariableUnion::of`, `Type::shares_constructor`). One
constructor is two arrays, lists, maps, errors, references or function
types, tuples of one arity, structs of one field set, variants of one
tag and arity, one abstract type, one typedef by its written name, or
one constructor variable applied (`[self<'c>, self<'d>]`); a variable
anywhere inside a member counts (`[('c, i64), (i64, 'd)]`, `[Array<'c>,
Array<fn(x: 'd) -> i64>]`). Members of two constructors (`[Array<'c>,
Map<string, 'd>]`, `` [`A('c), `B('d)] ``), one variable among them
(`[Array<'c>, Array<string>]`) and two typedefs of one expansion
(`[A<'c>, B<'d>]` over `type A<'x> = Array<'x>` and `B` alike) stand:
the rule reads the written form only. That form keeps what the source
wrote apart: as a union is parsed, a variable is its name and bounds, so
two members are one only where they are written alike
(`Type::flatten_written`, `setops::written_identical`). `fn(x: 'c) ->
i64` beside `fn(x: 'd) -> i64` is two members, which the rule refuses in
a signature and a typedef's body keeps; a member written twice (`[('c,
'd), ('c, 'd)]`) is one.

The written-form rule is the earlier message, at the definition; the
rule below is what makes the choice sound, whatever the source wrote.

### A union's member takes a type by rule, never by order

A containment that puts a type into a union (`contains.rs`: `set_commit`,
`place_members`, `residue_member`, all through `Type::take`) gives it to
one member, chosen by rules that read the union alone:

- a member identical to the type, or one that holds it as it stands,
  deciding no cell of its own or of the type (`takes_as_it_stands`:
  `[Array<'a>, null] ⊇ null`), takes it and binds nothing. Two function
  types are identical where their own cells pair, and the pairs weld
  (`contains::Link`): a cell a merge left forwarding to another pairs as
  that one, so a function over a cell that forwards to a rigid cell is
  not one over another rigid cell (pin
  `typ::contains::tests::a_cell_forwarding_to_a_rigid_cell_pairs_as_that_cell`);
- else constructor members before bare variables (`Result<'a, 'e> ⊇
  Error<`X>` is `Error<'e>`'s, `['b, Array<'b>]` takes an array through
  `Array<'b>`);
- in the first of those tiers where a member can take the type (it holds
  on copies), exactly one undecided member must. Two that could (`['a, 'b]
  ⊇ i64`, `[Array<'a>, Array<'b>] ⊇ Array<i64>`) are refused,
  `AmbiguousMember`: which would take it follows the order the check
  relates things in, and a definition's check and an instance's need not
  share it. An undecided member reaches an open cell no declaration holds
  rigid (`undecided`); a decided member that holds the type takes it
  before any undecided one, and decides it: `h({v: null}, [y])` over `|x0:
  {v: 'a}, x1: P<'a, 'b>|` (`P` with a `null` member beside its arrays)
  makes an unannotated `y` null, however the union's members are written
  or named, so a wrapper `|y| ..` takes a null alone, and `|y: (i64,
  i64)|` goes to `Array<'b>` (pins
  `lang::types::undecided_element_goes_to_the_decided_member_beside_an_undecided_one`,
  `undecided_element_a_decided_member_took_holds_no_other_type_refused`,
  `undecided_element_a_decided_member_took_in_the_other_member_order_refused`,
  `written_element_goes_to_the_undecided_member_beside_a_decided_one`).
  A decided member that holds a decided type binds nothing, so it takes
  the type before any member is weighed, in a walk on copies too. Which
  members are decided is read from the cells as they stand when the union
  is met (`ContainsHist::binds_nothing`): `[Array<'a>, Array<'b>] ⊇
  Array<i64>` after an earlier part of the same containment made `'b`
  `[i64, null]` goes to `Array<'b>` and leaves `'a` open, in either order
  of the members, and where the members are applications of names (pins
  `typ::contains::tests::a_member_decided_since_the_walk_met_it_takes_a_decided_type`,
  `lang::types::a_member_decided_in_a_trial_takes_a_decided_type`,
  `a_named_member_decided_in_a_trial_takes_a_decided_type`).
  Where every open cell the member and the
  type reach is held rigid, and every one their conjuncts and lower
  bounds reach, and none is one an earlier task left open
  (`decides_no_cell`), a commit decides nothing, so it is the probe: the
  first member the probe admits takes the type, on no copies. A function
  type takes a quantifier an earlier task left open fresh for each walk,
  outside a top-level input, so a commit decides the copy however the
  cell is held (pin `typ::contains::tests::
  a_commit_decides_a_quantifier_an_earlier_task_left_open`).
- two decided members that could each take a type holding a cell to
  decide (`[Array<i64>, Array<string>] ⊇ Array<'e>`: `[z]` over a `let z =
  never()`, `[]`, an untyped lambda at two function members) would decide
  it by their place in the union, which is the sorted one, so neither
  takes it: `UndecidedTaker`, the refusal a union bound gives (below),
  naming the undecided side to annotate. They are no choice where they
  decide it alike, each holding the type as the other decides it
  (`decide_alike`: two names holding one member, `[P, Q]` over `type P =
  [A, `X]; type Q = [A, `Y]`; a lambda with its types written, whose one
  open cell is what it throws).
- members that are one type are one member (Eric 11-11, D34): the same
  cells throughout, a union read as the set of its members, so neither
  their order, a member that stands twice, a nested union nor a cell bound
  to another tells two apart (`contains::one_member`, `setops::one_type`).
  `[Array<['a, 'a]>, Array<'a>] ⊇ Array<i64>` goes through the first and
  decides the one cell, as the other would; two open cells are two
  members, however alike (`[Array<'a>, Array<'b>]`,
  `[Array<['a, 'b]>, Array<'a>]`). The same count says whether a union has
  two undecided members for an undecided type to take whole
  (`place_members`), so `['a, 'a]` is `'a` there.
- two members of the type that one constructor member would take, and that
  decide a cell of it, are taken together as one: `['a, Array<'b>] ⊇
  [Array<i64>, Array<string>, null]` is `'b ⊇ [i64, string]` and `'a ⊇
  null`, in either order of the members, whatever the typedefs' or the
  declared variables' names (`place_members`, `join_cell_sharing`: `Array`,
  `List` and `Error` members, a name standing for one included). Over
  `|x: ['a, Error<'e>]| x?` a value of two error tags gives `'e` both, so a
  handler for one tag is refused as missing the other (pins
  `lang::types::two_array_members_one_cell_hold_it_together`,
  `two_named_array_members_one_cell_are_refused_as_the_second_name_is_not_first`,
  `two_error_members_one_cell_hold_both_tags`). A member that two
  constructor members could take, a name standing for a union, and a member
  of another constructor are not joined and keep the walk's order. Under a
  writable reference the join holds too: `&mut ['a, Array<'b>]` against a
  `&mut` of two arrays is refused in either order of the members (pin
  `two_array_members_one_cell_under_a_writable_reference_are_refused`), where
  the first member used to decide it.
- of several that could, the one that holds each of the others as it
  stands (`contains::widest`) takes the type, where taking it decides
  every cell another of them would (`contains::decides_each`): what any
  of them holds it holds, so the pick follows no order. `[(i64, [`A('x),
  `B(string)]), (i64, `A('x)), (i64, `B(string))] ⊇ (i64, `A(i64))` goes
  through the first and decides `'x`, as the second would: the type a
  select gives a scrutinee whose arm binds a part whole (`(1, b)`) beside
  arms that take that part apart (`` (2, `A(x)) ``, `` (3, `B(y)) ``).
  The condition is judged on copies first, then against the walk's own
  take, which may give a part to another member than the copies' ordered
  walk did: each other member that could take the type must hold it as
  it stands once the first has. Where one would still have a cell to
  decide, none takes it: `[(i64, Any, 'z), (i64, ('c, Any), 'z)] ⊇ (i64,
  (string, i64), i64)` is refused, since the first member's `Any` decides
  nothing of `'c`, which the second member's arm reads its bind by. No
  select gives a scrutinee that union: a name an arm binds inside a part
  another arm's `_` makes `Any` is fixed at its arm's end, `Any` or what
  the arm decided (`design/select_coverage.md`, "A test inside an `Any`
  part"), so `(1, _, z)` beside `(2, (c, _), z)` is `[(i64, Any, 'z),
  (i64, (Any, Any), 'z)]` and the first member takes the argument. A
  type still undecided is refused too where the others would not hold what
  it becomes
  (`(i64, 'q)` against `` [(i64, [`A('x), `B('y)]), (i64, `A('x)), (i64,
  `B('y))] ``), and a pre-unification leaves the choice to the argument's
  check. Pins
  `typ::contains::tests::a_member_holding_the_others_takes_a_type_where_it_decides_their_cells`,
  `lang::select_undecided::bind_beside_arms_that_take_the_part_apart*`,
  `wildcard_member_holding_a_bind_inside_*`.

A type that is itself undecided is no exception: `[Array<'a>, Array<'b>]
⊇ Array<'e>` gives `'e` to neither, so `f([])` over `|x: P<'a, 'b>|`
(`type P<'p, 'q> = [Array<'p>, Array<'q>]`) is refused. Where the
parameters before it decided the members (`|y: 'a, z: 'b, x: P<'a,
'b>|`) it goes to the one member that can hold it, and where both can
(`f(1, "s", [])`, `[z]` over a `let z = never()`) it is refused for the
two decided takers, at the argument, naming the binding to annotate
(`callsite.rs::undecided_taker`; `let e: Array<string> = []; f(1, "s",
e)` runs). Under a callback's parameter the union is the argument's own
and the undecided type the formal's, so the refusal names the parameters
to declare first (`f(|q: [Array<i64>, Array<bool>]| 0, 1, true)` over
`|k: fn(q: P<'a, 'b>) -> i64, y: 'a, z: 'b|`; with `k` last it runs). A
`let` or a write holding such a union refuses the value alike (pins
`lang::types::{undecided_element_into_a_union_of_two_decided_arrays_refused,
empty_array_into_a_typedef_union_of_two_arrays_after_its_deciders_refused,
empty_array_into_a_declared_union_of_two_arrays_refused,
callback_parameter_union_of_two_decided_arrays_before_its_deciders_refused,
untyped_lambda_into_a_union_of_two_function_types_refused}` and their
typed twins, `undecided_type_into_two_decided_members_that_decide_it_alike`,
`typ::contains::tests::two_decided_members_refuse_an_undecided_type_either_could_take`).
Nothing gives an argument to a
member before it is decided: a pre-unification that meets the choice
leaves it to the argument's own check (`Type::pre_unify_arg`), a decided
member beside another that could take the undecided argument included
(`ContainsHist::early`: `g(["s"], [[["s"]]])` through a wrapper, where the
first argument has decided one member by the time the second
pre-unifies; `f(["a"])` over `|x: [A, null]|` with `type A = [Array<i64>,
Array<string>]`, where the first member took the literal before its
element was checked; a variable bounded by a union of function types, a
callback's formal, is the same: two members that take the callback and
none holding the other leave it unbound to the callback until its own
check has its result, `TVar::bound_takers_ambiguous`, pins
`lang::types::callback_*_union_bound_under_a_function*`), and the
probes behind a refusal's note are ordered walks on copies
(`callsite::annotation_may_fit`); a function's parameters pre-unified
with a formal's leave two decided takers to the argument's own check too
(`FnType::pre_unify_params`), which meets the variable as the callback's
other positions left it: `h(|q: [Array<i64>, Array<string>], r: string|
1)` over `|k: fn(q: Array<'a>, r: 'a) -> i64|` runs with `'a` a string,
`r: [i64, string]` there is a mismatch at `q`, and a function the call
does not pre-unify (a field read, `h(s.c)`) is refused at `q`, the first
position (pins
`lang::types::callback_parameter_union_of_two_decided_arrays_{decided_by_a_later_position,
under_a_wider_later_position_refused}`,
`callback_field_read_union_parameter_before_its_decider_refused`). So a
call through an inferred wrapper,
a nested function or a seq step is refused where the direct call is, at
the check, never at the instance (pins
`lang::types::{wrapper,nested_function}_into_a_bound_union_of_two_undecided_refused`
and their twins, `empty_array_into_a_typedef_union_of_two_undecided_arrays_refused`,
`seq_step_empty_array_into_a_bound_union_after_its_deciders`,
`wrapper_argument_into_a_bound_union_beside_a_decided_member`,
`literal_into_the_second_member_of_a_named_union`).

A member that is a name or a bound cell standing for an open variable or
for a union holding one is its members first (`names_members`,
`Type::unfolded`): `['a, Id<'b>]` over `type Id<'x> = 'x` is `['a, 'b]`,
`['a, M<'b>]` over `type M<'x> = ['x, null]` is `['a, 'b, null]`, a cell
bound to `['x, 'y]` is `'x` and `'y`; a declared variable going into the
union meets it flat too (`set_commit_flat`), so `let g = |v: 'r| f(v)`
over `f = |x: ['a, Id<'b>]| ..` is refused where it is checked, not where
`g` is built at a type (pin
`lang::types::declared_variable_into_a_union_with_a_named_member_refused`).
What such a union must contain is met flat beside it (`Type::unfolded_held`):
each member that names a union or primitives as what it names
(`names_union`, a closed `Opt<i64>` too) and its primitives a type apiece,
so each part goes to the member that takes it and only what none takes to
an open one. `[Opt<'a>, string] ⊇ [Opt<i64>, string]` is `[null, 'a,
string] ⊇ [string, i64, null]`, which gives `'a` the `i64` alone;
`Opt<i64>` whole fits no member and would go to `'a`, breaking a bound on
it, and `[Opt<'a>, i64] ⊇ [i64, null, Opt<'a>]` would find no member for
`[i64, null]` (pins `lang::types::alias_beside_primitives`,
`bounded_variable_under_an_alias`, each beside its written-out twin).
A union written out with an open member takes a member of several
primitive types the same way (`place_members`, `place_covered`): where no
constructor member takes it whole, each type goes to the constructor
member that takes it and only the rest to the open member. `['a, null] ⊇
[i64, null, Array<i64>]` gives `'a` the `[i64, Array<i64>]`, so `x$` over
it selects with no null arm, and `'a: Number |x: [['a, null],
Array<i64>]|` takes that value as its alias spelling does. A member that
names a union still goes to the open member whole, under its name (the
part-at-a-time form of it cost a chain of bounds through a typedef its
linear time, and made two members of a named union compete for one
constructor member's variable by the typedefs' names). The variable is
then the narrowest type holding the argument, as from a union of
primitives alone: a later `y: 'a` widens it for a wider argument and is the
mismatch for one neither wider nor narrower (pins
`lang::types::covered_primitive_stays_out_of_the_open_member`,
`covered_primitive_leaves_a_bounded_open_member_its_numbers`, their
`_arm_refused` and `uncovered_` twins,
`open_member_beside_a_covered_primitive_*`,
`typ::contains::tests::an_open_member_takes_only_the_primitives_no_member_does`).
A rigid check never commits, so one that fails is asked again with such
members as their members, committing no bound of the name's parameters
(`Type::contains_unfolded`, under `ContainsFlags::RigidCheck` alone: every
other probe is followed by a commit, and scanning each failed one's union
cost a 100-arm select 2 G). So over a declared variable `[Opt<'a>, null]`
is `Opt<'a>` and `['a, null]`, bare and under a constructor or a named
type, and a recursive helper over `U<Opt<'a>>` (the tail's type of `type
U<'a> = [`Nil, `C('a, U<Opt<'a>>)]`) checks as its written-out twin does
(Eric 11-10, D29: a typedef names a union; pins
`lang::types::alias_over_a_declared_variable`,
`recursive_helper_over_an_alias`,
`recursive_helper_over_an_alias_in_an_impl`). An undecided rhs member takes a
union of undecided members whole, as it does alone (`['a, 'b] ⊇ 'c` binds
`'c`), and an equality relates such a union as the contained side first
(`equal_int`, `op::operand_type`: `u + 1` over `u: ['a, 'b]` decides
both), since what must hold it decides every member.

Two walks keep the first member that holds: a walk that binds copies
alone (`on_copies`, `place_in_order`'s trials, `contains_holding`,
`contains_through_copy`), whose answer the real walk judges again, and a
select's patterns (`contains_ordered`, `check_contains_ordered`: the
narrowing walk in `tvar.rs`, `pattern.rs`, `Select`'s predicate against
its scrutinee), whose arms are consulted top down. The members the
narrowing chooses among there are the arms' own, but for a written test
over a scrutinee two of whose members are cells still undecided: the
first would take it, so the arm is refused, naming the bindings to
annotate (`select.rs::written_takers`, `design/select_coverage.md`).
What a pattern leaves
in a binding's or a parameter's type is judged where a value next meets
it: `|x| select x { [h, t..] => h, [] => 0 }` types `x` as a union of
one array member an arm, `[Array<['e, 'e]>, Array<'e>]`. Its members hold
one element cell, so they are one type, and `f([4, 5])` gives the array
to them as one member, as the parameter written (`|x: Array<'e>|`) takes
it: no annotation. Two arms that bind elements (`[a, b, ..] => a, [c] =>
c, [] => 0`), a recursive function (`1 + len(t)`), a rest passed on
(`array::push(t, h)`), a list's `[<h, t..>]` and a slice inside a tuple or
a struct alike. The members are one type only where every cell is one,
and the arms' narrowing makes them one. An arm's bind holds its part of
every member the arm can match (`Type::narrow_per_member`); where one of
those parts is the bind's own cell, a member the arm's pattern gave the
scrutinee, the bind holds the others as that member's part does, so each
open cell among them is that cell and what is left binds it. A member is
read through a copy of the arm's predicate, but a cell the member holds
at the same part as the predicate, alone there, is not copied: it
is its own part of that member, and a copy, which carries the cell's
lower bounds, would merge back into it and leave them there twice, at
every arm and member (a part four arms bind over an undecided payload,
`B((2, x))`). A union of the one cell is that cell alone: a slice's
elements and rest, `Array<['e, 'e, 'e]>`, once an arm made them one. A
copy holds one cell for each open cell of the predicate, however many
forward links lead to it (a cell an earlier member's copy took forwards
to that copy's), so the links to a cell grow by the pairs narrowed,
never by a factor at each arm, and the check of a ladder of slice arms
costs what its arms hold (pins
`lang::types::slice_arms_over_an_unannotated_parameter_copy_no_chain_of_cells`,
units `narrowing_slice_arms_over_their_one_cell_copies_nothing`,
`narrowing_nested_slice_arms_copies_no_chain_of_links`). Once an arm
made the members one type the union still holds each of them, but the
walks over it take each way of holding the type once: a member walked
alike as one before it (`setops::walks_alike`: one type, a union's open
cells as a set and its other members in order) gives no part that one
did not, so the narrowing (`Type::narrow_per_member`) and an arm's
binds over its slices (`PatternNode::bind_narrowed`,
`StructPatternNode::hold_slice_members`) cost what one member holds, not
what every arm left (units
`narrowing_against_members_of_one_type_walks_one`,
`members_walked_as_one_before_are_dropped`). One type alone is not
enough, and the union is not cut to one member: over a union of two
members one type the narrowing makes the open cells among their parts
one, over one of them it does not (a lone member narrows by its
constructor), so an element union holding two arrays one type with each
other walks apart from one holding one, and a slice's rest beside an
array element takes the element's cell only over the members (pin
`lang::types::slice_rest_beside_an_array_element_behind_members_of_one_type`).
A cell that is
not copied stays open, with no link to a copy. A variable is ordered,
compared and hashed as the cell it stands for, the end of the forward
links merges left (`TVar::end_cell`), so which of two merged cells
forwards to the other tells no two unions apart, and a later input reads
the result of arms that give back their slices' parts (`[a, ..] => [a],
[] => []`) as it reads the written twin's (pins
`lang::repl_inputs::slice_arms_result_reads_as_the_written_twins_in_a_later_input`,
`forwarded_cell_stands_among_members_as_the_written_twins`,
`typ::tvar::tests::a_forwarded_var_orders_as_the_cell_it_stands_for`).
Members that differ only in open cells compare equal, so they stand in
the order the union was built in, which is not always the twin's: an
unannotated parameter the arms narrowed, read whole beside an arm's `[]`
(`select k { 0 => [], _ => p }`), stands first in either order of the
arms, where the twin's `Array<'e>` stands in the arms' order (pin
`lang::repl_inputs::narrowed_parameter_read_whole_stands_first_among_members`).
A cell an input's end settled to ⊥ sorts as ⊥ through its links too,
before a cell bound to a type, in a union a later input builds (`r.0`
over a union of tuples; pin
`settled_member_read_out_of_a_tuple_stands_as_the_one_met_whole`). What
a later read makes of the members' order is CR
value-members-one-cell-order-01. The
elements of the arms' slices share a cell that way, and so do the arms'
binds of another part of a tuple or a struct, however many arms bind it
(`([a, c, ..], b) => .., ([d], b) => .., ([], b) => ..`), literal arms'
(`(1, b) => .., (2, b) => .., (3, b) => .., (_, b) => ..`) and those of a
slice in each alternative of an or-arm (`` `A([h, ..]) | `B([h, ..]) => h
``). One cell is one type: no test of one part narrows another, so arms
that use the part as two types (`(2, b) => b + 1, (3, b) => str::len(b)`)
are refused where the function is defined. Arms that bind one part of
one variant tag (`` `A(1, a) => .., `A(2, a) => .. ``) stay no two
members: their predicates join as one variant over the union of their
cells, `` `A(i64, ['a0, 'a1]) ``, which holds each cell by itself. So a
bind whose cell is one of the cells of the union at its part of a tuple,
a struct or a variant narrows over those cells as it does over members
(`Type::binds_beside`, in `Type::narrow_parts`): the open ones are one
cell, and it takes what a bound one holds (a payload the scrutinee held
before the arms). A member of that union that is no cell is another
arm's test, and the bind takes it too wherever its values reach the
bind's arm: `` `A(1, a) => a + 1, `A(2, "x") => 0, _ => 0 `` is refused,
"cannot compute string + i64", since `` `A(1, "x") `` reaches the first
arm, as the tuple form is. What earlier unguarded arms cover whole is
left out of the union before the arm narrows
(`node::select::uncovered_parts`, over `Reach::covered`): the member
whose whole value at that part, the member's other parts as they reach
the arm, a cover takes (`` `A(null) => 0, `A(a) => a + 1 `` gives `a`
the cell alone). A cover is a type, so an earlier arm that tests a
literal at another part covers nothing there: `` `A(1, null) => 0,
`A(1, a) => a + 1 `` is refused, "cannot compute null + i64", as `(0,
null) => 0, (0, a) => a + 1` is. A guarded arm covers nothing. Pins:
`lang::select_undecided::payload_bind_*`.

A later arm's bind is typed by its own arm's narrowing alone. The
open-cell scrutinee holds that bind's cell as its member's part, and an
earlier arm narrows over a copy of it (`node::select::later_cells`,
`Type::cells_apart`): what the earlier narrowing makes of the copy
reaches the cell only where it is open cells alone (a slice's element
made one with another arm's, the cells of `` `A(i64, ['a0, 'a1]) ``
made one), and a type it decides for the copy is dropped. So the bind
takes its part of every member reaching its arm in any order of the
arms: `(1, 1) => .., (3, "x") => .., (4, v) => v + 1` is refused,
"cannot compute [i64, string] + i64", as it is with the bind's arm
first, and the arm's test passes every value of its shape. Pins:
`lang::select_undecided::bind_behind_*`.

A `_` at a position of a tuple, struct or variant where another arm of
the same shape binds a name is that name's twin over a scrutinee the
arms decide (an unannotated parameter, a `never()` let): its settle
reads the `_` as a cell of its own (`Type::wildcards_beside_binds`),
the cell its arm narrows (`Reach::settled`), so the part is what the
name's uses make it. `(1, _) => 0, (_, b) => b + 1` takes an `(i64,
i64)`, where the `_`'s `Any` typed `b` `Any`. The arm's run-time test
keeps the `Any`. A `_` with no name at its position in another arm,
a name only inside or over it, a slice's elements and a cell with
lower bounds (`*r`, an abstract's payload) keep `Any`. The twin's
refusals are the `_`'s: arms that use the part as two types, or a call
whose argument is wider than the part, name the part where they are
written. Pins `lang::select_undecided::wildcard_beside_a_bind_*` and
`wildcards_beside_binds_at_two_parts`, and the pins of the selects a
`_` beside a shape or a test keeps running
(`wildcard_beside_a_shape_*`, `wildcard_beside_a_literal_of_another_type`).

`contains` does not know bindings, so the refusal is a typed error the
relation's site names: a `<-` names its target (`declare u's type where it
is bound`, `Connect`), a call's argument the parameter
(`callsite::ambiguous_parameter`), and any other site gets the default
(`write the type of what holds that union`). A trait method's call and a
`.gxi` val's use check against the declaration alone and build an
instance of the implementation, so an implementation whose own type would
make that instance choose is refused where it is checked against the
declaration (`module::instance_picks_no_member`, from `traits::implements`
and `module::check_val`): `val f: fn(x: 'a) -> i64` implemented `|x: E<'a,
'b>|` over `type E<'p, 'q> = ['p, 'q]`. It judges as the build holds an
instance (below, An instance holds its signature cell by cell), so an
implementation whose variables stand at the declaration's places passes
(`val f: fn(x: E<'a, 'a>, ..)` implemented `|x: E<'a, 'b>, ..|`). Pins:
`lang::types::{write_into_a_union_of_two_undecided,argument_into_a_later_typedef_union,argument_into_an_identity_argument_union,argument_into_an_identity_body_union,decided_array_into_a_typedef_union_of_two_undecided_arrays}_refused*`
and their twins, `{typedef_union_of_variables,identity_member,option_member,nested_union_typedef}_refused_at_its_use`,
members that are one type by `lang::types::{slice_arms,slice_elements_bound_in_two_arms,slice_arms_of_a_recursive_function,slice_rest_passed_on,slice_arms_inside_a_tuple_and_a_struct,list_arms}_over_an_unannotated_parameter*`, each beside its written twin, and the unit test `members_that_are_one_type_take_a_type_as_one`,
a part several arms bind by `lang::types::{part_bound_in_three_slice_arms,part_bound_in_three_literal_arms,slice_in_each_alternative_of_an_or_arm}_over_an_unannotated_parameter`, `part_bound_in_three_{literal,slice}_arms_used_as_two_types_refused`, `part_bound_in_four_literal_arms_over_an_undecided_payload*` and the unit tests `narrowing_a_members_own_cell_{makes_the_open_parts_one,keeps_its_lower_bounds}`,
a variant's payload several arms bind by `lang::types::{payload_bound_in_two_*,nested_payload_bound_in_two_literal_arms_used_as_two_types_refused,payload_tested_by_one_arm_and_bound_by_the_next}` and the unit test `narrowing_a_cell_of_a_payload_union_makes_its_cells_one`,
`lang::traits::{trait_method_typedef_union_refused_at_the_call,default_body_typedef_union_refused,impl_method_typedef_union_refused}`,
`lang::interfaces::{open_union_call_refused,val_implementation_that_would_choose_a_member_refused,val_in_data_implementation_that_would_choose_a_member_refused,val_at_one_variable_implemented_at_two}`.

### Function types with variables of their own

A function type with type variables of its own stands only as a
declared signature's top: an interface `val`'s type, a trait method's
(and an impl's or a default body's method `let`, whose annotation is the
method's signature: `is_method_block`), a lambda's own declared
variables, a builtin's signature. Anywhere else in a written type it is
refused where the type is resolved from the AST, naming the fix:
declare the variables on the definition, or bind the generic function
with `let` and call it by name (`typ::QUANTIFIED_FN`). A written
`fn<'b: C>(..)` is one; so is a trait as a parameter's type inside a
written function type (`fn(c: Collection) -> i64`), which
`Type::rewrite_trait_args_inner` makes a quantifier, and a
constructor-trait quantifier (`ctor_quantifiers`). The positions refused:
a formal's type, a struct field, an element, a union member, a `let` or
`let rec` annotation, a typedef body, a function type's argument or
result, a reference's referent, an abstract type's representation or
parameter, a variant payload, `never<T>`, a bound
(`Type::rewrite_trait_args`), and a cast's target, a type test and a
catch's or a seq try's annotation (`Type::refuse_quantified_fns`). A
written `fn<..>` outside a signature's top is refused before its bounds
and parts are read, so a nest of them is refused in time linear in its
depth, and a trait as a type inside it or a hole in its bound reports
this refusal; a hole in its parts is refused first
(`Type::rewrite_written`). A generic operation passed around is a trait
(`|c: Collection| ..`, `'c: Collection`), or a definition's declared
variable that each call picks (`'a: Number |f: fn(x: 'a) -> 'a, x: 'a|
f(x)`). Pins: `lang::quantified` (each position refused, each kept
form, the refusal ahead of its bound's and parts'),
`lang::types::hole_in_a_quantified_fn_part`,
`lang::collection::collection_generic_formal_is_refused`,
`deep_nesting::a_nest_of_quantified_function_types_is_refused_in_linear_time`.

An interface `val`'s or trait method's own quantifiers are its
scheme's, and their bounds name any variable
(`Type::rewrite_declared_args`); so are an impl's method's and a
default body's. `FnType::contains_int` holds a scheme's own open
quantifiers rigid (`open_quantifier_gates`, generic cells only): what
the contained type is must hold for every type its bound admits. A
write to an earlier compile's generic binding holds its copy's
quantifiers rigid with its other cells (`node::held_cells`).

### Generalization

Every cell has a level (`tvar::Level`): the depth of the definition
that owns it and which definition that is (none at the top level), or
`GENERIC`, a scheme's variable no definition owns. Rémy's levels over
the definition gates:

- A cell is born at the current level (`tvar::AtLevel`): the top level
  in a statement's compile and check (`compile_top`, `check_and_fuse`);
  a definition compiles its signature and its gate checks its body one
  deeper, as its owner (`Level::definition`, `LambdaDef::level`; the
  signature's cells are claimed for it, `FnType::claim`). A cell written
  in source (the parser, the syntax codec), a quantifier minted for a
  trait argument or an impl head, and a let's compile-time scratch (the
  pattern predicate, the `let rec` placeholder) is born generic; a copy
  (`scope_refs`, `resolve_tvars`, `Fresh::Copy`) keeps the source
  cell's level. A variable a body's written type owns (a `let`
  annotation's or a `never<T>`'s, named by no definition around it:
  `lambda::body_annotation`) is `Level::Written`: generic, but lowered
  as any other cell.
- Binding a cell, merging two, or adding a conjunct lowers every cell
  the new contents reach to the cell's level (`tvar::lower`), the
  conjuncts and lower bounds a merge moves into the surviving cell
  included: what an outer cell is bound to or bounded by is the
  environment's. A generic cell is never
  lowered by a binding: a scheme bound into a cell (a let's name, a
  reference's placeholder) is still a scheme. A merge takes the lower of
  its two cells' levels, a generic survivor included (`TVar::merge_one`),
  so a scheme's copy that meets a live cell is that cell's: an array,
  list or map of composites, or a tuple holding a written name beside a
  holder's read is the name's one instance, where the name's cells meet
  the copy's. A struct update relates its replacement to nothing: each
  replaced field holds the replacement's own type, in every member of a
  union source, as a literal's field does (`data::updated_type`), so a
  written name it holds, one over `never()` included, stays that name's
  one instance, and an update of a declared variable's value is a
  struct, never the variable. What a
  merge may lower must never be a scheme's own cell, so a relation
  against a scheme instantiates it first: an omitting site outside a
  definition compiles its default among generic cells and judges a copy
  (`callsite::checked_default`), and a core trait's prototype calls an
  instance of its method (`Impl::build_prototypes`). Two function types
  are one union member when their cells pair up as a renaming, generic
  with generic or live with live; a scheme's copy never pairs with a live
  cell, which keeps that cell's instance (`setops::Identity::cells`). A
  union that collapses two members into one merges each pair of live
  cells the collapse paired, as ML unifies a conditional's branches
  (`setops::union_collapses`, applied once `Type::union` and its
  normalize are done): picking one of two structs or tuples of lambdas
  inside a definition calls the lambda of either arm at the type the
  call decides, and arms whose cells must differ (a written binding
  beside a generic holder used at another type) are refused at the
  check; generic pairs stay a renaming. Two declared variables of the
  definition under check never pair, since they cannot merge: `{g: |p:
  'a| p}` beside `{g: |q: 'b| q}` stays two members, and a call through
  them is refused, naming the members and the fix: one variable, or one
  written type, in both (`callsite::FnUnion`). Only that union pairs
  cells: a flattening that merges none (a normalize, a typedef's
  expansion, a substitution) keeps two function types apart unless they
  are identical cell by cell (`normalize::Members`), since dropping one
  would pick a member for every later relation: `E<fn(x: 'c) -> i64,
  fn(x: 'd) -> i64>` over `type E<'p, 'q> = ['p, 'q]` stays two members,
  which the choice below judges. A union standing inside a function
  type's signature is none of that union's alternatives, and its members
  stay as they stand there too (`normalize::NormCx::in_signature`): the
  cells a renaming would pair are the signature's own, which its calls
  decide, so merging them would equate two of its variables and dropping
  a generic member would type the call's result as the other. `|y: 'a,
  z: 'b, x: F2<'a, 'b>| x` over `type F2<'p, 'q> = [fn(q: 'p) -> i64,
  fn(q: 'q) -> i64]` read through a field, a tuple position or a select
  of two copies returns the union of both members, as a read of its
  binding does (pins `lang::types::field_read_keeps_a_signature_union_of_functions_over_its_variables`
  and its neighbours,
  `typ::normalize::tests::a_union_of_alternatives_keeps_a_signature_union_of_renamings`).
  One alternative joins nothing, however many times its node stands and
  whatever `⊥` stands beside it (`setops::sole`): it meets nothing and is
  its type as it stands, normalized with no pairing of cells. So a read
  of one value (a field, a tuple position, an error's payload, a
  dereference, a pattern's part, a select of one binding in every arm,
  `any` over one argument) is a read of its binding: the result of that
  `f` inside `|u, v, w| { let s = {fld: f(u, v, w)}; s.fld }` keeps both
  members and `u` and `v` two variables, and a call through the read is
  refused as a call of the binding is, naming the members. A read decides
  no variable: `f(1, |q: i64| q + 1).1` over `|x0: 'a, x1: F2<'a, 'b>|
  (x0, x1)`, whose `'b` no argument decides, is two functions, where
  `F2<'a, 'a>` is one (pins
  `lang::types::field_read_of_one_value_keeps_its_union_of_functions_over_two_variables`
  and its neighbours,
  `call_through_a_field_read_of_a_union_of_functions_over_two_variables_refused`,
  `call_through_a_tuple_read_of_a_union_of_functions_no_argument_decided_refused`,
  `typ::normalize::tests::a_union_of_one_alternative_keeps_its_union_of_renamings`).
  Nor does a read join two decided members: what `|c, a, b| select c {
  true => a, false => b }` returns over two functions of one type is the
  union of what its parameters took, two members at its binding's call
  and through a dereference, `any`, a select of it in every arm or an
  error's payload alike; `|c, a: 'a, b: 'a|` returns one function (pins
  `lang::types::call_through_a_deref_of_two_decided_functions_refused`
  and its neighbours, `call_through_reads_of_one_variable_s_two_functions`).
  A join pairs and collapses members of different alternatives only
  (`Type::union_tagged`; `setops::alternatives`): the members of one
  alternative, a select arm's union, the parts one pattern alternative
  takes from the members reaching it (`Type::union_exact_of_parts`,
  `TVar::narrow_per_member`), the elements a slice name reads
  (`Type::union_exact_of_members`), are one union whose members stay two
  however alike, collapsing only where identical as they stand
  (`setops::apart`, `Welds`). A member that two members of one other
  alternative could each meet meets neither (`Type::meet_open_fns`, the
  members' alternatives carried through `meet_members` and `meet_parts`),
  so the pairing never follows the members' order. A call through
  such a union is refused naming the variables, as a call of the
  binding is. The parts an or-pattern's alternatives take are
  alternatives, and meet. Pins
  `lang::types::a_select_of_a_union_of_functions_beside_null_keeps_both_members`
  and its neighbours,
  `typ::normalize::tests::a_join_of_alternatives_keeps_the_union_one_of_them_holds`.
  And
  a field read of a union meets the function members it reads
  (`data::struct_field_type`, `tuple_field_type`), as a select meets its
  arms (`Type::union_of_alternatives`), and so does a name the
  alternatives of an or-pattern share, or a whole capture, over its part
  of each member its arm matches (`PatternNode::wholes_by_member`, pins
  `lang::select::or_name_meets_its_functions`,
  `or_name_judges_every_default`): a member's generic cells meet
  through a copy, and its live cells bind, so a lambda literal in an arm
  is the instance the call through the union makes. Two members that are
  one signature but for the names of their variables, a scheme's cell
  beside a live one too, meet as that renaming: their cells weld pair by
  pair, each part beside its own image, and neither is contained in the
  other (`setops::renames`, `contains::renaming_linked`); two variables
  whose bounds differ pair too, and the weld merges both bounds into one cell
  (pins `lang::types::select_of_twins_of_a_signature_union_*`). So a union the
  signature holds over two of its variables (`x: P<'a, 'b>` over `type
  P<'p, 'q> = [Array<'p>, Array<'q>]`), whose members a containment
  cannot place while the variables are open, decides nothing and merges
  nothing where the copies meet: a select of two copies of a function,
  of the function and the same lambda written again, or of two structs or
  tuples that hold it, is called as its binding is, running what that
  runs and refusing what that refuses (pins
  `lang::types::select_of_copies_of_a_signature_union_under_one_constructor`
  and its neighbours,
  `select_of_a_twin_keeps_the_variables_of_a_signature_union_apart`,
  `select_of_a_twin_leaves_a_signature_union_of_functions_to_its_call_refused`).
  A member with cells
  of its own is one type with every member it meets, so one that meets
  two members it cannot both be (`|x| x` beside `|y| y + 1` and `|z|
  "[z]!"`) is refused where they meet, naming the three and the fix, the
  part of its type that tells the two apart, a parameter or what it
  throws (`setops::two_meets`): which of the two decided its
  cells would follow the order of the alternatives, and a union's
  members stand in the order of their sort. The refusal holds for the
  member that looks for its meets and for the one it finds, whose other
  meets the bind would end (pins
  `lang::types::field_read_refuses_a_lambda_two_members_could_decide`
  and its neighbours, `lang::select::or_name_refuses_a_function_two_could_decide`).
  The refusal names the two functions the member cannot both be, not
  the first it met (pin
  `lang::types::field_read_names_the_two_functions_that_are_not_one_type`).
  A member whose open cells are all a scheme's is an instance of the
  first member it meets, which decides nothing of it. The lambdas of a
  `let`'s literal are schemes where its elements meet, and an earlier
  one's instance that joins a later one leaves it that instance's cells,
  which no definition owns: it is a scheme's member still, and takes
  the first function it meets, in the order the elements are written. So
  `[|x| x, |w| w, |y: i64| y + 1, |z: string| "[z]!"]` checks in every
  order of its elements (pin
  `lang::types::literal_of_schemes_beside_two_functions_in_every_order`).
  A scheme that a member with cells of its own joins holds those cells
  from there on: beside `|w| { let e = w == v; w }`, which reads its
  definition's `v`, the same literal is refused in every order (pin
  `literal_refuses_a_capture_two_functions_could_decide_in_every_order`).
  A literal nested in another has met by then: `[[|x| x, |w| w], [|y:
  i64| y + 1], [|z: string| "[z]!"]]` holds one instance beside two
  functions, and is refused as an arm's lambda is. Members one value
  may be (`Type::shares_constructor`: arrays, lists, maps, references
  read-only or writable, tuples of one arity, structs of one field list,
  variants of one tag and arity) meet part by part the same way
  (`Type::meet_parts`), so a call through a part of the union judges
  every member's defaults and binds every member's lambda (pins
  `lang::functions::nested_array_defaults_of_two_types` and its
  neighbours, `select_of_mixed_references_defaults_of_two_types`). An
  alternative or a part that is itself a union meets as its members
  (`Type::meet_alternatives`), each union rebuilt of its met members:
  optional lambdas (`[fn(..), null]`) two arms hold are one function, and
  a lambda beside an optional callback is the callback's instance (pins
  `lang::functions::select_of_optional_defaults_of_two_types`,
  `lambda_beside_an_optional_callback` and their neighbours). Parts over
  nodes the walk has met, a bound cell read as the node it holds, take
  that meet while no cell has been written since (`setops::Meets`), so
  alternatives that share their parts meet once a node, not once a path,
  and each met member shares its parts as its alternative does (pins
  `typ::setops::tests::the_meet_of_two_pair_dags_takes_each_pair_once`,
  `deep_nesting::meets_over_two_pair_dags_are_linear`). Where one
  member must hold a value (a union bound passing its parts down to the
  value's open cells, `contains::commit_parts`; a union formal judging a
  function's hidden defaults, `hidden_defaults_at`), it is the one member
  that takes the value's constructor (`Type::takes_constructor_of`): a
  writable reference takes no read-only one. Two that take it are no
  choice for the hidden defaults, so neither is judged. A union bound
  goes further (`contains::member_with_parts`), for a function and for a
  value that holds a cell to decide: of the members that take it, the one
  a probe admits it into has the parts, or the one of those that holds
  the others. So a callback whose result decides between `fn(v: 'x) ->
  i64` and `fn(v: 'y) -> string` takes its parameter from that member
  (pins
  `lang::types::callback_into_{the_second_member_of_a_union_bound,a_union_bound_takes_the_members_parameter}`),
  `'a: [(i64, i64), (string, string)]` makes the `z` of `(z, 1)` an
  `i64`, and `[]` at `'a: P<'b, 'c>` (`type P<'x, 'y> = [Array<'x>,
  Array<'y>, null]`) is an array of what `'b` and `'c` were both decided
  to. Where two members admit a value that holds a cell to decide (`[z]`
  or `[]` at `'a: [Array<i64>, Array<string>]`, `&mut z` at `'a:
  [&[i64, null], &mut string]`, `|v| v` at two function members) and
  neither holds it whatever decides the cell, which member holds it
  follows that decision, later, and no bound on the cell says "one or the
  other": the call is refused, naming the binding to annotate
  (`contains::UndecidedTaker`, `callsite.rs::undecided_taker`), where the
  check used to accept it and the build refuse the instance. A trial, an
  ordered walk and a pre-unification pass the choice by; the argument's
  own check meets it. The members are what the bound's names and decided
  variables stand for (`'a: [A, B]` over `type A = Array<i64>`, `'a:
  [Array<i64>, 'b]` once an argument has decided `'b`), and an abstract
  member takes no structural value; where a member is a variable nothing
  has decided yet, no member is told apart here, and the bound's own
  walk gives a value that holds a cell to decide to the structural member
  before the variable, as a union written in a parameter does: `[z]` at
  `'a: [Array<i64>, 'b]` makes `z` an `i64` while `'b` is open, by rule
  and not by the members' order (`Type::set_commit`: constructor members
  before bare variables), and an array a `let` holds at `Array<string>`
  goes to `'b`; one written in the call, `["x"]`, meets the structural
  member at its pre-unification and is the mismatch (CR
  bound-union-binds-argument-early-01). So the parameters' order decides
  which of the two a call meets (D36; pins
  `lang::types::union_bound_an_open_variable_member_*`). A read-only
  reference
  member bounds a writable reference's referent (`'a: &[i64, null]` holds
  the `z` of `&mut z` within `[i64, null]`), a writable member's referent
  is the value's exactly (`'a: &mut [i64, null]` makes `z` `[i64,
  null]`), and so is an abstract member's parameter (`'a: B<[i64, null]>`
  makes the `z` of `B(z)` `[i64, null]`); a map passes its key down as it
  does its value (pins `lang::types::union_bound_*`, `reference_bound_*`,
  `writable_reference_bound_*`, `map_bound_*`, `abstract_bound_*`). A
  written
  cell is: an enclosing definition's parameter merged into `'n` in `let
  p: fn(y: 'n) -> Array<'n> = |y| [y, n]` makes `'n` that parameter,
  which every call of the enclosing definition's inner functions shares.
- A gate's close marks generic every cell its signature reaches at its
  depth or deeper (`FnType::generalize`). A cell a binding lowered
  above it (`let t = |x| x + y` gives `x` the top-level `y`'s cell)
  stays its environment's: `t` is monomorphic in it and each call types
  `y`.
- A call copies a cell that is generic or owned by a definition whose
  gate is not open (`Level::copied_by` the open gates
  `ExecCtx::rec_defs`; `FnType::instantiate`), at the current level,
  and shares every other: a top-level cell, and an open gate's (a
  parameter called through `((f))(v)` or `let g = f; g(v)`). A
  definition whose gate has not run (a submodule the interface declares
  first calls into its parent) is called at its declared scheme. A
  reference to a generalized binding copies the same cells, generic
  (`Fresh::Scheme`), so the call instantiates them. A call's own
  quantifiers are cells as any other: copied where their level says so,
  so a function value one instance holds keeps its quantifier one type.

A binding something writes is not generalized (the value restriction,
`CompileCtx::mark_connect_target`): every function its value holds, a
struct field's or a tuple element's included, is one instance its uses
and writers share (`CompileCtx::monomorphic`); a node the value's type
shares (two reads of one copy, `mk(g)` over `mk = |f| (f, f)`) is one
instance wherever it stands, as ML's weak variable is, so the instance
keeps the value's sharing and costs what its nodes do, not its paths
(a typedef application copied as its expansion copies each place its
body names apart, as a `val`'s type is written out; pin
`deep_nesting::written_holder_chains_check_in_linear_time`). The writer is the binding
the resolver names, whatever path or import names it. Within one
compile every statement compiles before any is checked, and a read
copies a scheme only as it is checked, so a write compiled before the
binding's check reaches it in time: the binding's check finds it a `<-`
target and takes its value as one instance (`Bind::typecheck0`: its
reads are its own, a single name's value is read as an instance, not a
scheme, and a name a pattern takes apart beside others takes an
instance of its part while the others keep the scheme,
`Bind::type_leaves`). A function body compiles later, as its definition
is checked, so a write in it would find a binding checked before it
already generic; a block therefore also collects the names its lets bind
that a `<-` or `&mut` in it writes, lambda bodies included, where no
nearer binding of the name stands between (`node::written_lets`), and
such a let is a `<-` target from its binding on and never generalized.
Outside a lambda body a write under a nested block's glob, which may
provide the name, is left to the resolver, which names it before the
binding's check. A module in the block is read for the names it may
write there (`node::module_writes`, one walk of each module's body per
compile, its own modules' writes folded in as it meets them, kept for
the blocks inside: `node::ModuleWrites`): a `super::` path that reaches
the block, a name it imports from it (a glob imports each name the
module does not bind itself), and, where the block is its module's top
block, a `package::` path or import through that module's path below
the package root (`package::a` at the root, a script's top level
included; `package::p::a` in module `p`). A `self::` write in a
module's top block names that block's binding of the name, past any
nearer one. A module's write names the nearest binding before it as a
block's does, so its own `let` of a name it imports writes nothing
outside. A write neither finds (a parent's `m::j` in a function body,
an alias of the module) is one compiled after the binding's check,
which is held as an earlier compile's is, below, and refused naming the
annotation, the binding's type on its `let` (pins `lang::quantified::
a_module_*`, `a_submodule_*`, `a_let_a_nested_block_*`,
`*_written_is_one_instance`, `a_parent_s_let_a_nested_glob_*`,
`lang::modules::module_writes_a_holder_name_*`,
`module_writing_its_own_let_over_*`, the shell's `script_scope`). A
binding generalized before a write to it compiles
(by an earlier input to the same runtime, the REPL or
`GXHandle::compile`, or earlier in this compile's check, for a function
body's write the walk did not find) had its uses checked against the
scheme, so it stays one: a write is checked against a copy
of it with every variable rigid (`Connect::typecheck0_with`), each
holding what its lower bounds hold (`|x| B(x)`'s `B<'c>` above `'x` takes
`|x| B(x)` again), and a
`&mut` of it is refused, since a reference cannot hold the variables
rigid.

A `let` nothing writes whose value holds a lambda literal or a
generalized binding's value in a data literal (a struct, its update, a
tuple, a variant, an array, a list, a map: `bind::holds_function`,
decided as the let compiles, before any definition in it is checked) is
generalized as a let of a lambda is
(`Env::poly_binds`): its type holds the closed definitions' signatures,
which a read copies, generic (`Type::scheme_read`, `Fresh::Scheme`), as
a reference to a generalized lambda copies its own. So an annotation of
the holder (`let t: {f: fn(x: i64) -> i64} = s`), a typed formal it is
passed to, or a site's settle of a bounded cell it reaches (`s.f(s)` over
`{f: |x| x == x}`) decides only that read's copy, and the definition's
scheme stays what its own check made it, which every instance is held to.
Every read in such a holder's own value, wherever it stands there (`let
x1 = {a: x0, b: id(x0), g: || x0}`: a call's argument, a block, a select
arm, a struct update, a lambda body, a nested holder's value), copies
through one cell map (`CompileCtx::holder_reads`, a `ReadCopies`, under
`bind::ValueReads::Shared`): two reads of one binding take one copy, and
a read of a type an earlier read made takes it as it is. The value is
typed as if it read each binding once, so what one part decides of a
copy (a call's typed formal) holds for every part (pin
`holder_value_reads_a_binding_once`), and a link of a chain
of holders holds one copy of the last: its type grows by a few nodes a
link, where a copy at each read would double it (pin
`lang::types::holder_chain_stays_a_dag`, which counts nodes and asserts
that two parts reading the last link are one type). An annotated `let`,
or one whose pattern binds a written name, takes a copy at each read
in its value (`ValueReads::Own`): what the annotation or the written
name's uses decide would otherwise hold for what the holder's other reads
share (pin `holder_destructured_beside_a_written_name`). A `let`
forwarding a generalized binding to a pattern that binds a written name
(`let (a, b) = h; a <- ..`) gives each name a copy of its own part
(`Bind::apart`, pins `holder_forwarded_beside_a_written_name`,
`holder_forwarded_through_a_partial_capture`), a whole bind (`all@ {p,
..}`) included: a capture stands over the whole value, never only the
fields its pattern names. A
module with no interface that writes such a name is not known as a
writer when the parent's `let` compiles, so its write decides the copy
the parent's other names share (CR module-holder-write-01). Two lambda
literals one array or list holds meet as its elements (`[|x| x, |y| y]`
is one function type, and so are `[g, g]` and `[s.g, s.g]`), and a `seqq` reads a holder once per request,
every step of the run sharing that read's copy. A lambda a let holds through a
select is not generalized, though a call through it instantiates it (CR
holder-block-select-01); one through a block's final expression is
("Generic only where a binding names it"). Pin
`lang::types::holder_reads_copy_its_signatures`.

An image carries each cell's level and each definition's depth.

### Settling

- At `CallSite::typecheck0`-end, constrained cells reachable from the
  return/throws but NOT from any argument (derived results) settle
  eagerly, before an annotation could narrow them — a freely
  narrowable derived cell would be unsound (`let r: f64 = f2(i64:1,
  i64:2)` accepted while the body computes i64). Because the witness
  is the conjunction's narrowest member, a derived cell whose def-time
  fact was `i64` settles to `i64`, not wide. Arg-reachable cells stay
  open through typecheck0: annotations narrow them and the actual
  arguments enforce the narrowing. So do the cells of the callee's
  own declared quantifiers (`'b: Any |s| -> Array<'b> []`): its check
  held them rigid, so its body produced nothing into them, and the
  caller's annotation or the terminal settle decides them. So does a
  cell rigid at the site: an enclosing definition's variable, which
  its own callers pick. A callback parameter's result or throws is one
  (`'b: [i64, null] |s: i64, f: fn(x: i64) -> 'b| -> 'b f(s)`): settled
  there it would bind the definition's variable to its bound, every
  call would return `[i64, null]`, and a trait method declaring the
  variable could not be implemented. Pins
  `lang::types::a_declared_variable_in_a_callbacks_{result,throws}_*`,
  `lang::traits::impl_method_bound_in_a_callback_s_result*`.
- An OMITTED labeled default is checked at its site by the check
  (`CallSite::check_omitted_defaults`): the default of every definition
  the callee's `lambda_ids` names is compiled as the site sees it,
  checked against the site's argument type, and discarded; elaboration
  compiles it again for the bind and finds the cells decided. When a
  definition is not known at the check (a fn-typed parameter, a
  runtime definition), the cells the omitted defaults reach are exempt
  from the site's settle and the default binds them at the bind. A
  function a definition's body returns names the definition its check
  made of the literal, and that definition stays registered with the
  bindings it captures until the enclosing definition is deleted
  (`BodyShared::held`, `LambdaDef::hold_returned`; only the node of a
  literal no definition encloses releases what its body holds, so the
  deletion of a body's check or of an instance of it releases nothing),
  so a later REPL
  input's call of the result finds its defaults: they are judged with
  those bindings put back for the judgment
  (`LambdaDef::restore_captures`), and a cell the earlier input settled
  is refused at that call, naming the binding. Pins
  `lang::repl_inputs::returned_default_*`,
  `lib_tests::leaks::returned_definitions_go_with_their_enclosing_definition`.
  Where the site's fn type hides the label, the bind's instance runs at a copy
  of the callee's signature fitted to the site's, and the default the
  bind compiled is checked over that copy's label, so the instance's
  body types see what it decides (`CallSite::instance_signature`; pin
  `lang::functions::default_hidden_by_a_fn_type_types_the_instance`).
- A default that holds a function is called through its label, so it
  is walked against the label as a written argument is against its
  formal (`check_hidden_defaults` from `callsite::checked_default`):
  the defaults of that function a call through the label may omit are
  judged there and narrow the label's variable, at every check that
  judges the outer default (the call that omits it, the fn type that
  hides it or makes it optional). `let f = |#w: fn(?#d: 'z, x: i64) ->
  string = |#d = 1, ..| .., x: i64| w(x); f(7)` holds `'z: i64`, and
  with a `y: 'z` given a string it is refused as `f(#w: |#d = 1, ..|
  .., "q", 7)` is, whether or not the body passes `#d`. The walk goes as
  deep as the defaults nest; a default that holds its own function
  (`let rec f = |#a: fn(b: i64) -> i64 = f, ..|`) reaches the judgment
  in progress, which is the one it gets (`CompileCtx::enter_default`
  in `judge_hidden_defaults`). Where the site's fn type hides the outer
  label, the bind walks the default it compiled over the fitted copy's
  label as well, so a body that calls what the held function returns
  (`w(0)(x)`) reads the label decided. Pins
  `lang::functions::default_holding_a_defaulted_fn` and its neighbours.
  Every judgment of a default compiles it again, and each compile's
  lambda links onto the label that holds it, so the walk takes the
  compiles of one lambda over one lexical snapshot and the same declared
  variables around it as one definition, the first compiled
  (`callsite::distinct_defs`,
  `LambdaDef::same_literal`), and walks a label with no open cell once
  for the function's own parameter and the function as the site holds
  it. Without either a chain of such defaults behind labels that hide
  them never finishes (pin
  `lang::functions::defaults_holding_defaulted_fns_twenty_deep_behind_labels_that_hide_them`).
- A function value's labeled default that a fn type hides or makes
  optional is judged where the value meets that type
  (`callsite::check_hidden_defaults`). A call's formal is the site's own
  instantiation, which the default narrows. So is the type of a binding
  something writes (its annotation, a `<-` target, a reference write's
  referent; `Narrowing::Written`): every reader reads that one type and
  the bind runs the function written last against it, so the default
  decides the label's variable where it is written (`let v: fn(?#d: 'q,
  x: i64) -> string = never(); v <- |#d = "q", ..| ..` holds `'q:
  string`; a later write of `#d = 1` and a call `v(#d: 1.5, 7)` are
  refused, and a parameter's `&mut fn(?#d: 'a, ..)` refuses a write
  whose default decides `'a`, "write the default's type in the binding's
  type"; pins `lang::functions::default_decides_a_written_bindings_label`
  and its neighbours). The write judges the functions the binding was
  born with beside the one written, in the order the program writes them
  (an earlier write's default has decided the label already): holding
  the written value links the two function types, so each names the
  other's definitions (`LambdaIds::link`), where the binding's function
  contains the written one and where a member of the binding's union is
  the written function's type, its cells welded where the two differ
  (`contains::Link`); two functions an array or a select met share
  their cells before the write and are linked by it all the same. So
  `let o = select j { 0 => |#d = 1, ..| .., _ =>
  null }; o <- |#d = "q", ..| ..` is refused at the write as `let o =
  |#d = 1, ..| ..` is, whatever the calls pass, "annotate the binding
  that holds them": `let o: [fn(?#d: [i64, string], x: i64) -> string,
  null]` (pins
  `lang::functions::default_of_another_type_written_beside_a_null` and
  its neighbours, `..written_after_an_array_met_them`,
  `typ::contains::tests::a_union_member_welded_to_a_function_names_its_definitions`,
  `a_union_member_that_is_a_function_names_its_definitions`).
  A declaration nothing writes (a `let`'s
  annotation, a declared return type) and a generic binding's copy at a
  `<-` are judged per function they hold
  (`callsite::Narrowing::Copy`): one that names the value's definitions,
  by the link `contains` left on it, is judged on a copy and decides
  nothing of the declaration, since every call through it judges the
  default again at its own view. That verdict is given while the
  declaration's variable is open, and the body's calls decide it after: a
  call that passes the label may make it a type the default does not fit
  (`let v: fn(?#d: 'q, x: i64) -> string = select j { 0 => |#d = 1, ..|
  .., _ => never() }; v(#d: "s", 7)` holds `'q: string` and never needs
  the default), and with no call it settles to bottom. So an instance,
  whose `let` reads the variable as its definition's check left it, takes
  the definition's verdict on such a function and judges nothing
  (`Narrowing::Instance`); the function a typedef names, below, it judges
  as it stands, as its definition did. Pins
  `lang::functions::default_under_a_bodys_annotation_no_call` and its
  neighbours. A function type a typedef's body writes
  names none: each expansion is a type of its own, so the reader's `w.cb`
  is a function no definition is known for, no call judges its default,
  and the bind would meet whatever the declaration's variable settled to.
  Such a function is judged as it stands: the default decides the
  declaration's open cell (`let w: W<'p> = {cb: |#d = 1, ..| ..}` holds a
  `W<i64>`), and a definition's declared variable, rigid in its check,
  refuses it, naming the annotation (`-> W<'z>` over `#d = 1`: "write the
  default's type in the declared type", `-> W<i64>`); a default of the
  variable's own type fits (`|z: 'z| -> W<'z> {cb: |#d = z, ..| ..}`).
  The typedef's body written out in the declaration keeps the per-call
  judgment. Pins `lang::functions::default_behind_a_typedef_return_type`
  and its neighbours.
- A trait method's declared return is read the same way: a call of the
  method takes the trait's signature, a copy that names no impl's
  function, so `T::mk(x).cb(7)` judges no default and the bind would
  meet the signature's variable as bottom. So where a method's value is
  held to its signature (`traits::implements`: an impl's method and a
  trait's default, whatever the value is, a lambda, a block or a
  binding's read), the functions its return holds are walked against the
  declared return as it is written (`callsite::check_method_return`,
  `Narrowing::Method`). The walk runs on a fresh copy of the signature,
  every variable rigid, and on a copy of the method's own return, so it
  decides nothing of the method's scheme or of the impl's variables (a
  merge with the impl's own cell would take the copy's level, and no
  call would copy that variable again). `trait T { val mk: fn(self) ->
  {cb: fn(?#d: 'z, x: i64) -> string} }` over `#d = 1` is refused,
  "write the default's type in the trait's signature" (`?#d: i64`),
  whatever the calls pass. A default of the signature's own variable
  fits (`fn(self, v: 'z) -> ..` over `#d = v`), and so does one whose
  type is its own to decide (`[]` under `Array<'z>`, `never()`). A
  default whose type holds a cell every use shares (it reads a binding
  nothing has decided yet) would tie that binding to the copy's
  variable, so under a label that holds a declared variable it is
  refused, naming the binding to annotate; under a written label it
  takes that type. A label the signature hides (`fn(self) -> fn(x: i64)
  -> i64` over `|#w: fn(?#d: Array<'q>, x: i64) -> i64 = .., x|`) holds
  the returned function's own variables, which the call that binds it
  picks with the default: a default there stands as it does in a
  function no trait declares, an undecided one included (the test is a
  rigid cell under the label, `Type::has_rigid`). The walk judges the
  return alone: a default of the
  method's own label is left to the call that omits it. Pins
  `lang::traits::method_return_default_under_a_declared_variable_refused`
  and its neighbours.
- The walk reaches a function wherever the two types give one at the
  same place: a part of a struct, tuple, variant, array, list, map or
  reference, a union's member built by the value's constructor, and what
  a function returns. A callback's returned function is judged at the
  call that takes the callback (`app(|j| |#d = 1, ..| ..)` over `f:
  fn(j: i64) -> fn(?#d: 'z, x: i64) -> string` holds `'z: i64`), since
  inside `app` the call `f(0)(7)` names no definition; a declaration over
  a function that returns a typedef's function (`-> fn(k: i64) -> W<'z>`,
  `let g: fn(k: i64) -> W<'p> = ..`) is judged as the typedef alone is.
  Pins `lang::functions::default_behind_a_callbacks_return` and its
  neighbours.
- An abstract type's constructor holds its argument through the type's
  representation, an expansion no reader of the payload names a
  definition by (`c.0` reads a fresh one), so `C(v)` judges `v`'s hidden
  defaults against the representation at the construction's own
  parameters (`data.rs::Construct`, `Narrowing::Constructed`). A default
  is a LOWER bound of the label's variable, as the argument is of the
  parameter: `C(|#d = 1, ..| ..)` over `Abstract<fn(?#d: 'z, x: i64) ->
  string>` is a `C<i64>` where nothing else decides it, a `let c:
  C<[i64, string]>` or a second write of a `#d = "q"` widens it, and a
  declaration that cannot hold the default refuses (`C<string>`; `->
  C<'z>`, "write the body's type, `-> C<i64>`"). Pins
  `lang::functions::default_behind_an_abstract_constructor` and its
  neighbours.
- The terminal settle (`FnType::settle_terminal`, the check's drain)
  walks the LIVE ftype in dependency order, the cells its bindings hold
  as positions included and every cell a chain of bounds reaches from
  them, however deep, each cell's binding walked once (pin
  `typ::settle::test::a_terminal_settle_decides_a_bound_of_a_bound`), and settles
  whatever remains. The cells a lower bound of any of them holds are
  walked too, and settle before the cell they bound: they are positions
  of the witness it settles to, so a payload's `Array<'_1>` leaves the
  site decided as a bound `Array<'_1>` does, and the build's unification
  of an instance's result into the site's meets no open cell the check
  left (an omitted default's exempt cell keeps its bounds' cells; pins
  `typ::settle::test::a_terminal_settle_decides_a_cell_a_lower_bound_holds`,
  `lang::types::undecided_payloads_met_in_a_select_build`). One cell
  does not settle before the cell whose bound holds it: a cell that
  stands above a type commits its conjuncts over it when it settles
  (above), which is where the cells they hold take their part, so a cell
  settled before that has none. A cell that would settle to ⊥ or to a
  type holding an open cell (it is open, above no type, and a ⊥ was
  produced into it or its conjuncts are no closed type) waits while a
  cell whose conjuncts reach it stands above a type, or itself waits for
  one that does (`settle::TerminalRun::waits`), and settles when the
  last of those has. Under `'a3: ['a4, null], 'a4: Array<'a5>, 'a5:
  string`, `f(never(), ["s"])` (`x5: 'a5`, `x3: 'a3`) puts `'a4` above
  `Array<string>`; `'a5`, fed ⊥ alone, waits for `'a4`, whose settle
  binds it `string`, where ⊥ first left `'a4`'s bound `Array<_>` and the
  call refused, "unsatisfiable constraints". So does a variable no
  argument holds: `'a0: ['a1, string], 'a1: ['a2, string]` given a
  `duration` settles `'a1` and then `'a2` there. Every other cell keeps
  the order: one with a type below it, and one whose conjuncts are
  closed once the cells they reach have settled, settle before their
  holder, which then holds them decided (`'a4: [Array<'a5>,
  Array<['a5, null]>], 'a5: string` above `Array<string>` tells its
  members apart only by a decided `'a5`; with a `never()` at `'a5` the
  members are undecided and the one that holds the other, `Array<['a5,
  null]>`, takes the argument and decides `'a5` as the other would,
  `contains::widest` above).
  Pins `typ::settle::test::a_terminal_settle_takes_a_cell_above_a_type_before_its_bounds_cells`,
  `lang::types::never_under_a_union_bound_*`,
  `unheld_variable{,s}_under_*`,
  `{closed_bound_under_a_union_bound,bound_closed_by_its_own_chain}_settles_before_its_holder`.
  A cell still
  unbound after the whole tc0 phase binds ⊥ (`TVar::settle_or_bottom`):
  nothing ever produced into it, and what bounds it (a trait, a
  predicate) has no witness, so a trait dispatch on it is a standing
  bottom; a cell bound `Concrete` or `Function` is refused instead
  (Eric 10-19; pin `lang::traits::dispatch_on_a_never_produced_self_is_bottom`).
- A site's settle leaves what an enclosing definition's signature
  reaches when the settle runs, not when the gate closed: an impl
  head's `'k` reaches a method's scheme only once the impl relates
  them, after the method's gate closed (`PendingSettle::Site::sigs`).
  It also leaves every quantifier of a function type its signature
  holds (`FnType::inner_quantifiers`): a generic function the scheme
  holds is its callers' to pick.
- The check decides every cell it creates: the drain runs the site
  settles, then the operator operand and `let`-over-⊥ settles, then the
  rules judged over them (`PendingSettle`). Elaboration never settles
  a cell an earlier compile task created (`tvar::earlier_task`): what
  the check left open, it decided open. An open quantifier of such a
  function type (a generic function value in `m: Array<fn<'a: Number>
  (..)>`) is any instance of itself to a later task: `FnType::
  contains_int` compares it as a fresh copy, held rigid on self's side
  and instantiated on t's, so neither side links it to the task's own
  cells; a top-level input's own check is the exception (below). A block's statements elaborate in parallel tasks that all read
  the check's cells, and a link made by one is seen by the others at
  whatever point they have reached.
- An input's end settles what the input leaves open. A REPL input (each
  statement of a line is one), a statement-at-a-time driver's statement,
  a script and a dynamic module's load are each checked and run alone,
  so their values are built at the types their end decides: every open
  top-level cell that the types of the bindings the input made a later
  input can name (its scope's, a lowering's excluded) and of its value
  reach settles as the terminal walk settles one (the lower bounds'
  witness, else ⊥, nothing of it having been built), and every top-level
  cell they reach is `CellFact::Settled` (`settle_input` in
  `compile_top` and `Module::compile_source`;
  `typ::settle::settle_input`, one walk over all the types). A scheme's
  cells stay open, as do a function type's own quantifiers and what
  their bounds reach: a call copies them. A binding inside a
  definition's body or an instance is reached only through what names
  it: an instance's own cells are its run-time binds'. Each top-level
  input, a load included, is a compile task of its own under `OwnWrites`
  (`Input` in `compile_top` and `Module::compile_source`), as a module's
  check is: a decision about a cell an earlier input made is refused and
  not made, so a refused input leaves every earlier cell as it was, and
  the refusal names the earlier bindings that hold the cell (a check
  that fails after trying one is told that first, `Input::refused`: the
  decision not made may be what it missed). What an earlier input left
  open, a function type's own quantifier included, is one type to the
  input's own check (`tvar::in_input`): `contains` compares no earlier
  quantifier there as a fresh copy (`FnType::has_earlier_quantifier`), so
  a use that would decide one is refused (`let fs = [g, g]`, `g` generic,
  then `let z: fn(x: i64, y: i64) -> i64 = fs[0]$`; the refusal names
  `fs`, not a type: annotate `fs`), while a read of a generalized binding
  (`let j = s.g`, then `j(1)`, `app(j)`) copies its scheme as always; a rigid gate it
  holds on an earlier input's cell records no write (`tvar::gated`:
  inputs run one at a time), so a write as general as the binding
  decides nothing. A cell
  below a settled ⊥ is linked to the settled cell (`contains`' right
  copy, `TVar::settled_bottom`): a function a later input defines that
  writes an earlier binding is held to the binding's type, so `let y =
  never()`, then `let p = |x| { y <- x; x }` makes `p` take `_` and
  refuses `p(1)`. A refusal a settled ⊥ causes, on either side (a type
  mismatch or a comparison's operands where the settled part is what
  fails, `typ::tvar::settled_cause`; a select's coverage, a load's val
  check), names the binding a later input can name that holds it, the
  first made, with where it is declared (`typ::tvar::settled_note`). A write the binding cannot hold is told the declaration that
  holds it, the binding's type with each settled ⊥ written as what the
  write puts there (`Type::fill_settled`: `let e = []`, then `e <- [1]`,
  is told `declare e: Array<i64>`). A load's check of its signature
  reopens no settled cell (`Type::unbind_tvars`), and one that would
  reopen any other cell of the program's is refused, as any decision is.
  A declaration site
  a refusal names is printed one way (`typ::tvar::BoundAt`): the line and
  column, and the file, module or REPL input's text where that is not
  the refused input's. Pins: `lang::repl_inputs`.
- ⊥ never binds a cell: the `(TVar-unbound, ⊥)` arm of `contains` is
  no-bind (⊥ is contained by whatever the cell may become, so binding
  gains nothing and forecloses the cell's writers), and `flatten_set`
  drops ⊥ members (an all-⊥ set is ⊥). The eager-bind variant was
  tried first and broke both `f(never(), i64:5)` and the connect-seed
  idiom. `never()` is syntax typed the literal ⊥ at compile time; the
  connect-seed idiom (`let res = never(); res <- v`) lives in the
  binding: an unannotated `let` over a ⊥ initializer seeds a fresh
  cell, writers refine it at their tc0, and the check's settle
  (`PendingSettle::LetOverBottom`) binds a cell nobody refined to ⊥.
- An operator's bound (`Number` for arithmetic, an integer for an
  index) narrows an open cell of its operand, alone or a member of a
  union (`[i64, 'x]`), and checks only the known parts
  (`op::constrain_operand`); it never binds the cell to the bound, which
  would claim every type the bound admits at once.
- A cell remembers that ⊥ reached it while open (`CellFact::BottomFed`,
  set by that `contains` arm, merged by aliasing, copied at
  instantiation like the conjuncts). Every other production binds a
  cell, so a ⊥-fed cell still open at a settle after its writers binds
  ⊥ whatever conjuncts its readers gave it (`TVar::settle`: an
  operator's operand settle, the `let`-over-⊥ settle, the terminal walk):
  `let t = never(); let u = never(); t + u` and `g(true) + { let t =
  g(false); t }` with `g = |b| never()` are accepted as
  `g(true) + g(false)` is. The `CallSite::typecheck0` eager settle takes
  only the witness (`settle_witness`): the cell's writers are not
  checked yet.

### Undecided types at a select

A select's coverage and its arms' run-time tests are decided where the
select is checked, so a type still open there (a `never()` let written
after it, a parameter nothing has bound yet) is one unknown type, equal
only to itself, whatever the program decides later (Eric, 10-31 and
11-03, D5 (b)). Once the arms have decided what they will (Reach::new's
settle, the narrowing, the bodies), `Select::check_undecided` walks each
arm's pattern over what reaches it (`Reach`'s type before the arm: a
member an earlier unguarded arm took whole, by a tag at another
position, meets no later arm; that arm passed the rule itself) with
`StructurePattern::meets_undecided`,
the walk the declared-variable rule takes (`meets`, met over open cells
instead of rigid ones): a pattern other than a name or `_` standing over
a value of such a cell is refused, naming the binding whose type it is
(`Holders`), else the nearest one whose type holds it
(`Holders::name_part`). Neither is ever a binding whose type no source
can write: a lowering's, or the cell of a reference's referent that no
binding names (`env::Binder::Referent`), which a binding holding the
reference declares (``let r: &mut ([`J, `M], i64) = &mut (x, 0)``; `x`
annotated leaves the referent's cell open). Every hint names such a
cell after the first binding whose type is a reference to it, `'r` "the
type `r` refers to" (`Holders::name_cell`, where no binding's type is
the cell itself). The cells counted are those
the scrutinee's type held before the arms ran (`settle::position_cells`),
still open at the end: a cell an arm's own pattern brought, a bind's or
a `_`'s, which the arms' settle put into an open scrutinee, is the arms'
decision and not counted. A written test is judged over its own type,
where any cell open before the arms is its own variable nothing
declares, met as the open cell it was where the narrowing against what
reaches the arm bound it (`Array<['q, `K(i64)]>` over an array of `[i64, `K(i64)]` binds `'q`,
over `Any` to `Any`: `Deciding::hold_written`, `meets_decided`, so a
destructure that takes apart all it bound passes), and over what
reaches it: it
tests an undecided cell of a member when the member passes the test
with the cells copied (whatever they become) but not with its undecided
cells held rigid together, each one type of its own
(`written_tests_undecided`: two `contains_holding` per member holding
such a cell, more only to name the cell a refusal names): else
`` (`J, [i64, string]) as (`J, s) `` would take `` (`J, z) `` out of what
reaches the later arms, deciding nothing of `z`, and a later `(_, n)`
would meet no value of it. The cost is one walk per arm, plus for a
written test one containment per member reaching it that holds an
undecided cell.

An arm's pattern may decide such a cell instead of leaving it open: the
arms' settle (`Reach::new`) binds an open part inside a structure to the
first arm's predicate, a partial struct pattern completes against a
sibling member, a literal or a tag narrows it. Nothing is open at the
end then, and a later arm is refused as dead or a later write as
mismatched, naming nothing the program wrote. So the cells the scrutinee
held open are noted before each step that runs a pattern (the settle and
completion before the arms, each arm's completion and narrowing; never a
guard or a body), and a cell such a step bound counts as undecided
(`Deciding`, Eric 11-05, D5 follow-up (b)): `meets_decided` walks it as
the open cell it was (`union_members_stopping` stops at it), and a
written test is judged by `written_tests_undecided` over a copy of what
reaches it with those cells open again (`written_tests_decided`). An
arm the coverage finds dead (`Reach::arm`), which an earlier arm's
decision may have made so, first has the arms up to it judged by this
rule, so the message names the binding; any other error in an arm's
check, its pattern's, guard's or body's, is that error, and no arm is
judged. A whole
scrutinee that is one open cell is not counted: the arms decide it
together, as `select x { `J => 1, `M => 2 }` makes `x` a `` [`J, `M] ``;
except where an arm holds a partial struct pattern, whose completion
against an open cell is the struct of the fields it names, a type the
program never wrote.

A decision may rewrite a binding that held the cell: `y` bound to `['x,
`J]` becomes `` `J `` once `(`J, _)` decides `'x`. So a bound cell of
the scrutinee whose binding holds an open cell is noted with that binding,
and one a step rewrote is walked as it was (`Decisions::held`), where the
decided `'x` stands beside the member that holds the literal's type. The
binding is kept under the holder's own cell, never the cell the holder
stands for after the rewrite: `['x, 'n]`, `'n` a bind's cell, becomes
`'n`, a member of the binding it replaces, and a walk that read `'n` as
that binding would not end.
Coverage (`Reach::arm`) is a pattern step too.

An arm that only takes a decided part apart is no test of it
(`StructurePattern::only_destructures`, Eric 11-05, D5 follow-up 2 (b)):
a tuple, a struct naming every field, a slice or list with a rest, an
abstract payload, holding only names, `_` and such patterns, each part of
the shape of every member of the part as decided but an open cell there,
the arm's own, which takes the shape (`destructures`: `[(a, b), ..]`
takes apart an element of `['r, ('a, 'b)]`). It
decides the part's shape as an annotation would and tests nothing of it:
a later arm meets the part as decided, so one written for the part's
other values is unreachable, as under the annotation, and so is one of
another shape. A literal, a tag, `null`, `[]`, an
or-pattern, a written test, a partial struct (its completion is the
struct of the fields it names) and a slice of a fixed length choose among
the part's values, and stay refused: a fixed length is a choice among
lengths as a literal is among values. A rest leaves the lengths it does
not take to a name or `_` in a later arm, or to the coverage; a later
`[]` chooses a length and is refused.

Beside a member of its shape a destructure chooses between that member
and the part (`((a, b), _)` over a `y` that is `'x` or `(i64, i64)`,
`(W(a), _)` beside `W`, a ladder of such arms, `([a, ..], _)` beside an
array), so it is a test of the part, refused whether the part stays open
after the arms or the coverage merges it into the member. It is refused
too where a later arm is found dead first: the dead arm's refusal judges
the arms up to it, by what reaches each, against the cells still open
(`still_open`) and those the arms' patterns decided.

What only names and `_` meet passes, and stays sound two ways. A name
over a union position where one member's part is an open cell takes
that cell beside the other members' parts (`TVar::narrow_per_member`:
a copy the narrowing left open took the member's cell), so a body that
decides the name decides the cell, and a later write to it apart is
refused at the write. And exhaustiveness is judged first with the
undecided cells held rigid on copies (`Type::contains_holding`), where
the product distribution law applies, since a rigid cell is a constant
to it; only where that fails does the committing cover run, which may
decide them as the arms need.

A slice's rest is the one cell of the arms' own that is a member of a
type they test: over a scrutinee the arms decide (`let v = never();
select v { [`J(n), ..] => n + 1, _ => 3 }`) the element type is
`` [`J('n), 'rest] ``, the rest left for a later write, and the slice's
run-time test passes a value of whatever the rest becomes at every
position. An element's structure takes such a value by its tags, arities
and literals alone, so a member the rest came to hold that the structure
admits and the element's type does not hold (`v <- [`J("s"), `K]` after
the body made `n` an `i64`: the write puts `` `J(string) `` in the rest)
would reach the binds as a type they were never typed over. Each element
of a slice that is not a name or `_`, standing over an element type with
a member still open where its arm is checked, is therefore judged again
at the statement's settle (`PendingSettle::ElementBinds`, `lib.rs::
element_binds`): for each member of the element type as settled that the
element's structure admits (`StructPatternNode::footprint` of the
element's own type: every position it does not test `Any`, an abstract
pattern every instance of its type), the element's type, whose cells are
its binds', must hold the member, or the part of it the structure takes
(`Type::matching_type`), every cell still open held as one type of its
own (`Type::contains_holding`); else the pattern is refused, naming the
member: annotate the scrutinee where it is built, which types the binds
over every member (`n: [i64, string]`). A member the structure cannot
take (`` `K ``, a tuple of another arity, a part that cannot hold a
literal the element tests) passes, and so does one the write put in the
element's own type, a bind still undecided at the write taking its part
(`[`J(n), ..] => "[n]"`). The open member may be a cell the scrutinee
held that a destructuring element left open, too (`[(a, b), ..] => a + 1`
over `[e]`, then `e <- ("s", 1)`). A member still open at the settle (a
rest nothing wrote, a declared variable a write brought) is read as what
it may become (`lib.rs::unheld_member`). Each member of a bound of its
that is a type (`TVar::typed_bounds`: `'a: [`J(string), `K]` brings
`` `J(string) `` with `v <- x` over `x: Array<'a>`) is judged as a
settled member is, and a cell with several such bounds passes where one
of them does, since each holds every value of it. One with no such bound
may become any type, so it is refused unless the element's type holds
everything its structure admits (a pattern that binds nothing, an
abstract type without parameters). The runtime-form judgment of the same
test (`PendingSettle::Told`), deferred before this one, refuses most of
those first ("may share ..'s runtime form"); an abstract type's test
shares no other type's form, so `[W(p), ..]` beside an open member is
refused here alone. A bind still open at the settle is held as one type
of its own there too: in a function it would be a variable of the
signature, which a call decides apart from the value the element takes.
Pins:
`lang::select_undecided::slice_element_*`, `list_element_*`,
`slice_suffix_element_*`; the skill's `select.md` cases.

A partial struct pattern's completion is the arm's run-time test, so
members its left-out fields tell apart only by undecided cells are two
members (`strictly_identical`), refused as any ambiguous completion is,
unless what reaches the arm leaves one: annotate the cells, the member,
or name every field; a program whose cells end up equal is refused until
annotated. A destructuring `let` and a parameter complete their pattern
alike and refuse it alike, each in its own words ("at this let",
annotated `let {a, ..}: T = ..`; "at this parameter"), the remedy written
over the whole pattern its holder holds where the refused one is a part
of it (`let ({a, ..}, n): T = ..`, `T as ({a, ..}, n)`). The refusal
writes the two members as a program can (`pattern::members_refusal`,
which every completion site calls with its `Holder`): an undecided cell
as `'x` for the binding it waits for, one a let's annotation wrote as
its variable (`'q`), and one no binding names (the element of a `[]`, a
read through a reference to an unnamed expression, a call's result)
numbered, `'_1`, `'_2`, one number a cell (`Holders::number`), told as
"types no binding names, annotated where their values are built". A
call's copy of a variable its callee declares is one no binding names,
though it carries the callee's name (`TCell::naming`): two calls of
`|x: 'a| -> Array<'a> []` give `Array<'_1>` and `Array<'_2>`, not
`Array<'a>` twice, which reads as one type. A let's annotation that took
the copies names them (`'q`, `'r`) through every handle, the copies' own
included.

Two members decided apart (`Apart::Decided`) are refused in the same
holder's words: a select asks for the member (`T as {a, ..}`); a let and a
parameter take every member at once, so they name every field where the
two members share their fields and say to bind the value whole and take it
apart in a select where they do not (pins
`lang::tuples_structs::*_over_members_*`).

### Unfolding a pair of definitions

`Type::diff` (what reaches the next arm) and `could_match` walk two
named types into their definitions. A pair met again inside its own walk
is answered by assumption: diff subtracts nothing there (the residual is
the minuend), could_match answers an overlap that rests on the pair in
progress (`Rests`). Keyed by the types themselves, a walk that subtracts
a union member by member meets a new key at every level and does not end, and the exact
difference over Error cycles of coprime lengths repeats only after their
lcm. So a pair of two named types is also keyed by its two definitions,
arguments ignored (`setops::DefPair`), and a path may hold
`setops::UNFOLDINGS` (one) unfoldings of it (`Unfoldings`; Eric 11-05,
diff-cycle-hang-01 (b)). A pair with a side that is no named type is not
counted: that side walks into its own parts, smaller at every step, until
it meets a name, so a pattern written as a type, an alias or the
scrutinee's own recursive name in it included, is walked as deep as it
goes and a tag it cannot hold is found dead however deep it sits.

Within that bound, unions that branch at every level still multiply the
walk's paths: a family of `n` definitions `T_i = [Error<T_i+1>,
Error<T_i+2>, `A_i]` gives an exponential number of pairs. So one walk
unfolds at most `setops::WALK_PAIRS` (1000) pairs, a side of each a named
type, counted, never timed (Eric 11-05, diff-cycle-hang-01 follow-up 2
(b)), past which diff subtracts nothing and could_match answers an overlap
wherever a pair is met, beside the pair that spent the budget too, so a dead
arm is accepted there (pins
`typ::setops::tests::diff_over_branching_unions_is_bounded`,
`could_match_over_branching_unions_is_bounded`,
`typ::matches::tests::could_match_answers_an_overlap_past_the_pair_budget`).
A residual is a DAG:
over `T = [Error<T>, Error<U>, `A]`, `U` alike, a test nested `d` deep
leaves `2^d` paths through `d` shared levels. diff keeps the difference
of each finished pair whose walk was cut only at itself or below it (a
pair in progress, `Unfoldings`), a pair with a named side by its
`RefHist::ref_id`s, a pair of shared unions or of one constructor by
their nodes' identities while no cell is written, each with what its
walk entered or was cut at (`DiffHist`, `Reached`). A kept difference is
read again only where the path holds none of that: a fresh walk there
would be cut nowhere it was not, so the memos change no answer (pins
`memos_change_no_answer_over_shared_residuals`,
`memos_change_no_answer_over_shared_parts`). The pair `U - T`,
whose walk meets `T - U` and comes back to itself, is kept, so each
level of the residual is subtracted once: the next arm's walk takes
pairs linear in `d` and stays exact, and a repeated `Error<U> as e` arm
after the deep test is dead at any depth (pins
`diff_of_a_shared_residual_walks_each_part_once`,
`lang::select::dead_arm_after_a_deep_test_is_refused`;
diff-residual-depth-01).

A residual is built of the names it came from. A named minuend, a union
or an error by its definition (the two a difference builds anew), whose
difference is that definition, nothing taken, is answered as the name, at
any depth, and a union member taken whole leaves no empty member beside
the rest. Unfolded instead, a name is a new type at every level
it recurs through (`Error<[T0, `A1]> - Error<U1>` over `T0 = [Error<[T0,
`A1]>, Error<T0>, `A0]`), no pair is met again and the walk spends its
whole budget on a residual 1000 levels deep, which every later walk then
reads (pins `typ::setops::tests::diff_keeps_a_name_it_takes_nothing_from`,
17 pairs for 1000; `deep_nesting::
a_type_test_over_two_recursive_unions_checks_in_time`).

A residual nests few Errors over a recursion. `Error<T> - Error<U>` keeps
the Error around what is left of its payload, and a pair met again answers
its minuend whole, wider than what is left there. The next subtraction
from that residual (the subtrahend's next member, the next arm) unfolds
the minuend again beneath the Errors already built, so each return through
the recursion wraps what the levels below left in one more Error: a new
type at every pair, which no key meets again, a walk that ends only at its
budget, and a residual a thousand Errors deep whose paths every later
walk, and the refusal's message, read one by one. So a walk knows what the
difference it is building holds (`DiffHist::over_cut`): a minuend answered
whole (for a pair met again, past the budget, or a collection or an Error
kept whole over one), and how many Errors it built around that minuend. An
Error whose payload's difference holds one beneath
`setops::ERRORS_OVER_CUT` (three) built Errors keeps its payload whole:
wider, and sound as below. A part of which nothing is left holds nothing,
so a difference that is exact through a recursion (`U - T` above, each
level of a deep test) is built as deep as the test is written. Three was
chosen from 2600 generated selects (gen_rnd.py seeds 1..1600 of review-r8
X-r1-1, gen_sel.py seeds 1..1000 of round 20): main's six timeouts end,
none of its other verdicts is lost, and 13 dead arms its budget hid are
found (a catch-all after an arm that covers the scrutinee, a test an
earlier arm holds), for 2087G instructions where main takes 2387G on the
files it finishes. One Error costs 2080G and loses five of main's
dead-arm refusals, two lose six, four lose two. The count is a heuristic:
over those and 2200 selects generated after (gen_sel.py seeds 1001..1600,
gen_rnd.py 1601..2400, 800 of a generator that nests Errors to four and
writes up to 14 arms) three ends all 22 of main's timeouts, finds 60 dead
arms and loses four of main's dead-arm refusals (`Et as x0 => .., Error<At>
as x1 => ..` with `Error<At>` a member of `Et`: what the first arm leaves
keeps an Error whole that the second test meets); four finds 53 and loses
four, six finds 33 and loses three; none refuses a missing case in a
select main accepts (pins
`typ::setops::tests::a_residual_nests_few_errors_over_a_pair_met_again`,
`residuals_over_two_families_of_recursive_unions_stay_shallow`,
`diff_over_names_recurring_through_errors_ends`, 58 pairs for 1000;
`deep_nesting::
type_tests_over_two_families_of_recursive_unions_check_in_time`).

A wider residual is sound: fewer dead arms are found, a missing case may
be refused that a `_` arm fixes, a later arm's bind is wider. It is not
sound for a caller that reads what a difference left out: `?` and `$`
pick their strip by whether `t - (t - error)` holds a member, and `?`
raises that part to its handler, so a widened inner difference would
read as nothing taken. `Type::diff_exact` says where the budget widened a
difference (pin `typ::setops::tests::diff_exact_past_the_budget_is_none`).
There `?` and `$` split the operand by its members instead, read through
cells and through names, each name expanded once per parameter list,
whose open cells are told apart by identity (`F<'a>` and `F<'b>` are two)
(`Type::dispatch_members`): an error member (or the null) is what the
strip takes, which decides the strip and is the raised type, and the rest
is the result type (`Strip::taken`, `Strip::left`; pins
`lang::errors::an_operand_past_the_pair_budget_*`,
`one_name_behind_a_wrapper_past_the_pair_budget_*`).
One unfolding and
1000 pairs were chosen from the 1600 generated selects (review-r8
X-r1-1): of main's 1547 finishes none changes verdict or the kind of its
message, 52 of its 53 timeouts end, the last in matching_type's
containment; 100 pairs moved two of main's verdicts (a dead arm accepted, a
dead-arm refusal become a missing case), 10000 pairs cost a generated
select (rnd13) 3.4G instructions where 1000 cost 1.7G. Over Error cycles
of coprime lengths diff walks pairs that grow with the lengths, not their
lcm (pin
`typ::setops::tests::diff_over_error_cycles_walks_pairs_by_their_lengths`).

An operand that is an open cell, or a union with an open member and no decided
member that holds an error, is refused (`Strip::of`): what the member later holds
would decide the strip, the null form or the error form, after it was taken
(pins `lang::types::strip_over_*`). Beside a decided error member the error form
is the strip whatever the open member becomes, as errors come off first. A `?`
over such an operand is refused when the open member may itself hold an error
(no `NoError` conjunct): the handler a raise joins into is fixed where the `?` is
written, and an error the member later holds would be raised past it. A declared
variable is told to declare `'a: NoError`, an undecided cell to be annotated
(`Strip::open_member_raising_nothing`; pins `lang::types::qop_over_*`). `$`
raises nothing and keeps the error form.

### Containment over a pair of definitions

`contains` answers a pair of named types met again inside its own walk by
assumption: it holds (`ContainsHist::assuming`). A verdict reached under
an assumption still in progress is the pair's only while that assumption
stands, so it is kept provisionally (`Provisional`, with what it rests
on, `Rests`): dropped when an assumption it may rest on is answered
otherwise, kept for good once each was answered as assumed. A verdict that
reads a provisional one rests on what that one rests on (pin
`typ::contains::tests::a_kept_verdict_rests_on_what_it_read`). A probe's
verdict is kept with the count of cell writes it was reached at and read
only while the count stands; a commit's is kept for the rest of the walk,
since what its proof bound stays bound, and only where it holds. A write
outdates the probes kept for good before it, and a pair answered again
since is read from the provisional verdicts while it rests on an
assumption, so a commit that binds cells walks a pair again after a
write, not at every path to it (pins
`typ::contains::tests::a_kept_verdict_is_read_past_an_outdated_one`,
`lang::types::open_applications_of_mutually_recursive_typedefs_check`).

A verdict is keyed by its two nodes. A name applied over a cell
(`ContainsHist::open_app`) has no node to key by: its expansion holds the
cell, so every visit builds it, and the applications inside it, anew. It
is keyed by its definition and its parameters' content, a cell by its
identity (`ContainsHist::pair_key`, `Type::param_key`), and the walk
holds each such cell so no other takes its address (`TypeRef::key_cells`).
So the applications of a recursive definition over cells that stay open,
each reaching every other (`type D<'a, 'b> = [`N, `X(fn(x: 'a) -> i64,
D<'b, 'a>), `Y(D<'b, 'a>)]` against an identical `F<Any, Any>`: one
application an order of the cells), are each walked once by a probe, and
by a commit once per trial a union with two open members runs on copies,
where a walk that forgets a pair once it is answered takes every path
through them (pins
`typ::contains::tests::open_applications_of_a_recursive_definition_are_walked_once`,
`a_kept_verdict_over_open_applications_is_the_pairs_own`,
`an_application_key_holds_its_cells`,
`lang::types::open_applications_of_recursive_typedefs_check`).

### Two applications of one definition

`T<a> ⊇ T<b>` over one definition is answered from the arguments, a
question a parameter, where their expansions ask no more
(`Type::applications_hold`). What they ask of a pair of arguments is how
the definition's body meets the parameter (`typ::variance::meets`, kept
by the definition, `ResolvedRef::meets`): under constructors alone `a ⊇
b`; under a function's argument `b ⊇ a` with the sides swapped
(`ContainsFlags::Contra`); under a writable reference or an abstract
type's parameter both ways; at two positions, each one's question;
nowhere, nothing (`type P<'a> = [`N, `C(P<'a>)]`). A parameter passed
to another application is met as that definition meets its own, and
one a recursion only passes on to itself is met nowhere; the scan is a
fixpoint over the definitions the body reaches, so a recursion that
permutes its parameters meets each as every position it reaches does.

The arguments stand for the expansions only where the walk over them
has no choice to make. A parameter is left to the walk
(`Meets::walk`), and its arguments must then be one type or the two
applications are walked as two definitions are, where it stands:

- in a union two of whose members may take one value and one of them
  holds a parameter (a bare parameter, `['x, null]`; two members of one
  constructor, `[Array<'x>, Array<i64>]`): which member takes a value
  decides what its cells become. Members are told apart by their
  outermost constructors, a named member by its body's; variants by
  tag and arity, tuples by arity, structs by their fields;
- as an argument to a bounded parameter, whose bound each expansion
  holds, or to a name that is a trait or has another arity;
- under a quantified function type or a constructor application
  (`Type::App`);
- as the payload of core's `ErrChain`, whose application is not its
  body over its argument: a chain of a chain is that chain
  (`Type::lookup_ref`), so `ErrChain<`A>` holds `ErrChain<ErrChain<`A>>`;
- past a scan of 65536 steps (`variance::BUDGET`), every parameter.

A name its body reaches that is not defined yet gives no answer, kept
by no definition, and the comparison is the one before it: equal
arguments holding no open cell are one type, anything else is walked.

A commit binds the arguments' cells in parameter order, and the walk
binds them in the body's. Where two arguments share a cell the orders
can differ in what holds (`D<'q, 'q>` against `D<i64, Any>`), so a
commit is first tried on copies, and one that does not hold there is
left to the walk, cells untouched. Where it holds, the application
holds: each question was asked as some position of the expansion asks
it, so what is bound is a solution of the expansion's constraints,
though the walk's order may find none: over `type D<'a0, 'a1, 'a2> =
[Array<D<'a1, 'a0, 'a2>>, `U('a0, 'a1, 'a2)]`, `let h = |k: D<'q1,
'q1, 'q2>| 0` called with a `D<i64, Any, Any>` checks, `'q1` being
`Any`, where against a textual twin `F<i64, Any, Any>` the walk binds
`'q1` to `i64` first and refuses (pin
`lang::types::a_cell_two_arguments_of_one_typedef_share_takes_both`). A
refusal the arguments' probe finds binds nothing, so its message prints
the cells as they stood.

The cost is the parameters', not the applications the definition
unfolds to: one definition of `n` parameters that permutes them reaches
`n!` applications of itself, and two of them compare in `n + 1` probe
steps (pins
`typ::contains::tests::applications_of_one_definition_compare_by_their_arguments`
at 6 and 12 parameters,
`lang::types::{six,twelve}_parameters_of_one_recursive_typedef_check`,
`typ::variance::test`). Two definitions with one body are walked, each
application twice: once by the trial of the outermost union with two
members holding open cells and once by the placement it allows
(`design/type_operation_scaling.md`).

### A trial starts from the questions in progress

A pair with a name on one side is expanded under the assumption that it
holds, so its recursion, the pair met again inside its own proof, is
answered (`ContainsHist::assuming`). A trial on copies
(`Type::place_in_order`, `on_copies`) is a walk of its own over fresh
cells, and the same recursion met there is a pair nothing has in
progress. A union with a bare open member places the other side's
members on copies first, since the bare cell could take any of them,
its definition's own recursion included: a recursive definition with a
bare parameter (`type Tree<'a> = ['a, `Node(Array<Tree<'a>>)]`) would
expand the copy, place its members on copies again, and never end. So a
walk keeps the pairs holding an open cell that it is expanding
(`ContainsHist::unfolding`), and that trial starts with their copies in
progress (`ContainsHist::seed` with `Inherit::Expanding`; the copies
are made with the trial's own cell map, so their cells are the
trial's): it meets the recursion as the walk on the cells themselves
will, by assumption. Every other trial made inside one carries on the
pairs its walk was seeded with (`Inherit::Assumed`) and adds none of
its own, so a walk no bare member reaches is the walk it was.
`Tree<'q>` holding a `Tree<i64>` gives `'q` its `i64`, in one
definition or two with one body (pins
`lang::types::a_recursive_typedef_with_a_bare_parameter_over_an_open_cell_checks`
and the three after it, `two_definitions_with_a_bare_parameter_check`,
`two_recursive_typedefs_with_a_bare_parameter_over_an_open_cell_check`,
`a_permuting_recursive_typedef_with_a_bare_parameter_checks`,
`a_bare_parameter_under_a_union_argument_of_a_recursive_typedef_checks`,
`typ::contains::tests::a_trial_meets_the_recursion_it_is_inside_by_assumption`).

### The occurs check

Before a cell binds, takes a lower bound or merges with another, the
check walks from the other side to it through bindings, conjuncts and
lower bounds (`tvar::would_cycle_inner`); where the walk reaches it,
`tvar::cycle_toward` says how. A bind or a merge makes the two sides one
type, so its walk takes every edge. A lower bound `t0 ⊇ t1` closes a
cycle only down from `t1` (its binding, its lower bounds, and the
covariant parts of what they hold) or up from `t0` (its binding and
conjuncts): `'p` above both `Array<'x>` and `'x` is `[Array<'x>, 'x]`, no
cycle (pin `lang::types::lower_bounds_beside_an_array_of_themselves`).
A contravariant part turns the walk round; an invariant part (an
abstract's parameter, `&mut`, a constructor application) and a typedef's
parameter take every edge. A declared variable under its definition's
check does none of the three, so against a bound cell it takes no walk:
the binding decides, through the variable's bounds where the cell is to
hold it, and a cell its bound names closes no cycle. Under `'e: 'z` with
`'z` decided (an instance checked again), `'z` holds `'e` and `'e` keeps
its bound (pins
`typ::contains::tests::a_declared_var_and_the_bound_cell_its_bound_names`,
graphix-shell's `no_subst::an_instance_checked_again_holds_a_nested_variable_bounded_by_an_enclosing_one`).
Where two cells meet (`contains_tvars`):

- A path through a constructor's part (an element, a field, a payload,
  a function's argument or result, a referent, an abstract's
  parameter, a typedef parameter its body holds under a constructor,
  through every typedef it names): the cell would hold itself, which no
  finite type does, as no non-contractive typedef does. The relation is
  refused. `b <- B([t ~ b.0])` over `B([])` (`type B<'p> =
  Abstract<'p>`) asks `'p ⊇ 'q` with `'q` above `Array<'p>`, and is
  refused as `b <- [t ~ b]` is (pins `lang::types::payload_cycle_*`,
  `array_cycle*`). A write so refused says no finite type holds both
  and to declare a named recursive type (`type X = Array<X>`), never a
  union of the two (`Type::meets_itself`, which reads an open cell as
  its lower bounds too and pairs functions' arguments and results);
  any other refusal such a cycle decided says `cannot infer a finite type here`
  (`TypeMismatch::infinite`), as an argument a cycle only the instance
  closes refuses does (pin `payload_cycle_an_instance_closes`). The
  hint reads the types alone: where the held side is closed (`let l =
  `Nil; l <- `Cons(1, t ~ l)`, or a REPL input's `b = B([1])`, whose
  `'p` settled to `Array<i64>` before `b <- B([t ~ b.0])`), nothing
  meets itself and the write names the union.
- An open cell holding a union it is a member of drops that member
  (`'r ⊇ [T, 'r]` binds `'r := T`), and so does an open cell meeting a
  bound cell whose binding reaches it, which walks the binding. The
  union's open members that are not rigid are one type with the cell,
  as each would be alone; the others, a declared variable under its
  definition's check among them, are held together, as the union
  without the cell would be: `'r ⊇ [Array<i64>, Array<string>, 'r]`
  binds `'r := [Array<i64>, Array<string>]`, never its first member,
  and `'r ⊇ ['a, Array<i64>, 'r]` binds `'r := ['a, Array<i64>]`. So a
  recursive function returns the union its non-recursive twin does,
  except that an arm whose type is an undecided variable is one type
  with the result (pins `lang::types::rec_block_*_collapses`,
  `rec_union_result_*`).
- A path through cells and unions alone otherwise (`'p` above `'q`
  above `'p`): every cell on it is one type (`merge_bare_cycle`, the
  paths' vars from `tvar::bare_path`; every path, so a cycle through
  three cells merges all three). The open cells merge, each first
  dropping its bounds that are cells of the cycle: a union lower bound
  holding one keeps its other members, as `'r ⊇ [T, 'r]` collapses, and
  a conjunct union holding one always holds. A bound that names a union
  is that union here (`TVar::drop_bounds_on`): a name holds a cell as a
  parameter its body holds under no constructor, which is a member of
  the union it names, so under `'a: O<'b>` over `type O<'t> = ['t,
  null]` the bound goes as `['b, null]` does, and a lower bound `O<'b>`
  leaves `null`. `'a` holding `'b` is then `'b`'s type, decided or not,
  and a `null` declared for it is refused. The dropped name still holds
  its parameters to their bounds, as every lookup of it does: under `'a:
  N<'b>` over `type N<'t: Number> = ['t, null]` the merged cell stands
  within `Number` (pins
  `lang::types::bare_cycle_through_a_bounds_*`,
  `typ::contains::tests::bare_cycle_through_a_typedefs_parameter_*`).
  Each bound cell on the cycle is the merged cell (`'r = [T, 'r]` gives
  `'r ⊇ T`). Two declared
  variables of the definition under check are not one type, so such a
  relation is refused. A later write to any of the cells widens them
  all: `x = B(1); y = B(t ~ x.0); x <- y; y <- B(t ~ "a")` makes `x.0`
  `[i64, string]` (pins `lang::types::payload_cycle_of_cells`,
  `bare_cycle_*`).

The merges' own guards (`TVar::alias`, `alias_cells`, `copy`) skip a
merge that would close a cycle and mark both cells
`CellFact::CycleRefused`; one still open at the terminal settle is
refused as the infinite type. Most
relations that reach them were checked here first; a `Singleton`
bound's merge of a union's open members was not, and the mark leaves
such a program refused later, at the settle or by the bound (pins
`lang::types::merge_under_a_constructor*`).

A conjunct that holds its own cell says nothing of it: `'c ⊆ ['c,
null]` whatever `'c` is. It holds the cell where it is the cell, a
union one of whose members holds it (through names and bound cells), or
an open cell above a lower bound that holds it (`contains::holds_cell`).
A bound's part that holds an open cell is passed down to no cell of the
value (`contains::commit_parts`): the bound's own containment, which
follows the parts (`contains::commit_conjuncts`), relates the two, the
bound's cell standing above what it took. `f([u, v])` over `'x:
Array<['a, null]>` with `u` and `v` undecided puts `'a` above both and
leaves each as it stood, and `f([u, [u]])` puts it above `u` and
`Array<'u>`, as the union written in the parameter takes both. A cell
narrowed by such a part would stand within a union whose variable stands
above it, and the same cell under a constructor could go below that
variable no more: the two bounds read as a cycle. A conjunct added by
any other way (`TVar::narrow_cell`) is not added where it holds
its cell, and a probe of a cell (`conjuncts_admit`) skips one that came
to hold the cell later (a member took the cell as a lower bound after
the conjunct was added), since it would send the probe back to the
cell, a check without end. Pins:
`lang::types::wrapper_passing_a_union_over_its_variable_*`,
`undecided_element*_bound_*`, `two_undecided_elements_*`,
`undecided_{value,binding,array}_*_under_a_union_bound*`,
`contains::tests::{a_bound_that_holds_its_own_cell_narrows_nothing,
a_conjunct_that_comes_to_hold_its_own_cell_admits_every_type,
a_bounds_part_holding_an_open_cell_narrows_no_cell}`.

A bound's parts reach every open cell of the value its variable takes
(`contains::commit_parts`): a part that is one type binds the cell it
meets, any other that holds no open cell narrows it. The walk goes
through what the two sides
name: a decided variable of the bound is its binding, and a typedef
application in the value over an open cell is what it stands for, each
pair of a part and an application once, so two recursive names met
against each other return. A cell the walk left out would stay free of
the bound and settle within its own alone, where no instance holds
both. A wrapper `'b2: Array<G<'b4, 'b3>>, 'b3: Q<'b4, 'b4> |y4: 'b4, y3,
y2: 'b2| f(y2, y3, y4)` over a callee `'a2: Array<G<'a4, 'a3>>, 'a3:
Q<'a4, 'a4>` stands `'b2` within both chains, the callee's over `y3`'s
cell: `y2` given `[]` takes the element `G<'b4, 'b3>`, and its `'b3`,
which `G`'s body holds under the name `H`, takes the callee's part,
`y3`'s type. Pins: `lang::types::partial_wrapper_bound_*`,
`contains::tests::a_bounds_parts_reach_the_cells_under_a_name`.

A rigid cell of the value takes no part. A declared variable's bounds
are the ones its declaration writes and the conjuncts of a cell merged
into it, so its part is proved and never recorded on it. The probe
before the bind holds it against the part: `'b: Array<[i64, null]>`
taking a wrapper's `Array<'a>` is refused at the wrapper, which declares
`'a: [i64, null]` to pass. Where the part holds an open cell of the
bound, that cell stands above the variable, as it stands above a decided
type: `'b: Array<['c, null]> |x: 'b, s: 'c|` called `g(w, v)` in `|v: 'a,
w: Array<'a>|` puts `'c` above `'a`, which settles it as `'a` when
nothing else is above it, and `g(w, null)` is refused there, since `'a`
holds no null. A lower bound that another lower bound of the cell, a union,
holds as a member is not kept beside it (`TVar::add_lower`), so `'c` above `'a`
and above `['a, null]` is `['a, null]`, one union and not two bounds
that a later argument chooses between. A part recorded
on the rigid cell would prove itself (`['c, null] ⊇ 'a` by the conjunct
`['c, null]`), and `null ⊇ 'a` by deciding `'c` null. Where the value
holds the variable in a union, the bound's cell stands above the union
as it does above a decided one: `'b: {p: ['c, null], q: 'c}` taking
`{p: ['a, null], q: ['a, null]}` makes `'c` `['a, null]`, and a result
typed `'c` holds the null in the wrapper. The written parameter merges
`'c` with `'a` at `p` and refuses `q`, with a decided union alike. Two
unions of the bound over `'c` met by two of the wrapper's over `'a`
(`'b: (['c, null], ['c, i64]) |s: 'c, x: 'b|` taking `(['a, null], ['a,
null])`) put `'c` above `'a` and above the null of each, where a merge
would have the second union's `['c, i64]` hold no null. A call whose
argument at `s` is the null is still refused there, since `s`'s cell
stands above `['a, null]` and the argument binds it before `'a` is
decided. Pins:
`lang::types::declared_variable_under_a_callees_bound_*`,
`lang::types::declared_variable_in_a_union_under_a_callees_bound_*`,
`lang::types::declared_variable_under_two_unions_*`,
`contains::tests::a_bounds_part_narrows_no_rigid_cell`.

### `Concrete`: a conjunct that is a predicate

`'b: Concrete` (`Type::Concrete`, parsed only as a bound) says whatever
binds the cell is fully known: no open cell, no ⊥, no reference and no
function in it (a value read from data can't be a reference: a number
would become one to whatever variable has that id) and no abstract type
without a `Cast` implementation (design/abstract_cast.md). `'a: Data`, the
writers' bound, is its sibling: no reference, no function and no such
abstract type, an open cell admitted and never refused at the settle. The
type-directed builtins declare it on their target (`str::parse`, the
json/toml/pack/sqlite reads, `sys::net::subscribe`/`call`, the db
trees); their `Apply::typecheck1` hooks only extract the type they cast
to and never refuse.

- `Concrete ⊇ t` is a probe: `Data ⊇ t` and no ⊥ in `t`, judged through
  its type references (`type R = &i64` holds a reference), bound cells
  by their bindings, open cells admitted (`Type::concrete_holds`). A bind of a `Concrete` cell hands the
  conjunct to every open cell its binding reaches (`TVar::bind`), so the
  requirement is hereditary.
- It is never a witness, like a trait conjunct. A cell whose conjunct it
  is stays open until the terminal settle, which refuses it open or ⊥
  ("the type 'b must be fully known here"), but only at a position of
  the settled signature: a conjunct's own cells are no position.
- It travels as a conjunct: merged by aliasing, copied at instantiation,
  packed and imaged with the type, printed in the `fn<'b: Concrete>`
  header. A rigid variable absorbs it the way `|x: 'r| x + x` absorbs
  `Number`, and the inferred signature carries it to every call.
- A read's result is `Result<'b, E>`, a union beside its own error. In
  a union checked against a union the known members are matched first
  and an open member takes only the members they leave (all of them when
  none are left, `contains.rs`; a primitive member a known primitive only
  partly fills is left whole, so over `[i64, string, `A]` beside `i64` it
  takes all three, as over `[i64, string]`), so `let r: [&i64, Error<E>] =
  parse(..)`
  offers 'b only `&i64`, which `Concrete` refuses, never `Error<E>`.
  Where two members hold open cells (`[Array<'a1>, Array<'a2>, null]`, a
  bound naming two variables), the member that fits first alone may decide
  them so a later one fits nowhere: the other union's members go in as
  written, else deepest first, whichever order places them all on copies
  (`Type::place_each`). The walk on copies takes every such union below
  it as written, and so does the placement in an order it held, while
  the two decide alike: a placement that meets a choice between a
  union's members, or a part that waits for the end of a function's
  arguments, asks a trial at each such union from there
  (`Type::place_in_order`, `ContainsHist::strays`).
- A builtin reads a quantifier only through the values of the arguments
  that hold it. At a call, a quantifier of a builtin's signature that the
  result and throws do not hold, and that no argument the call passes or
  omits can carry a value of (none holds a function or an open part), is
  never read: it settles to ⊥ whatever its bounds
  (`CallSite::settle_unread`). So `sys::net::publish`'s `#on_write:
  [fn(v: 'a) -> _, null] = null` keeps `'a: Concrete` and a call with no
  handler is accepted; the builtin publishes without writes. A definition
  written in Graphix gets no such settle: its body may read the variable
  elsewhere (`cast<'a>(x)`).

A definition's check runs no `typecheck1`, so its call sites record
their terminal settle in the gate's frame (`CallSite::typecheck0` under
a gate), and the gate hands them to the enclosing statement's drain with
every cell its signature reaches exempt (`DefGate::close`): a cell the
definition owns is judged at the definition, called or not; a
signature's cell stays generalized and each call settles its copy.

### Generic only where a binding names it

A function stays generic only where a binding names it (ML's rule, Eric
11-05): a `let` whose value, past parentheses and a block's final
expression, is a lambda, a generalized binding or a field or tuple read
of one (`let j = s.g`, `let j = t.0`, `let j = u.a.g`, `let f = { let n
= 1; |x| x }`: `bind::names_generic`), or a data literal holding these
(`bind::holds_function`), a write to a generalized binding, a labeled
default (its parameter names it), a call's callee by name, a top-level
input's value (`bind::name_value` marks each: the lambda, or the
reference at the root of the read). The value restriction stands: a
binding something writes is one instance (`let j = s.g; j <- ..`), and a
reference is no binding naming a function (`let r = &f` makes `*r` one
instance), as in ML. A lambda a binding names generalizes its signature at its
gate's close; any other takes its enclosing level's
(`Lambda::named`, `DefGate::close`, `FnType::claim`), so its cells are
the enclosing definition's, or the top level's, and every use shares
them. A read of a generalized binding a binding names copies its scheme
generic (`Fresh::Scheme`); any other read is one instance of it
(`Ref::naming`, `FnType::one_instance`, `Type::instance_read`:
`Fresh::Instantiate`). So an element of a collection a call returns, a
callback's result, a field of a call's result and an argument is one
instance, which its uses decide: `let fs = array::map([1], |r| 'a:
Number |x: 'a| x); let g = fs[0]$` makes `g` one type, `g(1)` and then
`g(1.5)` is refused, and so is `let k = (id, 1).0` at two types (a read
of a literal, not of a generalized binding). A call
copies a quantifier only where its level says so, as any cell
(`FnType::instantiate`), and a parameter called in its definition's body
shares its cells. A write to a generalized binding's place through
`&mut` reads the root as named, so the place's type holds the scheme's
cells (`crate::holds_generic` refuses it). Pins: `lang::quantified`
(`a_callbacks_result_is_one_instance` and its neighbours,
`a_field_of_a_generic_binding_is_generic`,
`a_block_whose_value_is_a_lambda_is_generic`,
`a_reference_to_a_generic_function_is_one_instance`,
`a_written_field_alias_is_one_instance`),
`lang::collection::collection_returned_function_is_one_instance`,
`lang::functions::generic_function_*`.

`instance_sig` holds an instance's signature to a copy of the
definition's by `FnType` containment, the parameters in the order
written, then the result: a result typed by a union of two open cells
meets what the parameters that hold each member alone decided, and a
parameter typed so is met where it is written, as the call that made the
instance met its argument. One cell standing twice is no such union:
`Array<['a, 'a]>`, what a slice arm binding two elements leaves an
unannotated parameter, is decided where it stands, before the result
(pins
`lang::types::slice_arm_returning_two_elements_over_an_unannotated_parameter`,
`slice_arm_returning_two_elements_takes_an_array_of_two_undecided`, and
`slice_arms_over_an_unannotated_parameter_{read_from_a_field,closing_over_an_outer_one}`:
a function a field holds is bound at run time, where an instance that does
not type gives its call no value).
A written function type holds no such union (below, A union of
variables in a signature), so only an inferred one meets it (`|c, a, b|
select c { true => a, false => b }`), and a type that either member
could take is refused (A union's member takes a type by rule).

**An instance holds its signature cell by cell.** The instance's
signature is the definition's at the types the call's check decided, and
the call may have decided a union's members from other positions than the
containment meets first: the call meets its arguments in the order of
the parameters, and a union a typedef hides (`E<'a, 'b>`) is not held back
as a written one is. So where the containment fails, the instance's copy
takes each cell from what stands at its own place in the instance's
signature, wherever the two are built alike (`Type::place_cells`: a named
type's parameters, an array's element, a tuple's, a struct's, a function's
positions; a place holding ⊥ gives its cell ⊥), and the containment then
judges the whole (`FnType::held_at`).
A union's members have places only where the instance's union stands
member for member beside the definition's, as a copy of it does: as many
members, in the definition's order, each built by its member's
constructor, an open cell's being whatever stands there
(`fntyp::members_stand_alike`). A cell takes such a place only where no
place of its own binds it, since a union that reached the instance some
other way (a written function type the definition was passed as) may hold
its members in another order (unit test
`typ::fntyp::tests::a_cell_takes_a_members_place_after_its_own`). That is
what gives a cell only unions hold
the ⊥ a call left it at, which no containment of the union finds: `|x:
P<'a, string>, y: O<'a>|` over `type P<'p, 'q> = [Array<'p>, Array<'q>]`
and `type O<'t> = ['t, null]`, called through `|u, v| f(u, v)` with `["s"]`
and `null`, is held at `[Array<string>, Array<⊥>]` and `[null, ⊥]`, where
the containment gives `Array<'a>` to the decided `Array<string>` and then
finds no member of `[null, ⊥]` for a string (pins
`lang::types::instance_cell_only_union_members_hold_takes_its_members_place`,
`instance_cell_under_a_recursive_typedefs_members_takes_its_members_place`,
`instance_cells_under_a_unions_member_take_a_typedefs_arguments`).
Before any of that, a cell the definition leaves open is the instance's
cell wherever that one is open too, at a place of the cell's own
(`Type::place_open_cells`): the call's check left it open, so no member of
a union beside it takes it. `f(never(), 1.5)` over `|k: fn(q: P<'b, 'a>)
-> i64, y: 'a| k`, inside a wrapper, is held at `fn(q: P<'b, f64>) -> i64`
with `'b` open, where the containment alone gives `Array<'b>` to the
decided `Array<f64>` (a decided member takes an undecided type first) and
types the instance at `P<f64, f64>`. The instance's result joins its
call's the same way (`CallSite::setup_static_bind`): the cells both leave
open are one cell at each place, then the call's result holds the
instance's; a call whose result reaches no open cell (`Type::prove_ground`)
has none to share and walks nothing. So the build decides nothing the
check left open and refuses no union two of whose members are open on both
sides (unit test
`typ::fntyp::tests::an_open_cell_is_held_at_the_instances_open_cell`,
pins `lang::types::instance_result_holding_two_undecided_members_is_its_calls`,
`graphix-shell/tests/shell/elab_audit.rs::an_instance_leaves_open_what_its_call_left_open`).
Every placing walk takes a pair of shared nodes once among a union's
members and once outside one (`fntyp::Places`): a result that is a DAG of
pairs costs its nodes, where a walk of its paths never ends (unit test
`typ::fntyp::tests::placing_walks_a_pair_of_shared_nodes_once`).
A function an instance defines takes its signature from its row of the
enclosing definition's check the same way: its cells are placed at the
row, its written parameters then take the row's
(`FnType::pre_unify_params`, for a cell no place bound), and the
containment judges (`Lambda::typecheck0_instance`), in that order. The
containment alone would give `Array<'a>` of `|x0: P<'a, string>, x1: 'a|`
to the row's `Array<string>`, and the build would refuse every call of
the inner function at another `'a` (pins
`lang::types::function_defined_in_an_instance_holds_a_variable_{beside_a_written_member,in_a_written_union,no_argument_decides}`,
and `function_defined_in_an_instance_stays_generic_beside_the_outer_variable`,
which calls the inner function at two types).
Where that still has to choose, the cells no position holds, a variable
only bounds name, are first settled to their bounds, the deepest first
(`TVar::settle_bounds_first`), as the call's check leaves a variable it
did not decide; the members are then decided and the containment has no
choice to make (pins
`lang::types::instance_{cells_only_bounds_hold_settle_to_their_bounds,cell_at_a_bottom_place_is_bottom}`).
Such a cell's bound holds the copy's own cells, never copies of them: in
`'a0: ['a1, null], 'a1: ['a2, null], 'a2: ['a3, null], 'a3: ['a4, bool],
'a4: Number |x: ('a0, 'a2)|` the cell of `'a1` is `[null, 'a2]` over the
`'a2` that then takes its place, so `'a0`'s bound is decided when `'a0`
is placed (pin
`lang::types::instance_cell_only_bounds_hold_names_a_placed_cell`).
The placing takes a cell after the open cells its bounds hold, through
any number of bounds (`fntyp::place_in_order`), so a variable a bound
names takes its own place, not what the bound's holder is placed at
(`'b: Array<'a> |x: 'b, y: 'a|` at `Array<i64>` and `[i64, string]`; pins
`lang::functions::bound_naming_a_wider_variable_builds`,
`lang::types::instance_cells_of_a_chain_of_bounds_are_placed_deepest_first`).
A cell is placed after the open cells its lower bounds hold as well: the
payload of `|a, z| a <- B([z])` stands above `Array<'z>`, and at
`B<[Array<i64>, Array<string>]>` and `string` the union alone would give
`'z` its first member that holds the bound, which `z`'s own place then
refuses (pins `lang::types::two_payloads_decided_apart_at_a_call_build`,
`typ::fntyp::tests::a_cell_a_lower_bound_holds_is_placed_before_its_holder`).
A variable a bound names that stands only among a union's members has no
place (`'b: Array<'a> |x: 'b, y: ['a, null]|`), and the holder's bound
would decide it narrower than the union stands. Where the containment
still fails, the formals that hold no bound's holder are contained before
the holders are placed, so the union decides its member
(`FnType::contain_free_formals`; pin
`lang::functions::bound_naming_a_wider_variable_in_a_union_builds`). That
order is tried last, so an instance an earlier order holds is held as it
was.
The containment picks no member of a union by order. The instance of
`|a| a <- B([never()])` at `B([])` holds one array: the settle made one
bound of the two that differ only in cells nothing has decided, and the
call's settle decided the cells its bounds hold (pin
`lang::types::undecided_payloads_met_at_a_call_build`). Without it the build refused what
the check accepted: `let f = |x: E<'a, 'b>, y: 'a, z: 'b| ..; f(never(),
1, "s")`, or `|x: ['a, Error<'e>]| -> (Array<'a>, Array<'e>) ..` called
with an `` [Error<`A>, Error<`B>] ``. Pins:
`lang::types::{undecided_argument_into_a_typedef_union_is_decided_by_the_other_parameters,empty_array_into_a_typedef_union_is_decided_by_the_other_parameters,empty_array_into_a_typedef_union_of_two_arrays_is_decided_by_the_other_parameters,undecided_argument_into_a_typedef_union_is_decided_by_the_result,two_members_one_constructor_member_could_take_are_placed_by_the_result}`.

A type written in a body names the variables of the definitions around
it (`lambda::body_annotation`); a labeled default is written in its
definition's signature, so its types name the variables of the
definitions around that one (`LambdaDef::in_signature`). An instance's
body compiles under the variables its definition's signature writes
(`FnType::written_tvars`: those its positions name and, through them,
those the bounds of its own quantifiers name), each the instance's copy
of that cell, bound to the instance's type (`lambda::written_tvars`,
through the map `lambda::instance_cells` returns). The copy is found by
its cell, never by its name: an instance's signature also holds the
cells its definition's check inferred, and those carry the names of the
callees' variables they were copied from (`array::len`'s `'a` in `|q, p:
'a| { let f = |x: 'a| x; (array::len(q), f(p)) }`), so a name can stand
for two cells there (pins
`lang::types::{written_variable_beside_an_inferred_cell_of_its_name,enclosing_variable_beside_an_inferred_cell_of_its_name,enclosing_variable_through_two_instances,variable_a_bound_alone_writes_is_the_enclosing_definitions,default_naming_an_enclosing_variable_beside_an_inferred_cell}`,
`lang::image::a_restored_nested_definition_names_its_enclosing_variables`).
A name only the bound of a bound writes (`'a2` in `'a0: Array<'a1>, 'a1:
Array<'a2> |x: 'a0|`) is the definition's variable in its check and in
an instance alike, rigid in the body as any other (pins
`lang::types::variable_a_bound_of_a_bound_writes_{is_the_enclosing_definitions,is_rigid_in_the_body}`).
A lambda no binding names is its one instance, so a variable it declares
stands bound in its signature and has no copy: its body compiles under a
cell of that name bound to the type, and the name never reaches a
variable of the same name a definition further out declares (`'a` in
`|p: 'a, n: i64| array::map([n], 'a: Any |s: 'a| { let v: 'a = s; v })`
is the callback's `i64`, never `p`'s type; pins
`lang::types::{one_instance_lambdas_variable_shadows_in_a_nested_signature,one_instance_lambdas_variable_shadows_in_a_body_annotation,one_instance_lambdas_variable_shadows_in_a_default,returned_lambdas_variable_shadows_at_a_run_time_bind}`).
A default compiled at a call's bind takes its types
from its definition's check, as an instance's body does: the definition's
`DefTable` records the rows of the defaults its check compiled
(`DefTable::record_defaults`), and the bind types the default's nodes from
them (`InstanceTypes::of_default`), each open cell the definition owns
copied, an enclosing instance's renamed through its map. So an arm the
instance's narrower type leaves unreached keeps the binds the definition
gave it and never runs, and a reached arm's bind keeps its type (pins
`lang::select::default_at_a_bind_takes_its_definitions_coverage`,
`default_at_a_bind_reads_an_unreached_arms_binds`,
`default_at_a_bind_keeps_its_definitions_bind_types`). Those rows stay
open at the definition's settle (`DefTable::settle_open` leaves them): the
bind narrows them by its own view of the label. Every default keeps its
rows, one typed with a type name included: a name a block declares is
found after the block (`Env::block_types`, `design/env_independent_typerefs.md`),
so the rows the definition's check filled mean at the bind what they meant
there (pins `default_at_a_bind_declares_a_recursive_type`,
`default_at_a_bind_names_a_block_type_over_an_outer_one`,
`lang::block_types::default_naming_a_block_type_takes_its_definitions_types`).
The bind checks a default in full only where there are no rows (a
builtin's, or under `GRAPHIX_NO_SUBST`; `node::rechecking_instance`).

A signature finds its quantifiers by name (`FnType::quantifiers`). A
parameter the body unifies with a declared quantifier (`x` in `'a:
Number |x, y: 'a| -> 'a x + y`) is a second name for the quantifier's
cell, and every copy keeps the written name beside the inferred one
(`TVar::named_as`; inferred names of one cell collapse to one), so the
copy still knows the cell for a quantifier and a site's settle leaves
it. A merge re-points only the var it merges; every other var of the
abandoned cell reads it as a forward link to the survivor (`TVar::
open_cell` follows it), and is a second name for the survivor: a copy
names the survivor's image after it too (`TVar::forwarded_as`), a gate
holds the cell at the end of the link (`TVar::open_rigid`), and a
quantifier is open or rigid by that cell (`FnType::bounds_hold`). A gate on the forwarding cell would
leave the survivor flexible, and `Number ⊇ 'q` would bind it to
`Number`'s first member.

`FnType::contains` checks a declared bound only for a BOUND variable
(`bounds_hold`), one whose cell is bound at the end of any forward
link. An open one stays open, standing for one type the
bound admits; its cell enforces the bound at every binding. Binding
it to its bound would type the argument's parameter as the whole set
(`Number`), which is a different claim: values of several numeric
types at once.

### `Function`: a conjunct that is a predicate

`'a: Function` (`Type::Function`, parsed only as a bound) says whatever
binds the cell is a function type a generated call can pass every
argument of: no variadic argument and no defaulted label. A builtin that
wraps a function and reads its signature declares it (`queuefn`); its
`typecheck1` reads the signature through type references and never
refuses.

- `Function ⊇ t` holds for such a function type, through bindings and
  type references (`Type::function_holds`), and for an open cell; ⊥, a
  union and a function with a variadic argument or a defaulted label are
  not. It is not hereditary.
- Like `Concrete`, it is never a witness, travels as a conjunct, and a
  cell that stays open to the terminal settle at a position of the
  signature is refused ("the type 'a must be a function here"): ⊥ binds
  no cell, so `queuefn(#trigger: t, never())` is refused there.
- The checked-types snapshot tooling reads (`Type::snapshot`) keeps an
  open cell's `Concrete` and `Function` conjuncts.

### `Singleton`: a conjunct that is a predicate

`'a: Singleton` (`Type::Singleton`, parsed only as a bound) says
whatever binds the cell is one type, not a union of several; a
primitive set of two or more (`[i64, f64]`, `Number`) is a union.
Arithmetic is `fn<'a: Number + Singleton>(x: 'a, y: 'a) -> 'a`: an
operand's open cell takes both conjuncts (`op.rs::arith_rule`,
`Type::narrow_singleton`), so a generic definition called with
`[i64, f64]` is refused at the call, by the check. Before it, only the
instance's own re-check refused it, at elaboration.

- `Singleton ⊇ t` (`Type::singleton_holds`) counts `t`'s members as a
  union through bindings and type references; ⊥ adds none, `Any` is
  several. An open cell, alone or a member of a union, is admitted.
- A bind of a `Singleton` cell narrows the open members of its binding
  (`Type::require_singleton`): beside one known member each must lie
  within it, so `[i64, 'x]` cannot become `[i64, f64]`; with none, they
  merge into one cell, which takes the conjunct (`UnionParts::
  merge_open`). A rigid cell can't merge or narrow, so a union of two
  declared variables, or of one beside a known member, is not a
  `Singleton` (`UnionParts::rigid_apart`).
- Like `Concrete`, it is never a witness and travels as a conjunct. It
  refuses no open or ⊥ cell at the terminal settle: an operand cell
  left open there has `Number` for its only witness, which the
  conjunct refuses.
- Comparison carries no such bound: it takes any one type, unions and
  mixed numerics included (Eric, 2026-10-04). It carries `Discernible`.

### `OneNumber`: a conjunct that is a predicate

`'a: OneNumber` (`Type::OneNumber`, parsed only as a bound) says
whatever binds the cell holds at most one numeric type: `[i64, null]`
and `[string, u8]` do, `[i64, f64, null]` and `Number` do not. Nothing
adds it; it is written, for a nullable number of one numeric type:
`'a: [Number, null] + OneNumber`.

- `OneNumber ⊇ t` (`Type::one_number_holds`) counts `t`'s numeric
  members the way `Singleton` counts its members; open cells are
  admitted. Against a fixed signature (`ContainsFlags::Sealed`) a rigid
  member is its caller's pick (`UnionParts::count_rigid_numbers`): it
  counts as the numeric types that are members of every type among its
  conjuncts, all of them where it has none; a conjunct with a member
  that is `Any` or a variable admits all, and an unknown below a
  constructor (`Array<'c>`) is no number; where a conjunct implies
  `OneNumber` and more than one of those is new to the union, as one
  more, which no other member is known to be. So ``['a, `X]`` holds
  under `'a: Number + OneNumber` and not under `'a: Number`, `['a, i64]`
  under `'a: i64` and not under `'a: Number + OneNumber`.
- A bind of a `OneNumber` cell hands the conjunct to its binding's open
  members while none of its members is numeric
  (`Type::require_one_number`). Beside a numeric member an open one may
  still bind another numeric type: no upper bound says "not a number,
  or this one".
- Like `Singleton`, it is never a witness and refuses no open or ⊥ cell
  at the terminal settle.

### `Discernible`: a conjunct that is a predicate

`'a: Discernible` (`Type::Discernible`, parsed only as a bound) says no
union anywhere in whatever binds the cell holds two members with one
runtime form (`Type::rep_ambiguity`): two function types, a Rust-backed
abstract type at two instantiations (a tuple, struct, variant and list
each carry their kind's marker, `design/distinct_composite_forms.md`, and
a comparison reads a reference as what it names). Members that share a
constructor are told apart by their parts; a union under a reference,
a function or an abstract type with its own `Eq` or `Ord`
implementation is not looked at, and values of such a type compare and
hash as their types do. An abstract type with neither compares, orders
and hashes by its representation, so the walks look through it, its
parameters substituted (`compared_rep`), and a failure found there is
told by the abstract's name, its representation being its module's
(`Indiscernible::Abstract`); a Rust-backed one has none to look into. Comparison (`==` and the
orderings), a map literal's key type and the stdlib functions that
compare or hash (`uniq`, `min`, `max`, `array::sort`, `array::dedup`,
`list::sort`, the `map::` functions that take or make keys, `db`'s tree
keys) carry it; a generic definition's variable takes it from its body,
as arithmetic hands it `Singleton`, so each call checks it.

- `Discernible ⊇ t` reads an open cell as no type yet: nothing collides
  with it (`Open::Benign`).
- A bind of a `Discernible` cell hands the conjunct to every open cell
  its binding holds where a comparison looks (`Type::
  require_compared`): not under a reference or a function; an abstract
  type's parameters included, whether or not it has its own
  implementation, which the bind cannot see.
- A conjunct on each open cell can't say that two members of one union
  must differ from each other, so the rest is judged at the statement's
  settle (`PendingSettle::Discernible` for a comparison and a map
  literal; each `Discernible` cell of a call's signature, from the
  site's settle): the cells filled by then are read as they stand, and
  one still open reads as any type it may yet bind (`Open::Unknown`):
  it collides with another open member and with a member whose form
  another type shares. In a generic definition that refuses `['a,
  string]` and `['a, 'b]` as compared or key types; annotate them.
- A `Concrete` cell's binding is judged at the same settle for its map
  keys (a map read from data).
- Like `Singleton`, it is never a witness and refuses no open or ⊥ cell
  at the terminal settle.

### `NoError`: a conjunct that is a predicate

`'r: NoError` (`Type::NoError`, parsed only as a bound) says no value of
whatever binds the cell is an error: no member is an `Error<..>`, holds
the primitive `error`, or is `Any`. `Result` carries it on its success
type, `type Result<'r: NoError, 'e> = ['r, Error<'e>]`, so a result's
error member is the only error in it and `error as e` over a result tells
the two apart. A typedef parameter's bound applies where the reference
expands (`lookup_ref_by`): an open argument takes the conjunct, a known
one is checked, so `Result<[i64, Error<`F>], `E>` is refused, and a
generic definition's variable passed as `Result`'s success type carries
it in its signature. A written name is held the same way where it is
written (`TypeRef::check_application`): in `'a: Result<'b, `E>` the cell
`'b` takes `NoError` as its own conjunct at the signature, so a call that
leaves `'a` undecided still refuses an `'b` the bound rejects. An argument
that is an open variable through a typedef name (`N<I<'b>>` with `type I<'t> =
't`) or through a cell bound to one counts as that variable
(`Type::open_cell`): it takes the conjunct and is not decided by the bound.

- `NoError ⊇ t` (`Type::may_be_error` negated) reads `t`'s members
  through bindings and type references; an error nested in a collection
  is no member. An open cell is admitted; against a fixed signature
  (`ContainsFlags::Sealed`) a rigid member is its caller's pick, an
  error unless one of its conjuncts implies `NoError`.
- A bind of a `NoError` cell hands the conjunct to its binding's open
  members (`Type::require_no_error`).
- A `NoError` cell below a union holding errors takes the union without
  them (`Primitive ⊇ 'r` gives `'r` every primitive but `error`), and ⊥
  where errors are all it holds (`Result<⊥, E> ⊇ Result<'a, E>`); a read
  target, `Concrete` as well, refuses that ⊥.
- Like `Singleton`, it is never a witness, travels as a conjunct and
  refuses no open or ⊥ cell at the terminal settle.

### `Ordered`: `Discernible` without references

`'a: Ordered` (`Type::Ordered`) is `Discernible` and no reference where
a comparison looks (`Type::indiscernible` with `ordered`). A
reference's value is its own cell, numbered in whatever order compiling
made it, so it neither orders nor says what the reference names. The
orderings (`<` and the rest), map keys (the literal, the `map::`
functions, `db` tree keys), `array::sort`, `list::sort`, `min`, `max`
and `array::dedup` require it; `==`, `!=` and `uniq` require only
`Discernible` and compare references by what they name
(`design/place_references.md`). It travels, judges and settles as
`Discernible` does.

### `JsonData`, `TomlData`: what a text reader can tell apart

`'b: JsonData` (`json::read`'s target, and the json writers' value) and
`'b: TomlData` (toml's) hold of a type every union in which tells its
members apart by a tag the data carries (`Type::text_refusal`,
`typ/textdata.rs`): its members other than `null` each carry one, no two
the same, or it is one member beside `null`. A variant carries its tag;
an abstract type its unqualified name, since the formats write its value
inside an object of one entry keyed by that name (`{"Port":80}`) and read
it only so. Anything else carries none: a primitive, a struct, a tuple,
an array, a map, `Any`, an open cell (it may yet be anything), and core's
`Transparent<T>`, which the formats write as `T`'s data bare. An error is
no member: no data reads into one. So `` [`A, `B(i64), Port, null] ``
and `[T, null]` for a `T` that is no union hold, `[i64, string]`, `[Port,
u8]`, `` [Transparent<P>, `A] `` and `` [`A(i64), `A(string)] `` do not
(the type system merges the last into `` `A([i64, string]) ``, whose
payload is refused), and members under one tag are refused unless they
are one type, compared with their names and variables expanded (`` [`A(K),
`A(string)] `` over `type K = string` is one member; `Walk::same`, a
recursive pair met inside itself alike). Inside the data an abstract type's `from_value` reads (its
representation, where its implementation leaves `to_value` out), a cast
of data reads, and every abstract value there is written bare, so no
abstract type carries a tag: `[Port, Ito]` and `` [S, `A] `` are refused
there, `[N, null]` is one member beside null (and an `N` that writes null
reads back as the null, as under `Transparent`). The walk carries the
abstract type whose `from_value` reads the part it is in, and walks a
named type again inside such data. The
union's members are its members past every union and name (a typedef
whose body is a union, a primitive set), each judged by its constructor.
Over a type known not to be abstract `Transparent` is refused: it would
change nothing. Over a definition's own open variable it is not judged,
and where a call binds that variable to a type that is not abstract it
writes that type's data bare, as it would without `Transparent`. The
verdict is the written place's: an instance's body checked again
(`GRAPHIX_NO_SUBST`, `node::rechecking_instance`) judges its JSON and TOML
bounds with `TextSite::Instance`, which leaves every `Transparent` to the
definition's check. The walk reaches every part of the type: elements, fields,
payloads, a map's key and value, an abstract type's representation where
its `Cast` implementation leaves a method out (and `Transparent`'s
parameter), each named type once per list of arguments, in the text and
in a `from_value`'s data; a union's members expand each name once,
however many paths reach it, and a typedef's arguments take finitely many
forms (no typedef is expansive, below). A map's form is its key type's alone: an object
under `string`, its pairs under any other, so its key union is judged as
any other. A TOML document is a table: where the bound is a writer's
(`'a: Data + TomlData`), a map at the type's root, past names and a
union's members, whose key type is known and is not `string` is refused,
since it writes its pairs (`Type::toml_root_pairs`); toml's writer refuses
a map written as pairs at the root by its value where the type says
nothing (`Any`). A reader takes such a map from a table's entries.

The rule asks nothing of the cast's pick, which may read data as any
member: what a union reads back is decided by the tag alone.

Unlike the other predicates it is judged only of the call's settled
binding (`site_bounds`): a refusal at the bind would make a union target
narrow to the one member that passes. It is handed to an open cell of
its binding as `Concrete` is, so a generic reader's or writer's callers
are judged at their sites; an open cell beside another member is
refused at the definition, since what binds it later is judged alone.

An interface must declare what its implementation requires: every
conjunct of an implementation variable the signature leaves generic
must follow from the signature variable's conjuncts, or from the bound
of a typedef parameter it is passed as (`FnType::sig_matches`). A call
through the interface is checked by the signature alone; before this
rule, `val f: fn(x: 'a) -> 'a` over `|x| x + x` passed the check and
refused `f("s")` at elaboration. The other direction is not required:
an implementation may be more general than its interface (`|x| x` under
`fn<'a: Number>(x: 'a) -> 'a`), whatever its variables are called; a
bound it does carry pairs the variables inside it with the signature's.
That pairing repeats, the unions it defers paired between passes, until
a pass pairs nothing new, so a variable met only inside a bound (``'b:
['a, `X]``), at the end of a chain of bounds, or through a bound later in
the signature, is judged as one at a position is. Every conjunct of a
paired cell pairs, not the first alone: a cell may carry several of one
shape (`'q: Array<'p> + Array<'r>`, and a trait method's cell carries
its trait's bound beside its own), and each one that no bound paired
with pairs with a bound that takes it whole, so the variables inside
each are judged. A conjunct of another shape than every bound pairs
nothing, and a variable inside it stands for a part of the signature only
where its own conjuncts admit it, and is one type wherever a bound names
it: the part at its one place, or the witness of its own bounds, or the
implementation is refused (`design/module_system.md`). So does a union of
a bound's length whose members pair with the bound's in no order
(`SigPairs::settle_bound`): the implementation's may be the wider.

That refusal does not model which place of a signature variable a call
decides first (argument order decides, nothing is chased),
so an implementation variable named at two places of one signature
variable is refused whether or not the first of them is a parameter, and
the rewrite is the one the message names: the variable at a parameter, or
a bound of one type (`'p: [i64, string] + NoError`). Pins:
`lang::interfaces::an_unpaired_variable_at_one_variable_*`.

### `FnType` derives its constraint list from the cells

The `constraints` list is gone from `FnType` (`typ/fntyp.rs`). Every
former consumer derives from the cells:

- `constraint_view()` — `(tvar, conjunct)` pairs, one per conjunct of
  each declared quantifier's cell, normalized, sorted by name then
  conjunct and deduped; a `+` bound prints as `fn<'a: A + B>`. Feeds
  Display, Eq/Ord/Hash (Hash reads the shape only), and the contains
  bound check (`FnType::bounds_hold`): only a bound variable is
  checked, whatever its bound count, since an open variable settles by
  a witness and the cell enforces its bounds at every binding
  meanwhile. On the wire a cell's conjuncts travel with the cell
  (each TVar writes its own, or the image's shared cell definition
  holds them); a fn type writes none of its own.
- `FnType.quantifiers: Arc<[ArcStr]>` — the names the `fn<...>` header
  declared, in source order; the constraint types stay in the cells. The
  names decide which cells a call freshens or holds rigid and which
  conjuncts `constraint_view` (so Eq and Ord) sees, and the image keys them.
  One instance keeps the list (a call through it shares its cells, which
  the cells' levels say, not the list), so the header means "generic"
  only in print: where cells print as they stand
  (`PrintFlag::DerefTVars`: a checker message, the shell's `-:` line; and
  `PrintFlag::StandingHeaders`, which prints each variable by its name
  alone: the language server's hover, completion and outline, over the
  type `resolve_tvars` copied, whose open cells keep their levels) it
  lists a quantifier only while its cell is open and generic
  (`fntyp::printed_quantifiers`), and one a call picked or one instance
  holds (an unnamed lambda's, a read no binding names, a call's result)
  prints where it occurs, like an inferred variable; canonical printing
  lists every declared one. A refusal formatted after its check
  (`TypeMismatch`) prints a quantifier as it stood at the refusal
  (`tvar::HeldQuantifiers`): a `let rec`'s read of itself in its own body
  is one instance, its variables held by the open definition, and prints
  no header, though the definition's close has generalized the cells by
  the time the message prints. A type the language server shows after
  the check (a hover, a completion) holds them alike: a named
  definition's close tells the editor the quantifiers it generalizes and
  the span of its text (`ide::HeldSpan`), and a type shown with the
  cursor in that span prints under them (`query::held_at`,
  `HeldQuantifiers::resolving`), whether a binding, a read, a field or a
  name being completed shows it, so `let w = rf` in `rf`'s body hovers
  `w: fn(x: 'a) -> 'a`, as does the read `rf` there, and `rf` its header
  at its binding and at a read outside. With no
  header to scope it, a named cell
  keeps one name through one message, in every type the message prints,
  and a second cell of that name prints numbered apart (`'a1`; an
  inferred variable's too, since an instance copies the generated names
  of the cells it copies: `'_7`, then `'_7_1`, which no inferred variable
  prints as, so a union of two instances' members prints them both;
  `tvar::printed_name`, one table per message, `tvar::Shown`, owned by
  the outermost `format_with_flags`, `tvar::Message`, a hover's
  `display_type` included, looked up by cell so a type of many variables
  prints in linear time), so two instances side by side read as two
  variables; a cell a header lists is that header's until the signature
  ends (`tvar::quantifiers_apart`), under whatever name a binding reaches
  it by, and its header takes no name the message already shows another
  cell under, so no two cells showing share one. Any other cell keeps the
  first name it prints under whatever name a later binding reaches it by:
  a parameter's cell a callee's instance merged into prints under the
  parameter's name in the result too, where a copy left the callee's
  variable's name on it (`TVar::named_as`). A cell a definition being
  checked declares prints under the name its signature declares,
  whatever handle reaches it: a call's result or an inferred argument
  merged into the rigid `'b` reads `'b`, never the call's `'_N`
  (`tvar::Declared`, the variables each open definition gate declares,
  innermost first, `TVar::declared_name`, which a pattern refusal names
  the variable by too); a refusal formatted after its check prints them
  as at the refusal (a type mismatch's `HeldQuantifiers` and an
  `AmbiguousMember` keep them). A cell that holds a type
  prints in one form through the message too, the form its first handle
  printed in: bare as what it holds where that handle's name is
  generated, as `'a: T` where it is written (`tvar::prints_held`),
  so `fn(x: string) -> Array<string>`, never `-> Array<'a: string>`
  beside a bare `x`. A union prints once a member
  that prints as an earlier one does (`print::write_deref_set`), in a
  hover as in a message: one cell twice is one member. A name numbered apart is
  new to the message: no cell or header showed it, and no type the
  message has printed, the one printing included, holds it, so a name
  those types declare never moves for it and a name a header listed stays
  that header's once its signature ends (the printed types' names are
  walked only when a name numbers apart, `Type::names_held`). A type the
  message prints later is not known yet: a cell there that declares a
  name numbered apart before it numbers apart in turn (`'a11`). A
  signature match prints its signature and its implementation as two
  messages (`matches.rs::side`): each side names its own variables, so a
  name both write prints alike on both, and the implementation's cell
  prints as the val's refusal around it prints the implementation. A message
  about a generic binding itself prints its scheme
  (`Type::scheme_read`), never the one instance a comparison held rigid:
  the refused write to a generic binding (`Connect::typecheck0_with`)
  names it header and all.
  Pins: `lang::quantified::*_prints_no_header`,
  `two_instances_in_one_printed_type_*`,
  `a_printed_quantifier_names_its_cell_inside_a_binding_too`,
  `two_generic_functions_in_one_printed_type_keep_their_headers`,
  `a_header_prints_apart_from_an_instance_shown_before_it`,
  `one_message_names_a_cell_alike_in_each_type_it_prints`,
  `a_name_numbered_apart_takes_no_name_the_type_declares`,
  `a_header_keeps_its_name_beside_a_name_numbered_apart`,
  `a_later_cell_takes_no_name_a_header_listed`,
  `a_type_mismatch_prints_two_definitions_cells_apart`,
  `lang::interfaces::a_signature_and_its_implementation_print_a_name_both_write_alike`,
  `a_signature_through_a_type_reference_prints_a_name_both_write_alike`,
  `a_returned_functions_variable_keeps_its_header_in_the_scheme`,
  `a_refused_write_to_a_generic_binding_prints_its_header`,
  `a_parameter_cell_merged_with_a_callee_prints_under_one_name`,
  `a_slice_ladder_cell_merged_with_a_callee_prints_under_one_name`,
  `a_decided_cell_merged_with_a_callee_prints_bare_throughout`,
  `a_decided_cell_merged_with_a_callee_prints_named_throughout`,
  `a_call_result_in_a_declared_variable_prints_its_name`,
  `an_operand_in_a_declared_variable_prints_its_name`,
  `a_call_result_handed_on_prints_its_name`,
  `an_enclosing_definitions_variable_prints_its_name`,
  `two_declared_variables_print_their_names`,
  `an_impls_variable_prints_its_name`,
  `a_call_result_in_a_lets_two_variables_prints_its_name`,
  `lang::types::payload_cycle_through_a_typedef`; the hover's:
  `graphix-shell/tests/lsp/main.rs::one_instance_hovers_with_no_header`,
  `a_hover_numbers_an_instance_past_a_header_name`,
  `a_cell_merged_with_a_callee_hovers_under_one_name`,
  `a_type_shown_in_a_body_holds_its_definitions_variables`.
  A self-referential constraint
  `fn<'a: fn(x: 'a) -> _>(…)` is legal, and once seeded the declaring
  header and an inner fn that mentions `'a` reach the same cell and
  conjunct; each listing walk holds a per-signature reentrancy guard, so
  the regress stops at the signature already being listed.
- A nest of signatures is read once, however many of its signatures
  ask. Listing a signature reads the cells it names
  (`collect_tvars`), and a quantified signature inside it answers with
  the cells it holds from outside itself (`fntyp::free_cells`). Under
  the outermost walk, comparison or print (`PartsDepth`) that answer
  is kept per signature beside its position cells, and dropped at any
  cell write on the thread (`tvar::cell_writes`). Only an answer no
  reentrancy guard cut is kept, and one that names a cell is used only
  where no guard is held: an empty one is empty under every guard. The
  parser does more for the length of the outermost signature's parse
  (`ParseDepth`). A declaration inside another signature's
  (`FnType::declaring`) aliases its own quantifiers alone
  (`fntyp::Aliasing::Known`): each name it does not declare is left to
  the enclosing declaration that does, or to the outermost, which
  aliases every name left. It then records the cells it names from
  outside itself, and the parse's walks read that record instead of
  walking the signature again: an alias walk enters it only where it
  names a name the walk aliases, a held check or a bound's walk takes
  its cells from the record, and position cells are kept for the
  whole parse. A signature that names nothing outside itself (closed)
  is skipped by every walk, so it is the signature it is alone. Each
  occurrence is aliased once, by the declaration of its name, which
  walks down through the signatures between it and the occurrence: a
  nest whose signatures name, beside their own variables, only ones
  the outermost declares or none does parses in time linear in its
  size.
  Pins: `typ::fntyp::tests::nested_closed_signatures_parse_in_linear_steps`,
  `nested_open_signatures_parse_in_linear_steps`,
  `an_open_nest_names_its_declarers_cells`,
  `nested_quantified_types_compare_in_linear_steps`,
  `a_cell_write_drops_the_free_cells_read_before_it`,
  `a_closed_signature_nests_as_it_stands_alone`.
- A signature holds the variables its positions name and, through
  bounds, those the bound of each of its own quantifiers it holds names
  (`fntyp::collect_held`): a quantifier held only through the bound of
  a bound, or through a signature nested in it, is held, and the
  variables its bound names are in the signature's record and its
  answer. An alias walk passes the bounds of each own cell it meets in a
  signature's scope, once: those of the cells the positions name with
  the positions, the rest at the scope's end (`FnType::alias_scope`). A
  nested signature's walk hands the cells it met from outside itself to
  the scope around it, and one the walk does not enter hands it its
  record (`tvar::ConjunctsPassed::meet`, `fntyp::meet_declared`), so an
  occurrence in such a bound is its declarer's cell, never a name a
  signature between them declares. The held walk reads each own cell's
  bounds under no reentrancy guard the memo heeds, so a nested
  signature's answer comes from the memo; a cell reached again while its
  bounds are read is cut (`HOLDING`), by any signature that holds it, a
  normalized copy included; a refused entry leaves the walk that holds
  the key holding it. A bound that reaches its own variable this way is
  refused as any recursive bound is:
  `fn<'a: fn<'d: 'z, 'z: Array<'a>>(qd: 'd) -> 'd>(qa: 'a) -> i64`.
  Pins: `typ::fntyp::tests::a_bound_of_a_bound_names_the_declarers_cell`,
  `a_bound_naming_an_outer_variable_is_the_outer_cell`,
  `a_bound_held_through_a_nested_signature_keeps_its_scope`,
  `a_bound_reaching_its_variable_through_a_bound_of_a_bound_is_refused`,
  `nests_held_through_bounds_parse_in_linear_steps`,
  `nests_held_through_bounds_walk_in_linear_steps`,
  `nests_held_beside_a_bound_print_in_linear_steps`,
  `a_held_walk_cuts_a_cell_whose_bounds_it_is_reading`,
  `a_refused_entry_leaves_the_key_held`.
- An alias walk aliases a bound's variables in the scope of the
  signature that declares the bound's variable: a signature's walk
  passes the bounds of its own quantifiers alone, and a walk no
  signature in it declares passes none (`tvar::ConjunctsPassed`). So a
  variable an enclosing signature's bound names is that signature's,
  whatever a signature inside it declares of the same name, in a
  written nest, a nested lambda's declaration and an instance's copy
  alike. A nested lambda's bounds are aliased with the names the
  definitions around it declare, as its parameters are, before its
  declaration aliases its own (`Lambda::compile`), so a bound names an
  enclosing definition's variable wherever the lambda does not declare
  the name.
  Pins: `typ::fntyp::tests::a_nested_quantifier_takes_no_enclosing_bounds_variable`,
  `lang::types::a_nested_signatures_own_variable_takes_no_enclosing_bound`,
  `a_nested_signatures_own_variable_holds_no_enclosing_value`,
  `a_nested_lambdas_bound_names_the_enclosing_variable`,
  `a_nested_lambdas_bound_takes_the_enclosing_variables_value`,
  `deep_nesting::a_nested_lambda_bounded_by_an_enclosing_variable_checks_at_once`.
- A name walk reads bounds in the same scope: a quantified signature's
  walk (`collect_tvars`, `collect_tvars_through_bounds`, the held check,
  `constraint_view`, `fntyp::collect_held`, a print) passes the bounds of
  its own quantifiers alone, an unquantified signature inside it passes
  in that scope, and only a walk no signature encloses passes every cell's
  (`FnType::in_name_scope`, `TVarsSeen::passes`; a set of the names, so
  a chain of bounds stays linear). A cell declared outside the signature is
  its declarer's, and its bound names its declarer's variables: the walk
  never finds one of those under the name of the signature's own
  quantifier, so the bound of an enclosing definition's variable holds
  none of a nested definition's quantifiers, a nested definition's body
  holds its own variable rigid, never an enclosing one of its name in
  its place (`DefGate::open` reads the cells through
  `collect_tvars_through_bounds`), and a nested signature prints its own
  variable's bound.
  Pins: `lang::types::a_quantifier_only_an_enclosing_bound_names_is_refused`,
  `a_nested_quantifier_stays_rigid_beside_an_enclosing_bound_of_its_name`,
  `parser::test::a_nested_signature_prints_its_own_variables_bound`,
  `typ::fntyp::tests::a_walk_through_bounds_finds_its_own_quantifier`,
  `a_held_walk_reads_its_positions_in_its_own_name_scope`.
- `FnType::name_auto_variables` names, on the resolved type, each open
  inferred cell (`'_N`, whose digits change from run to run) a
  letter no other variable of the signature uses, in the order the
  signature prints them, and lists it in the signature's header, so a
  bound prints in the header (`fn<'a: Number>(q: 'a) -> 'a`) and a
  variable the positions share stays one variable, in a nested signature
  too (`fn<'a: Number>(z: i64) -> fn(q: 'a) -> 'a`). It runs on each
  outermost function type of a type (a struct's field, an array's
  element), and on a local function as on a top-level one. The resolve keeps the
  bounds of such a cell (`ResolveTvarsCx::bounded`). It leaves alone a
  cell a quantifier of the type stands for, an implicit `throws` slot,
  and a cell of the definitions the cursor is inside (`held`): the
  resolve copies those under the declared variable's own name, so a
  local returning `'a` hovers `-> 'a`. A decided cell prints as what it
  was decided to be. A hover, an outline and a completion, the REPL's
  included, print a binding's
  type through one function, `graphix_lsp::display_type`. Pins:
  `lsp::a_decided_variable_hovers_as_its_type`,
  `lsp::an_open_inferred_variable_hovers_as_a_listed_letter`,
  `lsp::an_open_inferred_variable_of_a_local_or_held_function_hovers_as_a_listed_letter`,
  `completion::tests::a_completion_describes_a_binding_as_a_hover_does`.
  `sig_matches_int`'s impl-constraint check is SATISFACTION — the
  signature's concrete choice must be admitted by the impl's inferred
  constraint — not structural equality, which spuriously rejected a
  concretely typed `.gxi` signature over an inferred-generic impl.

## Things deliberately left as they are

- **`any_as_tvar` stays.** `Any` pattern leaves serve four consumers
  (unification, exhaustiveness, dead-arm, runtime dispatch) and only
  unification wants tvar behaviour; replacing the leaves with cells at
  construction breaks the other three.
- **Typedef parameter constraints stay separate** (`Option<Type>`,
  checked at ref expansion): a different lifecycle from cell conjuncts
  (checked at bind), and unifying them would mean ref expansion
  instantiating cells with no bug or feature asking for it.
- **`frozen` gates aliasing only** (a frozen var won't re-point); it
  does not block binds and settle ignores it. The ABI freeze already
  refuses multi-class cells, so no "freeze binds iff single register
  class" rule is needed at the typ layer.
- **Dynamically dispatched calls check only the signature**; a
  per-site body inconsistency there de-fuses or lazy-binds instead of
  rejecting.
- **No scoped aliasing.** A polymorphic fn value's instantiated
  signature arriving as payload in another signature brings a second
  quantifier scope into the walks; aliasing its `'a` with the native
  `'a` would build the infinite type, which the merge-occurs guard
  refuses (`alias`/`alias_cells` and the positional `would_cycle`
  guards, marking `cycle_refused`). Scope-aware alias legs were
  diagnosed before implementation (`GRAPHIX_DBG_CYCLE_BT`): the benign
  cross-scope class and genuine infinite types have IDENTICAL refusal
  channel profiles (positional `would_cycle` in argument acceptance
  walks, CallSite containment, instance checks). A fresh call signature
  is never aliased by name: `FnType::instantiate` keeps its topology by
  cell and freezes only the fresh cells that repeat, so
  a callback argument holding another function's `'b` stays apart from
  the callee's own. The only discriminator is the
  marked cell's STATE at terminal settle (open and unconstrained →
  error), which the dependency-ordered settle controls
  deterministically. Nested-fn name flatness within one signature is
  rank-1 polymorphism and load-bearing (`inlang_map`, the def-gate
  param knot, self-referential quantifier constraints); do not
  resurrect per-Fn-boundary scoping or cell provenance without a live
  witness.

## The rules in brief

**Types** (`graphix-types/src/typ/`): `Type` (structural),
`TVar` (inference cells; `design/tvar_constraints.md`), `FnType`.
`contains` expands `Type::Ref` through `lookup_ref`, so bindings made
during `contains` hold the EXPANDED form — code inspecting resolved types
handles both. `TypeRef` carries a write-once resolution cell
(`design/env_independent_typerefs.md`): rebuilds share it via
`with_params`, `with_scope` makes a new one (filled when the source's
is), never overwrite a filled cell; the cell holds the definition
weakly and the `TypeDef` owns it (a recursive body reaches its own
cell), so a type outliving its definition's env entry is refused,
never re-resolved, and what outlives the entry owns the definitions
its types name (a definition's check table, a kernel's type
constant, an abstract type's registration, the block's `TypeDef` node:
`DefTable::typedefs`, `record::KernelType`, `AbstractRep::named`,
`node::TypeDef`). A typedef a block declares (a scope ending in a
generated component) stays findable after the block in `Env::
block_types`, by scope and name, held weakly and shared by every copy
of the env: a lookup at a block level no lexical map holds reads it, so
does one at a compiling block's level (or a snapshot of it) for a name
the block declares later (`Env::open_block`), and one
that meets a dead entry is refused, never continued outward; a block compiled again declares the live definition, so one
source typedef is one definition; an image carries the live entries. A
scope an expression opens is named by where it is written and its
source's load (`place_component`, `Origin::tag`), never by an id, so a
restore names a block as the cold compile did (design/program_image.md,
Scope names).
`Env::seed_typedef_refs` (block types included) runs at the check's end
(an instance elaborates under its definition's env) and again right
before fusion in both modes, and every node outside a definition has
its type's names filled then (`seed_node_types`; a definition's rows at
its check, `DefTable::record`). A bound's ref resolves the same way, its
cell holding the trait (`Resolution::Trait`), filled by the seed walk at
a definition's check. Substitution by name (`Type::replace_tvars`: a
typedef's expansion, an abstract's representation, a trait method's
signature for an impl or a method value) is capture-avoiding,
since a signature finds its quantifiers by name: under a function type
its quantifiers shadow the names substituted, and one named like a
variable a substituted type holds is renamed (`tvar::under_quantifiers`).
A trait method named as a value is its
eta-expansion (`eta_dispatcher`); a dispatcher is only a call's own
function. A typedef must be contractive: every
self-reference sits under a constructor (`type T = [i64, T]` is refused
at `Env::deftype`), which is what makes the coinductive ref-pair memos
sound. And it must not be expansive (Kennedy and Pierce):
no parameter flows back to its own definition wrapped in a constructor,
through any chain of the definitions and abstract representations it
reaches (`typ::expansive`; ``type N<'a> = [`Nil, `Cons('a,
N<Array<'a>>)]``, `type P<'a> = [`L('a), `N(P<('a, 'a)>)]`, and `type A<'a>
= [`N, `C(B<'a>)]; type B<'b> = A<Array<'b>>` at B, are refused at
`Env::deftype`, naming the definition and the parameter). A parameter
passed on bare, to any position, or as a bare member of a union (unions
flatten, so `T<['a, string]>` applied again is itself) flows plainly. An alias applied to a
parameter wraps nothing, but walks compare applications as written, and
as written `A<I<'a>>` unfolds to `A<I<I<'a>>>`. So a definition stores
such an argument as the alias's expansion (`expansive::through_aliases`,
at `Env::deftype`): in each application that may unfold back to the
definition (it names the definition, a name not defined yet, or a
definition no walk has closed), an argument's member that applies a
typedef to a parameter becomes its expansion where that leaves every
parameter bare in a union or absent. `T<Opt<'a>>` over `type Opt<'a> =
['a, null]` is stored as `T<['a, null]>`, an alias of an alias and a
50-member alias alike, and no walk knows of it. An application of a
closed definition cannot come back, so its arguments stay as written and
its unfolding prints as before. An alias whose expansion holds the
parameter under a constructor stays as written and grows the type. One
that cannot be looked through where it is applied, because it is defined
below the definition (a name its block or module declares below is that
type, never one a `use` or a prelude brings: `Lexical::block_names`; a
name a `use` below imports is not read before the `use`:
`Env::open_list`) or
because it bounds a parameter of its own (only
its application holds the bound), stays as written and counts as a
constructor; a cycle through it is refused naming the alias
(`Expansive::Through`), since looking later would leave every walk made
between the two definitions an unfolding that nests. So
every type unfolds to finitely many applications, and every walk over
types ends at a pair it meets again, with no growth cut. A definition
whose body names something not defined yet is checked again once it is
(`Pending::Typedef`, `Env::check_typedef`), as contractiveness is: at a
`use` and at the check's end whole, at a later parametric typedef only
its self-references (`Completed::Typedef`: every flow that typedef makes
visible runs through its own parameters, so its own check refuses a
growing cycle it completes). A walk that meets no undefined name settles
every definition it reached whose parameters do not grow, so the checks
of N forward-referencing typedefs walk each about once. An inferred type is finite the same way: a relation that would
put a cell under a constructor of itself is refused (`b <- B([t ~
b.0])`); a lower bound's check follows its direction, and the cells
on a cycle through cells and unions alone are one type, merged
(`tvar::cycle_toward`, `contains::merge_bare_cycle`,
`design/tvar_constraints.md`). A rigid cell holds only itself, ⊥ and
what one of its lower bounds holds (`contains::rigid_holds_by_lower`): an open
cell a merge would put below it merges into it, or, where the occurs
check refuses that merge, stays a lower bound (`TVar::merge_into`). It
holds a union member by member, probed whole before any member commits:
`'e ⊇ ['p, 'q]` makes both open cells `'e` (a slice pattern's element
cells over `Array<'e>`, two arms' results under a declared `-> 'e`), and
`'e ⊇ ['p, i64]` is refused with `'p` left open.
A cast to an integer type, a type-directed read's included, is an
`InvalidCast` where the value does not fit (a float drops its fraction);
a float cast to `decimal` is the decimal its shortest text names,
rounded to 28 places, a whole one keeping its point and a zero (one
too small for 28 places included) `0.0` with its sign (`float_decimal`). A cast leaves a value's runtime class
only to parse a string into a scalar (never a collection), to print a number, bool, datetime or duration
plainly (`"42"`; a whole float, and a duration's seconds, keep the
point, `"3.0"`, as every plain print does: `typ::PlainScalar`), between a number and a datetime (Unix seconds) or a
duration (seconds), from a bool to a number (1 or 0), or to read an array of numbers as bytes; a list reads as the array of its elements and a map as the array of its `(key, value)` pairs, and an array of pairs reads as a map. A union target reads by its members past every union and name in it, its primitives, written or named, one set whose parts each go in their class (`typ/cast.rs::cast_member`, `members_by_class`, `merge_primitives`), in groups: an array the source type says is one (`cast_from`) an array member it inhabits; a member whose tag the value carries; the members of the value's own class (`in_own_class`: a container's class is its elements' too, and unmarked array data is a list's; a tuple of the data's length, then a struct, first, only where its elements stay in their classes); for text the members of the class of the scalar it parses as (`parsed_scalar`: integer text too long for every integer type is the decimal it names, else the nearest f64); the rest. Within a group a member that holds the value exactly comes first, then a real one that holds it rounded, then one that drops part of it (`Holds`, `Type::tier`; a bool is held exactly by 1 or 0), and short of a dropped part one keeping every number an integer or a real as it was before one reading it across (`Choice`); canonical order breaks a tie. An infinity a finite value overflows to is none of these: a member reads it only once every group failed short of it (`Holds::Overflowed`), so `cast<[f32, string]>(1e300)` is the string and `cast<[f32, i64]>(1e300)` f32 inf. So `cast<[f32, string]>(0.1)` is f32, `cast<[i64, string]>(3.5)` 3, `cast<[i64, f64]>("3.5")` 3.5, `cast<[duration, decimal]>("3.0")` decimal and `cast<[f32, u8]>(true)` u8 1. The JIT inlines
only the casts that always fit (`lowering::inline_cast`). A struct cast
reads a field the data omits as null wherever the field's type
admits null (`typ/cast.rs`), and a string parses as an RFC3339
datetime, so what the json and toml writers emit reads back into its
type. An abstract type crosses data only through its core `Cast` impl
(`design/abstract_cast.md`): casts and reads make it with `from_value`
(a value already of the type is held), writers write its `to_value`
(`abstract_value::to_data`), only the defining module implements it
and a method it leaves out is the representation; a type with no impl,
a Rust-backed one included, is refused by the check as a cast or read
target (`Concrete`) and as a written value (`'a: Data`), and an abstract
value decodes only under an image session. JSON and TOML write one as an
object of one entry keyed by its type's unqualified name, its data
inside, and read it only so (`core::Form::Text`, `CastInput::Text`);
the data inside is in the form its reader reads: with abstract values
in it wrapped where the representation is read, bare where a written
`from_value` casts it (`core::Form::Data`). Core's `Transparent<T>` is
written as what `T` writes without its object, and elsewhere is its
representation. They write a map as an object (a table) only under the
key type `string`, its `[key, value]` pairs under any other or none;
template data (hbs, `core::Form::Template`) writes abstract values bare
and a map whose keys are scalars that print apart as an object
(`core::map_shape`).
A reference is its own runtime form (`graphix_types::reference`, an
abstract holding its cell), never a number: a cast whose source can
hold one is refused (`Type::holds_ref`), and where only an instance
knows the source, the cast yields its `InvalidCast` error; a reference
prints as `&ref`; under `Any` a reference test is refused, since
references to different types share the form (`Type::ref_over_any`);
nothing orders references or hashes them by value (`Ordered`).
Format type variables with `format_with_flags(PrintFlag::DerefTVars, ..)`;
under it a union's members that name one cell or print alike (two cells
bound to one type) print once. A type that shares its parts prints as a
tree, 2^depth long, so a print where cells stand (`DerefTVars` or
`StandingHeaders`) stops at `typ::Brief::MOST` bytes, `…` where it is
cut, and a computed type a refusal names (a mismatch's two sides, what a
select's arms leave) prints through `typ::Brief`, cut at `Brief::BYTES`.
A hint's declaration writes a type as a
program can (`Type::written_form`): an open cell as `'x` for the binding
`x` it waits for (`node::Holders`), an unnamed referent's as `'r` for the
binding `r` holding its reference, else as the variable a program wrote,
else none, and the hint names no type; it visits a shared node once, and
a declaration printing longer than `Brief::DECLARATION` is not written
(the hint asks for "a type holding both"). A refused write into a payload
an earlier use decided is told both: the payload union, then a named
recursive type for a value that reads the target; one whose own
references name the target is told the recursive type alone
(`node::write_mismatch`, `design/tvar_constraints.md`).

- **`let rec` is monomorphic-recursive**, and a binding something writes
  (a `<-` target, a `&mut`'s referent, as the resolver names it) is not
  generalized: its uses and writers share one instance of each function
  it holds (the value restriction, `CompileCtx::monomorphic`; a node its
  value's type shares is one instance wherever it stands, so the instance
  keeps the value's sharing, but each place a typedef application copied
  as its expansion names is copied apart; a name aliasing a generalized binding, `let g
  = f`, holds an instance of its own, as does a written name a pattern
  takes apart). A write compiled
  before the binding's check makes its check take one instance
  (`Bind::typecheck0`); a writer in its block, a lambda body's included
  (a body compiles at its definition's check), and a module's in it
  through `super::`, a name it imports from there or a `package::` path
  through the block's module, `self::` in a module's top block, keeps it
  from being generalized at all (`node::written_lets`: the nearest
  binding before the write; under a nested block's glob only a lambda
  body's write, the rest left to the resolver). A binding
  generalized before a write to it compiles (an earlier REPL input, or a
  function body's write the walk missed) stays generic: the
  write is held to a copy of its type with the variables rigid, a
  bound's included, each function type's apart whatever their names (a
  typedef application as its expansion, past 64 in one copy as one), and
  a `&mut` of it, or of a place whose type holds one of them, is
  refused
  (`Env::poly_binds`); a `let` nothing writes whose value
  holds a lambda or a generalized binding's value in a data literal (a
  struct, its update, a tuple, a variant, an array, a list, a map:
  `bind::holds_function`) is generalized too: each read copies the
  closed definitions' signatures its type holds (`Type::scheme_read`), so
  an annotation, a typed formal or a site's settle decides only that
  read's copy, never the definition; every read in such a holder's value
  (a call's argument, a block, a select arm, a lambda body included)
  copies through one cell map, so reads of one binding share one copy
  and a chain of holders holds one copy of its last link
  (`bind::ValueReads::Shared`, `ReadCopies`), but an annotated `let`'s or
  one binding a written name copies at each read, and a `let` forwarding a
  holder gives each name a copy of its own beside a written one; two
  lambda literals in one array or list meet, one type of the array's own (a
  definition's type is untouched, however many met), whose label a call
  through it or a formal holding it judges each one's labeled defaults
  against, in the order the program writes the lambdas
  (`LambdaDef::written_order`: a session's inputs in the order given, a
  load's sources by origin, then position), a refusal naming the annotation
  (`callsite::met_default_note`);
  a type written
  in a body (a `let`
  annotation, `never<T>`, a select arm's type test, a nested lambda's
  signature) names one
  variable per name, and a name a definition around it declares, and
  the written place does not declare itself, is that definition's
  variable, any other the written type's own: generic until a cell of
  a definition or the top level merges into it, which claims it there
  (`tvar::Level::Written`; `lambda::body_annotation`, `enclosing_tvars`; a labeled
  default's, the definitions' around its own, `LambdaDef::in_signature`;
  in an instance, the instance's copy of each cell its definition's
  signature writes, `lambda::written_tvars`, never a cell its check
  inferred, whatever name a callee's variable or a body's annotation gave
  it); a quantifier
  a definition or a written `fn<..>` declares must be held by its
  signature (a parameter, the return, the throws, a held quantifier's
  bound), else it is refused (`FnType::check_quantifiers_held`), and a
  quantifier's bound (a definition's, a written `fn<..>`'s, an impl
  head's) may not reach it, directly, under a constructor or through
  another's bound (`'a: 'a`, `'a: Array<'a>`, `'a: 'b, 'b: 'a`;
  `check_bounds_acyclic`): a recursive bound is a recursive type; a
  written function type (a lambda's signature, a `.gxi` val, a trait
  method, one a body's annotation writes) holds no union written with two
  or more of its own bare variables (`['a, 'b]`, under any constructor as
  written; a typedef is not looked through; a trait's `self` and an impl's
  head are no method's own, and an impl's `self` is its target), nor two
  members of one constructor holding two distinct
  ones (`[Array<'c>, Array<'d>]`), refused, naming one variable or written
  types as the fix
  (`Type::variable_union`); a type that two undecided members of a union
  could take goes to neither, refused naming what holds the union
  (`AmbiguousMember`); a def's declared tvars are
  rigid in its body check: none binds to a concrete type or ⊥, no
  two unify (`contains.rs` Distinct) and none escapes into a cell outside
  the definition (`DefGate::check_declared_kept`; a variable the signature
  only names, undeclared, is lowered with that cell, and the definition is
  monomorphic in it; in a function something writes, a part of its signature
  left unwritten is such a cell, and the refusal names the annotation), so a function type without
  `throws` (which throws nothing) refuses one that throws `'e`, while a call instantiates them
  freely; a call copies only what the callee's definition owns once
  its gate is closed (or not yet open): a cell shared with the
  environment, or an open gate's, is shared (`let t = |x| x + y` is
  monomorphic in `y`'s cell; design/tvar_constraints.md,
  Generalization); a labeled default is checked at the definition
  against its parameter's type, or a declared tvar's constraints, a
  variable nested in the type taking any instance (`check_defaults`),
  and again at each omitting site by the check, where it may narrow
  that site's cells: against the site's own view of the parameter, the
  type the instance runs with, and only where a fn type hides the
  label against the callee's parameter as that type instantiates it
  (arguments, result and throws) and the label of the function value it
  hides, which the default narrows; compiled at a bind, it takes its
  types from its definition's check, as an instance's body does (the
  definition's `DefTable` holds the defaults' rows,
  `InstanceTypes::of_default`, `DefTable::record_defaults`). A default whose type holds an enclosing
  definition's open cell requires a closed parameter type (annotate the
  factory's parameter or the defaulted parameter); an enclosing definition's
  declared variable is closed: such a default fits its parameter whole, for
  every instance of the variable (an unannotated parameter takes its type),
  and a site outside that definition judges it over a copy of the scheme
  (`callsite::Judge`). A call's arguments are met in the order of the
  parameters, then the defaults it omits, and nothing waits (A call's
  arguments are met in order): a type variable that only data positions
  or read-only references hold widens to a decided argument wider than
  the ones before it; one neither wider nor narrower is the mismatch; an
  argument or an omitted default holding an undecided cell outside
  function types (a read target, a `let` over `never()`, `[]`'s element;
  never a lambda's, `Type::has_free_in_data`) takes its formal as it
  stands, and the variables it met widen no more (`Widening::take`,
  `pin`); a decided argument that two undecided members of its formal's
  union could take is refused where it is written, naming the remedy
  (`callsite::ambiguous_parameter`). One a
  callback holds settles to the first argument, and one an abstract
  type, a type application or a writable reference holds is fixed there, those
  arguments checked first whatever the order the call writes (a
  transparent typedef is its expansion)
  (`callsite.rs::Widening`, `holds_invariantly`); a function held at a
  function type meets the type's positions in the order written, and
  the first that meets a variable decides it
  (`FnType::contains_gated`); a function type with
  type variables of its own (a written `fn<'b: C>(..)`, a trait as a
  parameter's type, `fn(c: Collection)`, a constructor-trait quantifier)
  stands only as a declared signature's top (an interface `val`'s, a
  trait method's, an impl's or a default body's method `let`, a lambda's
  own declared variables): anywhere else in a written type it is refused,
  naming the fix (`typ::QUANTIFIED_FN`: `Type::rewrite_trait_args`, and
  `refuse_quantified_fns` for a cast, a type test and a catch's
  annotation); a function stays generic only where a binding names it (a
  `let` whose value, past parentheses and a block's final expression, is
  a lambda, a generalized binding, a field or tuple read of one
  (`let j = s.g`) or a data literal holding these, a write to a
  generalized binding, a labeled default, a call's callee by name, an
  input's value: `bind::names_generic`, `holds_function`, `name_value`;
  a reference, `&f`, is not one): a lambda no binding names takes its
  enclosing level's cells at its gate's close (`Lambda::named`,
  `DefGate::close`), and a read of a generalized binding no binding names
  is one instance (`Ref::naming`, `FnType::one_instance`,
  `Type::instance_read`), so an element of a collection a call returns, a
  callback's result or a field of a call's result is one type its uses
  decide; a call copies a quantifier only where its level says so, as any
  cell, and a containment holds rigid a scheme's quantifiers alone; a
  quantifier an earlier compile
  task left open is one a later task's `contains` compares as a fresh
  copy on either side and never writes (`FnType::has_earlier_quantifier`:
  parallel elaborations share the check's cells; not in a top-level
  input's own check, to which an earlier input's is one type), and an open quantifier
  never binds to its bound, no site's settle decides it; a copy of a type
  keeps each written variable name, so a parameter merged into a declared
  quantifier still names it (`TVar::named_as`), and a name whose cell a
  merge forwarded stands for the cell at the end of the link: a copy names
  that cell's image after it, and a rigid gate holds that cell
  (`TVar::forwarded_as`, `open_rigid`; `design/tvar_constraints.md`);
  `Any` binds an open
  cell only in a contravariant position (a `|v: Any|` callback fixes its
  formal's type; `let x: Any = y` decides nothing of `y`);
  union collapse requires strict tvar identity (in a `Type::union` alone
  two function types pair their cells as a renaming, generic with generic
  or live with live, never a generic cell with a live one,
  `setops::Identity::cells`, and it merges the live pairs,
  `setops::union_collapses`; any other flattening keeps them two members,
  `normalize::Members`, and so does that union inside a function type's
  signature, `normalize::NormCx::in_signature`; a written union tells
  variables by name,
  `Type::flatten_written`;
  a field read of a union, a select's arms and an or-pattern's name meet
  their function members, through a copy of a member's generic cells,
  binding its live ones, `Type::union_of_alternatives`); a merge takes the
  lower of its two cells' levels, a generic survivor included
  (`TVar::merge_one`), so a relation against a scheme instantiates it first
  (`callsite::checked_default`, `Impl::build_prototypes`); in a union checked against
  a union, an open member takes only the members its known siblings leave
  (`[&i64, Error<E>] ⊇ ['b, Error<E>]` gives 'b `&i64`, never `Error<E>`,
  all of them when none are left or what is left refuses it), and where two
  of its members hold open cells (`[Array<'a>, Array<'b>, null]`) the other
  union's members go in as written, else deepest first, whichever order
  places them all on copies (`Type::place_each`); a free union member stays free (a type test over an
  untyped parameter binds it: annotate the parameter, not the arms); float comparison is a total order (`NaN ==
  NaN`, below every number) so `Value` is map-key-able, and numbers of
  different types (under `Any`) order by value, exactly, ties by type,
  every number before every other kind (netidx-value's `Value::cmp`); checked arith
  (`+?` …) yields a catchable `ArithError` naming its cause, except
  over floats, which follow IEEE and never fail (inf, NaN), unchecked
  `+ - *` wrap, integer div0 and `MIN / -1` bottom, `x % -1` is 0; indexing is bounds-checked through shared helpers on
  both backends; `$` and handler-less `?` report a fired error operand,
  with no report-count or at-least-once guarantee inside a fused region:
  an unreached prologue guard may report, a constant in a late arm may
  never report, and native tail depths may share one report
  (`design/strict_fusion.md`; `tests/fusion_diagnostics.rs`).
  Unchecked-arith diagnostics, `sum`/`product`/`divide`'s
  included, are node-walk-only (debug with `--no-fusion`).

- **Operators**: `* / %` (and checked forms) bind tightest, then `+ -`,
  comparisons, `== !=`, `&&`, `||`, `~ ~!`; every binary operator is
  left-associative (`8 / 2 * 2` is 8), and a lambda may be an operator's
  last operand, its body taking the rest (`t ~ |x| x + 1`). `BinOp` (`expr/binop.rs`) is the
  one table. Arithmetic (`fn<'a: Number + Singleton>(x: 'a, y: 'a) ->
  'a`) and comparison (`fn('a, 'a) -> bool`) take operands of EXACTLY
  one type, each containing the other's (`node/op.rs::operand_type`):
  `[i64, null] == 3`, `` [`A, `B] == `A `` and `[i64, f64] + 1` are
  refused; a ⊥ operand, or one only ⊥ was produced into
  (`TVar::bottom_fed`, `CellFact::BottomFed`), takes the other's type. `Singleton` is a bound
  like `Concrete`: whatever binds the variable is no union, a primitive
  set of two or more included, so arithmetic over a type holding two
  numeric types is refused even against itself, and a generic
  definition's at the call, by the check; the open members of a union
  under it merge into one cell. Comparison takes any one type, unions
  and mixed numerics included, if it is `Discernible`; the orderings take
  `Ordered`, which refuses two numeric types. `OneNumber`, a written
  bound only, holds a type to at most one numeric type (`'a: [Number,
  null] + OneNumber`); a binding with a numeric member beside an open one
  is refused at the definition that leaves it open (annotate it). `NoError` holds a type to no member a value of
  which is an error (`Error<..>`, the primitive `error`, `Any`):
  `type Result<'r: NoError, 'e>`, so a result's error member is its only
  error (`design/tvar_constraints.md`). An interface declares every bound its
  implementation's variables carry, a typedef parameter's bound counting
  (`FnType::sig_matches`); the implementation may leave off a bound the
  interface declares.
