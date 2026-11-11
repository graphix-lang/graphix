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
leaves nothing behind. An open cell of a bound below a union binds to
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
and a level claim carry the list. The witness
of a cell with lower bounds is their union, so every settle binds it
there, one a ⊥ was produced into as well (`CellFact::BottomFed`)
included; a probe of a trait bound reads it so, and a commit settles it.

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
a body returning it as `'b` is refused. An
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
the variable a program wrote (`node::Holders`, `Type::written_form`); a
cell with neither has no written form, and the hint then names no type.
Two payloads of one Graphix-minted abstract type are declared as the
union of the parameter (`B<[P1, P2]>`), read through one application,
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
  T`). So no cell the parser builds holds itself, and the syntax codec
  refuses one that does rather than write it without its bound. Pins:
  `lang::types::a_quantifier_bounded_by_itself_is_refused` and its
  neighbours, `parser::test::a_bound_reaching_its_own_variable_is_refused`,
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

### A call's widening

A fresh instance's variable that only data positions hold
(`callsite::widenable`) settles to the widest argument, whatever the
order the call writes them in (`callsite::Widening`). Only arguments
holding no undecided cell outside function types take part in that join,
the defaults the call omits included, joined as written ones
(`Widening::join_default`); a lambda's open cells are not undecided data
(`Type::has_free_in_data`), so `({a: n, k: |x| x}, 1)` over `|x: {a: 'a,
k: fn(x: i64) -> i64}, y: 'a|` joins at once. An argument holding an
undecided cell (a read target, a `let` over `never()`, `[]`'s element)
pre-unifies with a copy of its formal in
which those variables are open and every other cell is the formal's own
(`Widening::opening`), so it decides none of them, and decides the
others, a variable a callback after it reads included, as it meets
them. Only the copy's own cells are opened: a widenable variable the
formal reaches through another variable's bound alone (`z: fn(q: 'a1) ->
i64` under `'a1: Array<'a3>`) is the formal's cell, and stays as the
arguments before it settled it (pins
`lang::types::callback_into_a_union_bound_beside_a_function_over_the_bounded_variable`,
`variable_settled_before_a_function_over_its_holder_takes_no_other_type_refused`).
The argument is checked at the end of the argument loop, when the join is
known (`CallSite::check_waiting`); its cells take what the join leaves
them. What it took from the copy is the formal's own by then: before the
check, each cell of the copy that nothing decided is made the formal's
cell it copies (`callsite::rejoin`), a variable only a bound holds
included, so the call has one type for each variable and the instance is
built at it (`f([], never())` over `'a0: [Array<P<'a1, 'a2>>, null], 'a1:
P<'a2, 'a3>, 'a2: T<'a3>, 'a3: i64 |x0: 'a0, x3: 'a3|`). A copy's cell
that the formal's cell reaches stays apart (`TVar::rejoin`): in `|y|
h([y, 1], (y, 2))` over `|a: Array<'a>, b: ('a, i64)|` the join put `y`'s
cell inside `'a`, and the two as one cell would be an infinite type (pin
`lang::types::undecided_argument_inside_what_its_formals_variable_settled_to`).
An argument expression whose type its own check has yet to make (a seq step's
captured argument, a field read) is not pre-unified with the copy: its
check would commit the copy's bounds before the other arguments decide
them. A binding's type stands already (a wrapper's parameter), so it
pre-unifies and takes the copy's shape, which an argument after it reads
(`|y| f(y, y.0)` over `|x: ('a, i64), z: 'a|`; pin
`lang::types::wrapper_parameter_read_by_a_later_argument_of_the_same_call`).
A formal that is an inferred parameter's cell is opened as what the cell
is bound to (`Widening::standing`), so an undecided argument waits
through a wrapper as at the direct call: `g([], 1, "s")` over `let g =
|u, v, w| f(u, v, w)` and `let f = |x: P<'a, 'b>, y: 'a, z: 'b|` (pin
`lang::types::wrapper_empty_array_into_a_typedef_union_is_decided_by_the_other_parameters`).
An omitted default
holding an undecided cell waits the same way,
checked at its formal once the join is known, whichever label comes
first. A written argument or an omitted default that is neither wider
nor narrower than what the others gave a variable waits too, and is
checked once they have widened it. Order can then decide only between a
value and a refusal, never between two types: where the join does not
hold the argument (`f(str::parse(s), 1)` over `|x: 'a, y: 'a|`: `'a` is
`i64`, which holds no parse error), the mismatch is refused, in either
order, and where some decision of the argument's cells outside function
types could fit (`callsite::annotation_may_fit`: on copies, it fits the
formal with the variables fresh and what it gives each holds or is held
by what the call gave it), the refusal says "annotate it", naming the
bindings the argument, or the omitted default, reads whose types are
undecided (`callsite::undecided_note`, `undecided_binds`); `f(1, [])` is a
plain mismatch. Two
undecided arguments at one variable: the second meets what the first
gave it, and its refusal says the same.

An argument with an undecided cell and nothing else at its variable
(`id(u)`) takes the variable as it is: one cell, decided by whichever
use comes first, as any cell is. A variable a callback also holds is
not widenable: the first argument decides it, an undecided one
included, so `f(str::parse(s), 1, |v| 0)` over `|x: 'a, y: 'a, k: fn(v:
'a) -> i64|` runs and `f(1, str::parse(s), |v| 0)` is refused. Annotating
the undecided argument widens nothing there, so where a wider decision
of the variable could hold it (`annotation_may_fit` over the variables
an earlier argument of this call decided: written ones, that the formal
holds outside function types), the refusal names the argument that
decided it, "annotate that argument as wide as this one"
(`callsite::decided_elsewhere`); `f(1, [], |v| 0)`, an undecided
argument at a formal with no variable or at a callback formal, and one
at a variable decided before the call (by an earlier call of one
instance, as in a seq step, or a `let rec` body) are plain mismatches. An argument or an omitted default whose type holds a function
type with open cells does not widen either (`Widening::widen` skips a
type with an unbound cell): it fits the variable as the arguments before
it left it, or is refused, so order decides between a value and a
refusal there: `f({n: 1, k}, {n: w, k})` over `|x: 'a, y: 'a|` with `w:
[i64, null]` is refused, naming nothing, and `f({n: w, k}, {n: 1, k})`
runs; an omitted default is met after every written argument.

### A function held at a function type

A function type that must hold a function (an annotated `let`, a
callback's formal, a field, a write) holds it position by position
(`FnType::contains_gated`), and a variable of the function still open
there is decided by what the positions give it. A variable that no
function type of the function's signature holds, nor a union beside
another undecided member (`FnType::order_free_cells`, over
`settle::DataPositions`), takes the widest of the types the argument
positions give it, whatever their order, as a call's arguments decide
it. Each part holding no undecided cell that the variable must hold
waits for the end of the arguments (`ContainsHist::defer`, `defers`);
there the variable meets the part that holds every other one first, then
the rest (`ContainsHist::settle_deferred`), and only then are the result
and the throws related. So `let g: fn(b: i64, k: Any) -> i64 = f` over
`let f = |b: 'y, k: 'y| 0` takes `'y` as `Any`, as it does with `k`
written first, and a narrower written result (`-> 'y` held at `-> i64`
beside `k: [i64, null]`) is refused in either order. Two parts of one
position (`('y, 'y)` held at `(i64, Any)`) are two parts.

- With no part that holds the others (`fn(b: i64, k: string)`) the parts
  meet the variable in the order written and the second is refused, in
  either order, as a call's arguments are.
- A part holding an undecided cell does not wait: it meets the variable
  where it is written, and the parts that waited then meet what it left.
- A position that holds the variable exactly (a writable reference, a
  nominal type's or a constructor application's parameter) binds it when
  it is met, and the parts that waited must fit that: `fn(k: i64, p:
  &mut [i64, null])` holds `|k: 'y, p: &mut 'y|` in either order.
- A variable a function type holds, a callback's parameter or result, is
  decided by the first position that meets it, as at a call (above): the
  held type writes its earlier position as wide as the later ones need.
  A typedef application met inside its own expansion (`S<'b, 'a>` in
  `S<'a, 'b>`) counts as holding its parameters so, its expansion being
  unwalked. A variable in a union beside another undecided member
  (`['b, Array<'b>]`) is decided in order too: which member takes a type
  depends on what the cells are.
- A variable in an application of a typedef that reaches itself waits
  as any other does where the typedef holds it as data: two applications
  over an open variable are related through their expansions, so over
  `type T<'a> = [`Leaf('a), `Node(T<i64>)]`, `fn(t: T<i64>, k: [i64,
  null])` holds `|t: T<'y>, k: 'y|` in either order. One the typedef
  passes on to its own application (`type L<'a> = [`Nil, `Cons('a,
  L<'a>)]`) is met inside its own expansion, the case above, and keeps
  the first position: `fn(k: [i64, null], l: L<i64>)` holds `|k: 'y, l:
  L<'y>|` and the same with `l` written first is refused (pins
  `lang::functions::written_fn_type_recursive_typedef_*`).

The pre-unification of a callback argument with its formal
(`FnType::pre_unify_params`) defers the same way, so `app(f)` over `h:
fn(b: i64, k: Any) -> i64` fits as the `let` does. A pair proven while
a part of it waited holds only if the part does: where a part is refused
at the end, the walk keeps none of the pairs it proved
(`ContainsHist::settle_deferred`).

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
  `[Array<'a>, null] ⊇ null`), takes it and binds nothing;
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
  before any undecided one.

A type that is itself undecided is no exception: `[Array<'a>, Array<'b>]
⊇ Array<'e>` gives `'e` to neither, so `f([])` over `|x: P<'a, 'b>|`
(`type P<'p, 'q> = [Array<'p>, Array<'q>]`) is refused, and runs where
the other parameters decide the members. Nothing gives an argument to a
member before it is decided: a pre-unification that meets the choice
leaves it to the argument's own check (`Type::pre_unify_arg`), a decided
member beside another that could take the undecided argument included
(`ContainsHist::early`: `g(["s"], [[["s"]]])` through a wrapper, where the
first argument has decided one member by the time the second
pre-unifies; `f(["a"])` over `|x: [A, null]|` with `type A = [Array<i64>,
Array<string>]`, where the first member took the literal before its
element was checked), and the
probes behind a refusal's note are ordered walks on copies
(`callsite::annotation_may_fit`). So a call through an inferred wrapper,
a nested function or a seq step is refused where the direct call is, at
the check, never at the instance (pins
`lang::types::{wrapper,nested_function}_into_a_bound_union_of_two_undecided_refused`
and their twins, `empty_array_into_a_typedef_union_of_two_undecided_arrays_refused`,
`seq_step_empty_array_into_a_bound_union_is_decided_by_the_other_parameters`,
`never_argument_beside_one_deciding_a_bound_union`,
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
`lang::types::declared_variable_into_a_union_with_a_named_member_refused`). An undecided rhs member takes a
union of undecided members whole, as it does alone (`['a, 'b] ⊇ 'c` binds
`'c`), and an equality relates such a union as the contained side first
(`equal_int`, `op::operand_type`: `u + 1` over `u: ['a, 'b]` decides
both), since what must hold it decides every member.

Two walks keep the first member that holds: a walk that binds copies
alone (`on_copies`, `place_in_order`'s trials, `contains_holding`,
`contains_through_copy`), whose answer the real walk judges again, and a
select's patterns (`contains_ordered`, `check_contains_ordered`: the
narrowing walk in `tvar.rs`, `pattern.rs`, `Select`'s predicate against
its scrutinee), whose arms are consulted top down. What a pattern leaves
in a binding's or a parameter's type is judged where a value next meets
it: `|x| select x { [h, t..] => h, [] => 0 }` types `x` as a union of
one array member an arm, so `f([4, 5])` is refused until the parameter is
written (`|x: Array<'e>|`), and so are two arms that bind elements (`[a,
b, ..] => a, [c] => c, [] => 0`); where one arm alone binds elements and
none the rest (`[h, ..] => h, [] => 0`) the members are one and the call
runs.

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
`lang::types::{write_into_a_union_of_two_undecided,argument_into_a_later_typedef_union,argument_into_an_identity_argument_union,argument_into_an_identity_body_union,slice_arms_over_an_unannotated_parameter}_refused*`
and their twins, `slice_arms_binding_no_rest_over_an_unannotated_parameter`, `slice_elements_bound_in_two_arms_over_an_unannotated_parameter_refused_at_the_call`, `{typedef_union_of_variables,identity_member,option_member,nested_union_typedef}_refused_at_its_use`,
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
generic operation passed around is a trait (`|c: Collection| ..`,
`'c: Collection`), or a definition's declared variable that each call
picks (`'a: Number |f: fn(x: 'a) -> 'a, x: 'a| f(x)`). Pins:
`lang::quantified` (each position refused, each kept form),
`lang::collection::collection_generic_formal_is_refused`.

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
  which the choice below judges. And
  a field read of a union meets the function members it reads
  (`data::struct_field_type`, `tuple_field_type`), as a select meets its
  arms (`Type::union_of_alternatives`): a member's generic cells meet
  through a copy, and its live cells bind, so a lambda literal in an arm
  is the instance the call through the union makes. Members one value
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
  `lambda_beside_an_optional_callback` and their neighbours). Where one
  member must hold a value (a union bound passing its parts down to the
  value's open cells, `contains::commit_parts`; a union formal judging a
  function's hidden defaults, `hidden_defaults_at`), it is the one member
  that takes the value's constructor (`Type::takes_constructor_of`): a
  writable reference takes no read-only one, and two that take it are no
  choice, so neither is judged, unless the value is a function and a
  probe admits it into one alone, which then has the parts: a callback
  whose result decides
  between `fn(v: 'x) -> i64` and `fn(v: 'y) -> string` takes its
  parameter from that member (pins
  `lang::types::callback_into_{the_second_member_of_a_union_bound,a_union_bound_takes_the_members_parameter}`).
  One case of two is decided: a union bound
  with a read-only and a writable reference member (`'a: [&[i64, null],
  &mut string]`) passes a writable reference that holds a cell to decide
  (`&mut z`, `z` not yet typed) to the writable member, which decides the
  cell, so `z` is a string; a decided one (`z: i64`) is left to the
  member the probe found (pins `lang::types::union_bound_*`). A written
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
  caller's annotation or the terminal settle decides them.
- An OMITTED labeled default is checked at its site by the check
  (`CallSite::check_omitted_defaults`): the default of every definition
  the callee's `lambda_ids` names is compiled as the site sees it,
  checked against the site's argument type, and discarded; elaboration
  compiles it again for the bind and finds the cells decided. When a
  definition is not known at the check (a fn-typed parameter, a
  runtime definition), the cells the omitted defaults reach are exempt
  from the site's settle and the default binds them at the bind. Where
  the site's fn type hides the label, the bind's instance runs at a copy
  of the callee's signature fitted to the site's, and the default the
  bind compiled is checked over that copy's label, so the instance's
  body types see what it decides (`CallSite::instance_signature`; pin
  `lang::functions::default_hidden_by_a_fn_type_types_the_instance`).
- The terminal settle (`FnType::settle_terminal`, the check's drain)
  walks the LIVE ftype in dependency order, the cells its bindings hold
  as positions included and every cell a chain of bounds reaches from
  them, however deep, each cell's binding walked once (pin
  `typ::settle::test::a_terminal_settle_decides_a_bound_of_a_bound`), and settles
  whatever remains. A cell still
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
decided `'x` stands beside the member that holds the literal's type.
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

A partial struct pattern's completion is the arm's run-time test, so
members its left-out fields tell apart only by undecided cells are two
members (`strictly_identical`), refused as any ambiguous completion is,
unless what reaches the arm leaves one: annotate the cells, the member,
or name every field; a program whose cells end up equal is refused
until annotated.

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
`one_name_behind_a_wrapper_past_the_pair_budget_*`). One unfolding and
1000 pairs were chosen from the 1600 generated selects (review-r8
X-r1-1): of main's 1547 finishes none changes verdict or the kind of its
message, 52 of its 53 timeouts end, the last in matching_type's
containment; 100 pairs moved two of main's verdicts (a dead arm accepted, a
dead-arm refusal become a missing case), 10000 pairs cost a generated
select (rnd13) 3.4G instructions where 1000 cost 1.7G. Over Error cycles
of coprime lengths diff walks pairs that grow with the lengths, not their
lcm (pin
`typ::setops::tests::diff_over_error_cycles_walks_pairs_by_their_lengths`).

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
`lang::types::open_applications_of_recursive_typedefs_check`). Two
instances of one definition over an open cell take the same walk
(`design/env_independent_typerefs.md`, Same definition).

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
parameter take every edge. Where two cells meet (`contains_tvars`):

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
- An open cell meeting a bound cell whose binding reaches it walks the
  binding, where a union collapses (`'r ⊇ [T, 'r]` binds `'r := T`;
  pins `lang::types::rec_block_*_collapses`).
- A path through cells and unions alone otherwise (`'p` above `'q`
  above `'p`): every cell on it is one type (`merge_bare_cycle`, the
  paths' vars from `tvar::bare_path`; every path, so a cycle through
  three cells merges all three). The open cells merge, each first
  dropping its bounds that are cells of the cycle: a union lower bound
  holding one keeps its other members, as `'r ⊇ [T, 'r]` collapses, and
  a conjunct union holding one always holds. Each bound cell on it is
  the merged cell (`'r = [T, 'r]` gives `'r ⊇ T`). Two declared
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
  (`Type::place_each`).
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
definition's by `FnType` containment, which decides a parameter or a
result typed by a union of two open cells only after every other position
(`fntyp::holds_open_union`): the positions that hold each member alone
decide it, a parameter's or the result's, and the union then holds what
they left. A written function type holds no such union (below, A union of
variables in a signature), so only an inferred one meets that order (`|c,
a, b| select c { true => a, false => b }`), and a type that either member
could take is refused by both orders alike (A union's member takes a type
by rule).

**An instance holds its signature cell by cell.** The instance's
signature is the definition's at the types the call's check decided, and
the call may have decided a union's members from other positions than the
containment meets first: an undecided argument (`never()`, `[]`) waits for
the others, and a union a typedef hides (`E<'a, 'b>`) is not held back as
a written one is. So where the containment fails, the instance's copy
takes each cell from what stands at its own place in the instance's
signature, wherever the two are built alike (`Type::place_cells`: a named
type's parameters, an array's element, a tuple's, a struct's, a function's
positions; a union's members have no places; a place holding ⊥ gives its
cell ⊥), and the containment then judges the whole (`FnType::held_at`).
Where that still has to choose, the cells no position holds, a variable
only bounds name, are first settled to their bounds, the deepest first
(`TVar::settle_bounds_first`), as the call's check leaves a variable it
did not decide; the members are then decided and the containment has no
choice to make (pins
`lang::types::instance_{cells_only_bounds_hold_settle_to_their_bounds,cell_at_a_bottom_place_is_bottom}`). Without it the build refused what
the check accepted: `let f = |x: E<'a, 'b>, y: 'a, z: 'b| ..; f(never(),
1, "s")`, or `|x: ['a, Error<'e>]| -> (Array<'a>, Array<'e>) ..` called
with an `` [Error<`A>, Error<`B>] ``. Pins:
`lang::types::{undecided_argument_into_a_typedef_union_is_decided_by_the_other_parameters,empty_array_into_a_typedef_union_is_decided_by_the_other_parameters,empty_array_into_a_typedef_union_of_two_arrays_is_decided_by_the_other_parameters,undecided_argument_into_a_typedef_union_is_decided_by_the_result,two_members_one_constructor_member_could_take_are_placed_by_the_result}`.

A type written in a body names the variables of the definitions around
it (`lambda::body_annotation`); a labeled default is written in its
definition's signature, so its types name the variables of the
definitions around that one (`LambdaDef::in_signature`). An instance's
body compiles under its definition's signature as the instance holds it
(`lambda::instance_sig`: each variable copied under its name and bound to
the instance's type). A default compiled at a call's bind takes its types
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
  admitted.
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
it in its signature.

- `NoError ⊇ t` (`Type::may_be_error` negated) reads `t`'s members
  through bindings and type references; an error nested in a collection
  is no member. An open cell is admitted.
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
nothing, and a bounded variable inside it is not judged
(`iface-bound-shape-01`).

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
  (`PrintFlag::DerefTVars`: a checker message, the shell's `-:` line) it
  lists a quantifier only while its cell is open and generic
  (`fntyp::printed_quantifiers`), and one a call picked or one instance
  holds (an unnamed lambda's, a read no binding names, a call's result)
  prints where it occurs, like an inferred variable; canonical printing
  lists every declared one. With no header to scope it, a named cell
  keeps one name through one printed type and a second cell of that name
  prints numbered apart (`'a1`; `tvar::printed_name`, one table per
  outermost print, `tvar::Shown`, looked up by cell so a type of many
  variables prints in linear time), so two instances side by side read
  as two variables; a cell a header lists is that header's until the
  signature ends (`tvar::quantifiers_apart`), under whatever name a
  binding reaches it by, and its header takes no name the type already
  shows another cell under, so no two cells showing share one. A message
  about a generic binding itself prints its scheme
  (`Type::scheme_read`), never the one instance a comparison held rigid:
  the refused write to a generic binding (`Connect::typecheck0_with`)
  names it header and all.
  Pins: `lang::quantified::*_prints_no_header`,
  `two_instances_in_one_printed_type_*`,
  `a_printed_quantifier_names_its_cell_inside_a_binding_too`,
  `two_generic_functions_in_one_printed_type_keep_their_headers`,
  `a_header_prints_apart_from_an_instance_shown_before_it`,
  `a_header_prints_apart_from_a_name_numbered_before_it`,
  `a_returned_functions_variable_keeps_its_header_in_the_scheme`,
  `a_refused_write_to_a_generic_binding_prints_its_header`. A self-referential constraint
  `fn<'a: fn(x: 'a) -> _>(…)` is legal, and once seeded the declaring
  header and an inner fn that mentions `'a` reach the same cell and
  conjunct; each listing walk holds a per-signature reentrancy guard, so
  the regress stops at the signature already being listed.
- A nest of signatures is read once, however many of its signatures
  ask. Listing a signature reads the cells it names
  (`collect_tvars`), and a quantified signature inside it answers with
  the cells it names from outside itself (`fntyp::free_cells`). Under
  the outermost walk, comparison or print (`PartsDepth`) that answer
  is kept per signature beside its position cells, and dropped at any
  cell write on the thread (`tvar::cell_writes`). Only an answer no
  reentrancy guard cut is kept, and one that names a cell is used only
  where no guard is held: an empty one is empty under every guard. The
  parser keeps a stronger fact for the length of the outermost
  signature's parse (`ParseDepth`): a signature whose declaration met
  no name but its own quantifiers is closed (`FnType::declaring`), no
  enclosing declaration aliases or seeds a cell of it, and the
  enclosing signatures' alias walks, held checks and position cells
  skip it, so a closed signature inside another is the signature it
  is alone. A nest where every signature names an outer variable is
  still aliased once per enclosing signature. Pins:
  `typ::fntyp::tests::nested_closed_signatures_parse_in_linear_steps`,
  `nested_quantified_types_compare_in_linear_steps`,
  `a_cell_write_drops_the_free_cells_read_before_it`,
  `a_closed_signature_nests_as_it_stands_alone`.
- `replace_auto_constrained` reads inferred `'_N` pairs from the cells
  directly (inference declares no header); `sig_matches_int`'s
  impl-constraint check is SATISFACTION — the
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
below the definition or because it bounds a parameter of its own (only
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
check refuses that merge, stays a lower bound (`TVar::merge_into`).
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
bound to one type) print once. A hint's declaration writes a type as a
program can (`Type::written_form`): an open cell as `'x` for the binding
`x` it waits for (`node::Holders`), an unnamed referent's as `'r` for the
binding `r` holding its reference, else as the variable a program wrote,
else none, and the hint names no type. A refused write into a payload
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
  in an instance, the instance's types under the definition's names,
  `lambda::instance_sig`, the names its signature writes alone, never one
  a body's annotation gave a cell its check reached); a quantifier
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
  (`callsite::Judge`). A call's type variable that only
  data positions or read-only references hold settles to the widest of the
  arguments holding no undecided cell and the defaults the call omits,
  whatever the order; an argument or an omitted default holding an
  undecided cell outside function types (a read target, a `let` over
  `never()`, `[]`'s element; never a lambda's, `Type::has_free_in_data`)
  decides its formal's other variables at once, waits for that join and
  takes the variable it settles, and is refused where the join, or an undecided argument before it, decided
  the variable and does not hold it, "annotate it", naming the bindings it
  reads, where a decision of its cells could fit (`Widening::opening`,
  `check_waiting`); a written argument or an omitted default neither wider
  nor narrower waits for the others to widen it. One a
  callback holds settles to the first argument, and one an abstract
  type, a type application or a writable reference holds is fixed there, those
  arguments checked first whatever the order the call writes (a
  transparent typedef is its expansion)
  (`callsite.rs::Widening`, `holds_invariantly`); a function held at a
  function type decides such a variable the same way, by the widest of
  the positions that hold it, whatever their order
  (`FnType::order_free_cells`, `ContainsHist::settle_deferred`); a function type with
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
  `normalize::Members`; a written union tells variables by name,
  `Type::flatten_written`;
  a field read of a union and a select's arms meet their function
  members, through a copy of a member's generic cells, binding its live
  ones, `Type::union_of_alternatives`); a merge takes the
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
