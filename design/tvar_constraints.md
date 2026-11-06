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
the "unsatisfiable constraints on 'a: i64 & string" error.

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
holding no undecided cell take part in that join, the defaults the call
omits included, joined as written ones (`Widening::join_default`). An
argument holding an undecided cell (a read target, a `let` over
`never()`, `[]`'s element) pre-unifies with a copy of its formal in
which those variables are open (`Widening::opening`), so it decides
none of them, and is checked at the end of the argument loop, when the
join is known (`CallSite::check_waiting`); its cells take what the join
leaves them. Order can then decide only between a value and a refusal,
never between two types: where the join does not hold the argument
(`f(str::parse(s), 1)` over `|x: 'a, y: 'a|`: `'a` is `i64`, which
holds no parse error), the mismatch is refused, "annotate it", naming
the bindings the argument reads whose types are undecided
(`callsite::undecided_note`), in either order. Two undecided arguments
at one variable: the second meets what the first gave it, and its
refusal says the same.

An argument with an undecided cell and nothing else at its variable
(`id(u)`) takes the variable as it is: one cell, decided by whichever
use comes first, as any cell is. A variable a callback also holds is
not widenable: the first argument decides it, an undecided one
included.

### A union of variables in a signature

A written function type may not hold a union of two or more distinct bare
type variables of its own (`['a, 'b]`, `['a, 'b, null]`), at any part (a
parameter, a callback's parameter, the result, the throws, a bound) and
under any constructor or typedef's expansion (`Array<['a, 'b]>`, `{v:
['a, 'b]}`, `E<'a, 'b>` over `type E<'p, 'q> = ['p, 'q]`):
`Type::variable_union`, judged where the type is written: a lambda's
signature as it compiles (a variable an enclosing definition declares is
not its own), a `.gxi` val (`module::check_val_variable`), a trait
method's declaration (`traits::method_decl`) and a function type a body's
annotation writes (`lambda::body_annotation`: a `let`'s, a select's type
test, `never<T>`). No value's type says which variable such a member
takes, so whichever relation meets it first would decide (a call's
argument first, an instance's signature last), and the refusal names the
annotation that passes: one variable there, or written types (`[i64,
string]`). One variable beside written types (`['a, null]`) is a member
a value's type picks, and a data union in an annotation (`let v: ['r,
'q] = ..`) is no signature.

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
  so a scheme's copy that meets a live cell is that cell's: a struct
  update, an array, list or map of composites, or a tuple holding a
  written name beside a holder's read is the name's one instance, where
  the name's cells meet the copy's; a name over `never()`, whose open
  cell is bound to the copy and never merged, is not
  (holder-structwith-written-01). What a
  merge may lower must never be a scheme's own cell, so a relation
  against a scheme instantiates it first: an omitting site outside a
  definition compiles its default among generic cells and judges a copy
  (`callsite::checked_default`), and a core trait's prototype calls an
  instance of its method (`Impl::build_prototypes`). Two function types
  are one union member when their cells pair up as a renaming, generic
  with generic or live with live; a scheme's copy never pairs with a live
  cell, which keeps that cell's instance (`setops::Identity::cells`), and
  a field read of a union meets the function members it reads
  (`data::struct_field_type`, `tuple_field_type`), as a select meets its
  arms (`Type::union_of_alternatives`): a member's generic cells meet
  through a copy, and its live cells bind, so a lambda literal in an arm
  is the instance the call through the union makes. A written cell is: an enclosing
  definition's parameter merged into `'n` in `let p: fn(y: 'n) ->
  Array<'n> = |y| [y, n]` makes `'n` that parameter, which every call
  of the enclosing definition's inner functions shares.
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
  as positions included, and settles whatever remains. A cell still
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
(`Holders`). The cells counted are those the scrutinee's type held
before the arms ran (`settle::position_cells`), still open at the end:
a cell an arm's own pattern brought, a bind's or a `_`'s, which the
arms' settle put into an open scrutinee, is the arms' decision and not
counted. A written test is judged over its own type, where any open
cell is its own variable nothing declares, and over what reaches it: it
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
a union member by member, or a definition whose parameters grow as it
recurses, meets a new key at every level and does not end, and the exact
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
wherever a pair is met (pins
`typ::setops::tests::diff_over_branching_unions_is_bounded`,
`could_match_over_branching_unions_is_bounded`). The pair budget does not
bound a residual that grows while one walk takes few pairs: over `T =
[Error<T>, Error<U>, `A]`, `U` alike, a test nested `d` deep leaves a
residual of `2^d` union members, and a select's running residual doubles
at each arm testing a level deeper than the last. Those selects stay
exponential in their depth, as before the budget
(diff-residual-depth-01). The next arm's walk over that residual takes
about `2^(d+1)` pairs, past the budget from `d = 9`: it subtracts nothing
there, so a dead arm after it is not found (a repeated `Error<U> as e`
arm, which main refuses, is accepted) and the arm after that walks the
unreduced residual again.

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
(`Type::distinct_members`): an error member (or the null) is what the
strip takes, which decides the strip and is the raised type, and the rest
is the result type (`Strip::taken`, `Strip::left`; pins
`lang::errors::an_operand_past_the_pair_budget_*`,
`two_cells_behind_one_name_past_the_pair_budget_*`). One unfolding and
1000 pairs were chosen from the 1600 generated selects (review-r8
X-r1-1): of main's 1547 finishes none changes verdict or the kind of its
message, 52 of its 53 timeouts end, the last in matching_type's
containment; 100 pairs moved two of main's verdicts (a dead arm accepted, a
dead-arm refusal become a missing case), 10000 pairs cost a generated
select (rnd13) 3.4G instructions where 1000 cost 1.7G. Where the widened
residual meets a type whose parameters grow, the arm's test against it can
no longer be decided (`Type::rep_collision`) and the arm is refused: a
test 10 levels deep in `G<'a> = [Error<G<Array<'a>>>, Error<G<('a,
'a)>>, `A('a)]`, which main accepts. Over Error cycles
of coprime lengths diff walks pairs that grow with the lengths, not their
lcm (pin
`typ::setops::tests::diff_over_error_cycles_walks_pairs_by_their_lengths`).

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
  none are left, `contains.rs`), so `let r: [&i64, Error<E>] = parse(..)`
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
a, b| select c { true => a, false => b }`), where a call relates its
arguments first: the split formal-union-order-01 keeps open.

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
however many paths reach it; a typedef whose
arguments grow at every level (``type N<'a> = [`Nil, `Cons('a,
N<Array<'a>>)]``) is refused by the growth rule of the representation
walks (`cast::grows_on`). A map's form is its key type's alone: an object
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
  conjuncts `constraint_view` (so Eq and Ord) sees, and the image keys them. A self-referential constraint
  `fn<'a: fn(x: 'a) -> _>(…)` is legal, and once seeded the declaring
  header and an inner fn that mentions `'a` reach the same cell and
  conjunct; each listing walk holds a per-signature reentrancy guard, so
  the regress stops at the signature already being listed.
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
