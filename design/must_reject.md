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

Stacking comes from the base, not from the mutation: a base is a
typemorph subject, already far from anything written by hand, and each
base yields one mutant per chosen site. A mutant is never mutated
again.

## The type map

The base is checked once with a type map: every expression's type,
resolved (`resolve_tvars`, so later unification cannot move it) and
keyed by its span in the body; nodes sharing a span answer with the
first type recorded. It is a new `Ide` sink, filled by one
walk over the checked root nodes after typecheck and only when a check
asks for it; a check without it pays nothing. A reference's entry also
names its binding (`ExprTypeSite::bind`), whose declared type the
check's env holds.

A candidate uses the map to pick its site and its disjoint type. A
type is DISJOINT from `T` when it is a primitive `T` does not contain
and that contains nothing of `T`; a site whose `T` holds `Any`, an open
cell or an abstract type is skipped (nothing is disjoint from `Any`,
and an open cell has not decided).

## Families

Each family: the site, the mutation, the rule that must refuse it, and
what disqualifies a site.

**1. Monomorphic reuse (instantiation).** Site: `let f = |..| ..` used
at two uses whose instances take different primitive first parameters:
a VALUE use's by the map's type of it, a call's by the map's type of its
first positional argument where the lambda's first parameter is left
open.
Mutation: route the lambda through a binding something writes, `let f =
{ let tm__0 = |..| ..; let tm__1 = &mut tm__0; tm__0 }`. Rule: a binding
something writes is not generalized (the value restriction,
`CompileCtx::monomorphic`; a written name's cells are claimed for its
level at its binding, and a merge takes the lower of its two cells'
levels, so a holder's copy that meets them is that one instance too,
`TVar::merge_one`), and `f`, a `let` of a block whose value names it,
forwards that one instance (`bind::names_generic`; a single name
something writes holds an instance of its own, `Bind::forwards`, pinned
by `lang::interfaces::written_alias_of_*`), so every use, a call's
included, shares its cells and the two uses meet in one instance:
`f(1); f(1.5)` over it is refused. A block alone would not hold it: a
`let` of a block whose value is a lambda, a generalized binding or a
field or tuple read of one generalizes (Generalization, "Generic only
where a binding names it", in `tvar_constraints.md`); a select's arms
are not seen through (CR holder-block-select-01). Right site: the
definition, or the statement of any use up to the second of the two:
every use shares the written binding's cells, so the checker refuses at
whichever conflicts first, a use the map gives no primitive first
parameter included. An unannotated `let g = f`
forwards the wrapped binding, so the value uses of `g` are uses of `f`
(family 7's sites follow the same aliases).

**2. Rigid variables.** Site: a lambda with declared variables (`'a`,
`'b`) whose parameters `x: 'a`, `y: 'b` are in scope in the body.
Mutation: add a statement that equates two of them (`x == y`) or binds
one to a concrete type (`x == 1`) or to ⊥ (`let b: _ = x`). Rule: a
def's declared variables are rigid in its body check: none binds to a
concrete type or to ⊥, no two unify and none escapes into a cell outside
the definition (a write to an outer binding, through a reference, a
function sharing its type or a callee's bound tying it to an outer cell's
constraint: `DefGate::check_declared_kept`; a variable the
signature names undeclared is lowered with the outer cell instead, and the
definition is monomorphic in it). Skip: a variable with a constraint the concrete type satisfies
only through the constraint (`'a: Number` against `1` is still refused,
but keep the first cut to unconstrained variables). Built: a first
statement comparing a parameter `x: 'a` with a literal or, alternately,
binding it under a `_` annotation, or calling a
parameter `f: fn(x: 'a) -> ..` with one (`'a` not `f`'s own
quantifier: each call picks that anew), right site the definition. Open: equating two declared variables;
writing a parameter `x: 'a` to an outer `let y = never()` (`y <- x`).

**3. Call conflicts.** Site: a call argument. (a) The callee's resolved
parameter type is concrete: substitute a literal of a disjoint type.
(b) The parameter is a variable shared with another argument whose
type is concrete: substitute a literal whose type neither contains nor
is contained by that argument's. Rule: containment for (a); for (b) the
widest-argument rule refuses when no argument's type contains all the
others, and a variable a callback or reference holds keeps the first
argument's type, so both paths refuse (`callsite.rs::Widening`). Skip:
labeled defaults, variadic positions, a parameter typed `Any`, a first
argument whose type its uses decide (a `let` over `never()`, or a value
holding one, `[n]`: the call decides it after every other argument, so
it meets nothing). Built:
(b), two or more positionals of one variable, the second mutated and
every other one there a concrete primitive (each takes part in the
widest-argument rule), the literal disjoint from them all, right site
the call; (a) is family 4's argument case.

**4. Widening into a rigid consumer.** Site: a use of a value `v: T`
whose parent is one of:
- an arithmetic operand (exactly one numeric type, each operand
  containing the other; `[i64, string] + 1` is refused);
- a select scrutinee with no catch-all arm (a bind or `_` at the top,
  or a type-test arm that `U` fits), the coverage rule;
- a struct field read (a union with a non-struct member is refused);
- a call argument with a concrete parameter type, the callee no lambda
  with an unannotated parameter (which may be a cell shared with the
  environment, `|y| z <- y` over `let z = never()`, that the call widens)
  and no function of the subject's own modules (whose state is such a
  cell as well): a bare callee is rigid only as a builtin or a `let` of
  a lambda literal with every parameter annotated, since any other name
  the body binds (a destructuring `let`, a `use`, an alias) may be such
  a lambda;
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
or an open cell the comparison binds. One hop is built (see Hops). A
widened value that makes a union of structs whose members' fields differ
is refused at a writable reference to such a field (`&mut s.c`: a
writable field step needs one field type), which the family counts as
its own refusal.

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
over ⊥ is skipped: its type is a cell the first use decides, so an
inserted writer decides it (`ExprTypeSite::cell` marks it). So is a
`let` over a call whose callee's declared type returns a cell that no
parameter holds, nor the bound of one a parameter holds, nor a payload
bounds from below (`list::nil`'s element, a type-directed read's `'b:
Concrete` target, `sys::exit`'s result, `db::tree`'s key and value): no
argument decides that part of the result, so its first use does
(`mustreject.rs::open_return`). The same judgment of an open value
(`open_init`) skips family 3's argument, the hop's `let`, family 5's
scrutinee, 9 (b)'s referent and family 11's value. Right site:
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
  (or a declared variable's constraints, `lambda.rs::check_defaults`; a
  variable nested in the type is any instance of it there, and a default
  holding an enclosing definition's declared variable fits its parameter
  whole) and again at each site that omits it, where it may narrow that
  site's cells, never an enclosing definition's scheme
  (`callsite::Judge`);
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

**8. Function bounds.** Site: a call argument whose parameter type is a
cell with the `Function` conjunct (`queuefn`'s `f`). Mutation: the
argument replaced with a literal. Rule: `'a: Function` admits only a
function type with no variadic argument and no defaulted label
(`Type::function_holds`). Built (`function-bound`), right
site the call.

**9. Writable references.** Site: `let r = &mut x` over a binding.
(a) A later `*r <- ..` and no rebinding of `r` between: the `&mut`
becomes `&`. Rule: a write needs every reference it may go through to be
`&mut` (`ConnectDeref::typecheck0_with`), as does a call passing `r`
to a `&mut` parameter (`&mut T` does not contain `&T`, `contains.rs`).
Right site: the write, or a statement between that uses `r`. (b) The
map shows `x` a concrete primitive: the let is annotated `&mut Any`.
Rule: `&mut T ⊇ &mut U` only when `T = U` (`contains.rs`). Right site:
the let. Built (`ref-write`, `ref-widen`); the generator's `&mut`
writes are the sites (`generate/funcs.rs`). (c) `&mut e` of a value (not
a binding) in a definition's body, annotated at a type that does not hold
`e`'s (`let r: &mut string = &mut [x]`). Rule: the value bounds the
reference's fresh cell from below (`check_contains_lower`), so the
annotation must hold it, in a body as at the top level. Right site: the
let. Open.

**10. One runtime form.** Site: a select over a scrutinee `s` the map
types a concrete `T`. Mutation: `select <&s or &u> { &T as r => select
*r { arms }, _ => never() }`, `u` a literal of a type disjoint from `T`
(`disjoint_literal`), `T` holding no function (a type test over one is
refused by its own rule first). Rule: no arm may tell apart two types with one
runtime form, here two references (`Type::rep_collision`, checked per
arm in `Select::typecheck0_with`). Right site: the outer select. Built
(`same-form`). The rule's other site, a trait dispatch over a union self
(`trait_contains` refuses a union whose members collide), has no
mutation: the generator's trait unions are of abstract types, which
never share a form.

**11. Abstract parameters.** Site: a statement `let v = e` the map types
`B<i64>`, a generated module's parameterized abstract type (the
generator's `type B<'a> = Abstract<'a>`, entering the vocabulary at
`B<i64>` through `mkb`/`unb`). Mutation: the let annotated
`m::B<string>`. Rule: a constructed value's parameter holds its
payload's type (`TCell::lower`), and an abstract type's parameters are
invariant, whatever its rep does with them (`contains.rs`, `equal_int`
over the parameters). A wider annotation (`m::B<[i64, string]>`) is no
mutant: the payload bounds the parameter from below, so the annotation
decides it. The map holds the type by id, so the module is
the one whose `mkb`/`unb` the body reaches, or with several the one
whose `mkb` the let calls; a value with a leaf its uses typed is
skipped, since a let annotation decides a parameter left open. Right
site: the let. Built (`abstract-param`).

**12. Function types with variables of their own.** Not generated: a
function type with type variables of its own anywhere but a declared
signature's top is refused where the type is written
(`typ::QUANTIFIED_FN`, `design/tvar_constraints.md`), a rule that reads
the written type alone, so no inference can repair around a mutant: the
shallow kind the principle above leaves to its pins (`lang::quantified`).
The rule's deep half, that a function no binding names is one instance,
is family 1's, whose call sites it made.

**13. Patterns inside a rigid variable.** Site: a lambda with a
parameter `x: 'a`, as family 2's. Mutation: a first statement `let tm__0
= select x { null => 0, _ => 1 }`. Rule: a pattern that tests or binds
inside a value of a type variable its definition holds rigid needs a
written test; a name or `_` over the whole value keeps the variable
(`StructurePattern::meets_rigid`, checked in `Select::typecheck0_with`
and a destructuring `let`), and an arm that can match no value of a
bounded variable is told as unreachable. Built (`rigid-pattern`), right
site the definition. Not generated: coverage beside a bounded variable (a head
arm at `['a, null]` leaves the variable's values to a name or `_`,
`Head::Open`; containment splits `'a` by its bound,
`Type::covers_by_parts`): the generated declared variables are
unbounded, where a head test beside one meets it and this family's rule
refuses first; pins `lang::select::pool_variable_*`,
`distribution_variable_*`, `containment_variable_*`. Nor a written test
whose part over the variable holds `_` or an undeclared variable
(`StructurePattern::meets_rigid_written`, the same message): the
mutation writes no test; pins `lang::select::written_test_holding_*`.

**14. Undecided parts at a select.** Site: a select over a concrete
scrutinee `s` whose arms bind no name and have no guard or type test,
one of them testing (not `_` or a name). Mutation: `{ let tm__z =
never(); select (select true { true => s, false => tm__z }) { arms, _ =>
last } }`, the last arm's body repeated under a `_` (a union scrutinee
beside a wildcard is one the arms' settle does not decide); and, where
an arm chooses a value (a literal, a tag, a length; not only a pattern
that takes the value apart), the decide shape, `{ let tm__z = never();
select (tm__z, 0) { (arm, _) .., _ => last } }`, where that arm would
decide `tm__z`; where every arm only takes the value apart, the beside
shape, `{ let tm__z = never(); select (select true { true => s, false =>
tm__z }, 0) { (arm, _) .., _ => last } }`, where an arm's destructure
chooses between `tm__z` and `s`. Rule: an open cell the scrutinee held before the arms,
still open at the select's end or decided by an arm's pattern, that an
arm tests is refused (`Select::check_undecided`,
`StructurePattern::meets_undecided`, `meets_decided`); a pattern that
only takes apart every value of the shape it decides is no test of it
(`StructurePattern::only_destructures`), except beside a member of that
shape. Built (`undecided-part`), right
site the select. Not generated: an arm that only destructures the open
part alone, which decides its shape as an annotation would (pins
`lang::select_undecided::destructuring_*`); a name over the open part, typed beside
the members' others (`TVar::narrow_per_member`), whose refusal comes at
a later write (pins `lang::select_undecided::name_over_an_undecided_*`);
a written test's own open variable (pins
`lang::select_undecided::written_*_open_variable_refused`) and a written
test of the undecided part (`written_tests_undecided`, pins
`lang::select_undecided::written_union_test_of_an_undecided_*`); and members
told apart only by undecided cells under a partial struct pattern
(`UndecidedMembers`; pins `lang::select::partial_struct_*_two_members`).

**Interface vals.** Not generated: the module generator writes every
`val` at one type. A val's variable outside a function type is refused
by a rule that reads the written type alone (`Type::
variable_outside_functions`), the shallow kind left to its pins
(`lang::interfaces::val_variable_*`). The rules that hold an
implementation cell no use copies under a val's variable
(`module.rs::hold_paired`, trait methods included) and a written val to
its implementation (`CompileCtx::check_val_written`) need a generated val
with a variable, or a write through a val over a generic body, which the
generator does not write; their pins are
`lang::interfaces::*_over_an_outer_binding_refused`,
`narrow_write_through_a_val_refused`, `bounded_write_through_a_val_refused`,
`write_through_a_*_val_refused`, `*_alike_named_*_refused`,
`mut_ref_to_a_*_val_refused`, `mut_ref_to_a_function_field_of_a_generic_val_refused`,
`write_through_a_typedef_val_after_*_refused`
and `lang::traits::*method_over_an_outer_binding_refused`,
`*method_value_no_lambda_*_refused`, `method_made_by_a_call_refused`.

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
checker bugs). Built: one hop for family 4 (its own id, `widen-hop`, so
the direct sites never fill its cap), an unannotated, non-⊥,
concrete top-level `let w = e` whose `w` a reached use puts under an
arithmetic operator, a comparison opposite a literal, or a field read
gets `e` widened;
right site the `let` and every statement `w`'s new type meets (family
6's closure). Open: longer chains, and hops for 5.

## Verdicts

Every mutant is checked like a typemorph probe (`typemorph-one`: a
warm runtime, `--check` semantics, a fresh process to confirm).

- **Rejected at the right site, for the family's reason: pass.** The
  refusal's `ErrorSite` (the innermost expression an error is attached
  to) lies inside the mutated node or the rigid consumer, and its head
  is one the family's rule produces (`Family::refuses_with`). The
  mutated node's span is found in the printed mutant by its preorder
  index, like every typemorph site.
- **Rejected at the right site for another reason: a finding.** The
  checker refused the mutant, but not by the rule the family cites.
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
or `FLIP <id> LEAK: accepted at <site>` / `FLIP <id> MISPLACED: <head>` /
`FLIP <id> WRONG REFUSAL: <head>`; a FLIP is
confirmed in a fresh process and recorded like any typemorph flip, as
`typeflip_N.gx` once per class (the id without its site + the
normalized head), so both lanes share confirmation and dedup. Open: the
differential run that adjudicates a LEAK.

## Integration

The families run inside the typemorph source: a subject yields its
must-accept probes as now and its must-reject probes on the same base,
the subject's own body.
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
