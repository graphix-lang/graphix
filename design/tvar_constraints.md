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
bounds commit together on copies first (`commit_lower`), so bounds a
probe admits apart but no binding holds together (`(i64, string)` above
`('x, 'x)`) refuse the bind and bind none of their cells: a refusal
another walk ignores (a select's settle against its arms) leaves
nothing behind. An open cell of a bound below a union binds to the
union where that holds every bound, else the union's open member takes
it as a bare cell would (`ContainsFlags::IntoMember`): a slice's
element set `['h, 'r]` that must hold `'x` and `Array<'x>` puts `'h`
above both, where `'x := ['h, 'r]` would hold itself under a
constructor.
Neither choice is complete without an upper bound on `'x`: the
member loses the union's decided members (`[i64, 'a]` above `'x`, then
`'a := string`, would refuse `'x <- 1`), and the binding makes `'x` one
type with the union's open cells, so `select b.0 { [1, h, ..] => h, [h,
..] => h }` over a payload above `'x` and `Array<'x>` binds `'x` to the
first arm's element set and is refused where the second arm's `h` would
hold `Array<'x>`, a type holding itself. Each conjunct must contain a
lower bound when it is added; a merge joins both cells' lists after
each side's conjuncts admit the other's (a rigid cell admits none but
itself); a copy, an image and a level claim carry the list. The witness
of a cell with lower bounds is their union, so every settle binds it
there, one a ⊥ was produced into as well (`CellFact::BottomFed`)
included; a probe of a trait bound reads it so, and a commit settles it.

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

### Quantified function types written elsewhere

A written `fn<'b: C>(..)` means the same wherever it stands: a `let`
annotation, a struct field, a typedef or a return type holds what it is
given to every `'b` the bound admits, as a formal does.
`FnType::contains_int` opens quantifier gates (`open_quantifier_rigid`)
on the containing side's open quantifiers, so a merge into one is judged
by `quantifier_admits`: `let f: fn<'b: Number>(..) = add` over `add` at
`'a: Int` is refused at the binding, and so is `{f: add}` under a field
of that type. Bounds that name each other (`'a: [i64, ('a, 'b)]`) reach
the judgment of a pair again inside it; a thread-local set of the pairs
in progress (`Judging`) answers such a re-entry that it holds, the
coinductive reading, which stops the recursion. One check adopts rather
than judges: an instance rebuilds a lambda from its written type and
checks it over the type its definition's check recorded, which may hold
a bound the body added (`'a: Number` with `+` gains `Singleton`); that
check passes `ContainsFlags::Adopt`, whose plain rigid gates absorb the
recorded bounds. Pins: `lang::types::rank2_struct_field_refuses_narrower_bound`,
`rank2_let_annotation_refuses_narrower_bound`,
`rank2_let_annotation_holds_each_instantiation`,
`lang::image::a_definition_entered_three_times_restores` (the mutual
bounds), `lang::collection::collection_returns_a_generic_function` (the
adopting check).

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
  cell's level.
- Binding a cell, merging two, or adding a conjunct lowers every cell
  the new contents reach to the cell's level (`tvar::lower`): what an
  outer cell is bound to is the environment's. A generic cell is never
  lowered: a scheme bound into a cell (a let's name, a reference's
  placeholder) is still a scheme.
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
  (`Fresh::Scheme`), so the call instantiates them. No copy but
  `Fresh::Copy` takes a rank-2 formal's quantifier while a gate holds it
  (`TVar::is_quantifier_rigid`): under the check of a call's argument it
  is one type, so a definition in the argument that holds it (`|x| { let
  g = |y: i64| x; g }`) is monomorphic in it.

A binding something writes is not generalized (the value restriction,
`CompileCtx::mark_connect_target`): every function its value holds, a
struct field's or a tuple element's included, is one instance its uses
and writers share (`CompileCtx::monomorphic`). Its writers are known
before its uses are checked: a block collects the names its lets bind
that a `<-` or `&mut` in it writes, lambda bodies included, where no
nearer binding of the name stands between (`node::written_lets`), and
such a let is a `<-` target from its binding on. A binding an earlier
compile already generalized, which only a later input to the same
runtime can see (the REPL, `GXHandle::compile`), had its uses checked
against the scheme, so it stays one: a write is checked against a copy
of it with every variable rigid (`Connect::typecheck0_with`), and a
`&mut` of it is refused, since a reference cannot hold the variables
rigid.

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
  from the site's settle and the default binds them at the bind.
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
  holds (`FnType::inner_quantifiers`): a rank-2 formal's `'b` is its
  callers' to pick.
- The check decides every cell it creates: the drain runs the site
  settles, then the operator operand and `let`-over-⊥ settles, then the
  rules judged over them (`PendingSettle`), and last the completions a
  partial struct pattern took for one member while cells not decided
  yet told them apart (`PendingSettle::OneMember`): refused unless an
  earlier arm's cover holds the other member or the two completions are
  one type once the check has decided (`setops::settled_identical`).
  That is the verdict `==` gives cells decided before the select, so a
  run-time test that would admit both members (a slice element, a
  function type) does not save the pair. Only a cell that holds no value
  is any type there, and the judgment is directional. ⊥ and a top-level
  cell still open (nothing decides it) are any type on either side. A
  definition's open cell its calls may still bind is only itself in the
  arm's completion, which is its run-time test and which a caller may
  name; in the other member it is any type unless a caller can hand it
  a value, which the enclosing definitions' signatures say
  (`FnType::input_cells`: what a parameter reaches, and what the result
  reaches under a `&mut`, a nominal type, a constructor application or a
  returned function's parameter). So an empty
  `[]` a definition returns is apart from nothing beside its other
  members, and one it returns as the test is apart from a parameter's
  values. Under `GRAPHIX_NO_SUBST` an instance's own check judges the
  types its caller chose, so it refuses such an empty `[]` a caller
  names apart, which the definition's check accepted. It reads the cells
  and decides none, so it comes after every rule that may. Elaboration never settles
  a cell an earlier compile task created (`tvar::earlier_task`): what
  the check left open, it decided open. An open quantifier of such a
  function type (a generic function value in `m: Array<fn<'a: Number>
  (..)>`) is any instance of itself to a later task: `FnType::
  contains_int` compares it as a fresh copy, held rigid on self's side
  and instantiated on t's, so neither side links it to the task's own
  cells. A block's statements elaborate in parallel tasks that all read
  the check's cells, and a link made by one is seen by the others at
  whatever point they have reached.
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
  above `'p`): the relation is accepted and neither cell records it; a
  commit marks both `CellFact::CycleRefused`, and one still open at the
  terminal settle is refused as the infinite type (pin
  `lang::types::payload_cycle_of_cells`).

The merges' own guards (`TVar::alias`, `alias_cells`, `copy`) skip a
merge that would close a cycle and mark the cells the same way. Most
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

### Quantified function formals

A formal of function type with its own quantifiers (`|f: F|`, `type F
= fn<'b: Number>(x: 'b) -> 'b`) is rank-2: every call of `f` in the
callee instantiates `'b` afresh (`FnType::instantiate`, and
`FnType::shared_call` for a call that shares the definition's other
cells), so the callee may call it at any type the bound admits. Its argument is therefore checked with `'b`
RIGID (`callsite.rs::quantified_formal`): the formal is expanded once,
rigid gates open on its quantifier cells, and the pre-unify, the
argument's typecheck0 and a `check_contains_rigid` all run against
that one expansion (each expansion of a typedef reference freshens its
cells). The quantifier keeps exactly its declared bounds: whatever the
argument's check adds to it, a predicate included, must follow from them
(`callsite.rs::added_bound`, by `Type::implies`, the rule an interface's
bounds follow), so under `'b: Number` `apply(|x| x == x)` checks,
`apply(|x| x < x)` is refused until the formal says `'b: Number +
Ordered` and `apply(|x| x * x)` until it says `'b: Number +
Singleton`; `apply(|x| x + 1)` and `apply(|x: i64| ..)` are refused,
since they hold only where `'b` is `i64`. A call of `apply` copies `'b` generic
(`FnType::generic_inner_quantifiers`): the argument's cells the check
aliases to it stay its own scheme's, which each call of `f` copies,
and the passing site's settle leaves it. While the argument checks, `'b`
is closed to what the argument defines, as an enclosing definition's
declared variable is: a reference to a definition holding it does not
copy it, and a labeled default reading it fits its parameter whole
(`check_defaults`), so `app(|x| { let g = |#d = x, y: i64| d; g })`
against `fn<'b>(x: 'b) -> fn(y: i64) -> string` is refused at the check
rather than at the bind.

A type written in a body names the variables of the definitions around
it (`lambda::body_annotation`); a labeled default is written in its
definition's signature, so its types name the variables of the
definitions around that one (`LambdaDef::in_signature`). An instance's
body compiles under its definition's signature as the instance holds it
(`lambda::instance_sig`: each variable copied under its name and bound to
the instance's type), so a default compiled at an instance's bind, which
no table row types, reads `'a` as the instance's type.

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
quantifier is open, rigid or a formal's by that cell (`FnType::
bounds_hold`, `is_rigid_formal`). A gate on the forwarding cell would
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

`'b: JsonData` (`json::read`'s target) and `'b: TomlData` (`toml::read`'s)
hold of a type with no union anywhere in it whose two members one value
of the format could be written by (`Type::text_ambiguity`,
`typ/textdata.rs`): the reader picks one member per value, so a value
written as one could read back as the other. Each member is read as the
data it writes (ints and f64 apart; JSON writes a datetime and a
variant's tag as strings, a duration as a float and a decimal as an exact
number; TOML has no null and a native datetime; a union reads a number
into its own class first, so an integer member takes a whole float that
an f32 or a JSON duration wrote, and only an f64 member keeps it), composites as arrays (an
object reads as its sorted key/value pairs, a struct takes exactly its
fields less the nullable ones, any-length arrays share the empty array),
and two members overlap where some finite value is written by one and
read by the other. Unlike the other predicates it is judged only of the
call's settled binding (`site_bounds`): a refusal at the bind would make
a union target narrow to the one member that passes. It is handed to an
open cell of its binding as `Concrete` is, so a generic reader's callers
are judged at their sites.

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
