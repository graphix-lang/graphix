# TVar cell constraints

Status: built 2026-07-12
Pins: `stdlib/graphix-tests/src/lang/select.rs` (`gated_scalar_unannotated`), `stdlib/graphix-tests/src/lang/types.rs` (`concrete_targets_are_known_where_they_settle`), `stdlib/graphix-tests/src/lang/functions.rs` (`same_named_tvar_in_callback_arg`), `stdlib/graphix-tests/src/lang/functions.rs` (`lazy_three_level`, `inlang_map`), `stdlib/graphix-package-rand/src/test.rs` (`rand_float_default`), `graphix-compiler/src/expr/test.rs` (the round-trip proptest), `graphix-fuzz/findings/{settle-order-jul2026,infinite-type-jul2026,tvar-alias-sever-jul2026,bound-cell-cycle-accepts-aug2026}/`

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
`typ: Option<Type>` and `constraints: SmallVec<[Type; 1]>` — a
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
- A definition's gate keeps what its body bound: the signature is the
  scheme, relations between its positions included (`|x| [x, x]` is
  `fn(x: 'a) -> Array<'a>`), so a call's types follow from the signature
  without elaborating the body. Only a cell bound to ⊥ reopens
  (`unbind_vacuous_tvars`: `throws := ⊥` means nothing was observed). A
  def-time binding is a fact, never erased by instantiation.
- Arith and `Neg` constrain instead of binding: an unbound operand
  cell gets a `Primitive(Typ::number())` conjunct (`node/op.rs`).
  Same-cell operands (`a + a`) alias the result to the operand cell, so
  `|a| a + a` infers `fn(a: 'a) -> 'a, 'a: Number`, identical to the
  explicit form. Distinct-cell operands get a fresh number-constrained
  result cell, and the rule (`op.rs::arith_rule`) is judged again in
  the check's settle after the operand cells settle
  (`PendingSettle::Operand`, then `PendingSettle::Arith`).
- An array index or slice bound constrains the same way: an unbound
  index cell gets a `Primitive(Typ::integer())` conjunct
  (`node/array.rs::check_index`), so `|x| a[-x]` keeps `[Real, Sint] &
  Int` on `x` and each call's argument meets both; a known index type
  is checked against the integers directly.

Rejected for the distinct-operand case: unifying the operand cells
(a semantic tightening — mixed-type calls that passed would reject),
and keeping the result wide (leaves two-param bare lambdas with the
annotation-reject behaviour). Arithmetic has since been made
homogeneous (`fn('a: Number, 'a) -> 'a`), which resolves the question
from the other side.

### Instantiation

`reset_tvars` freshens keyed by CELL IDENTITY — one fresh cell per
source cell — not by name, so the source's alias topology carries to
every instance (`|a| a + a` shares one cell between `'a` and the
return type in every instance too), and constraints copy to the fresh
cells. Bound cells stay bound: a def-time unification is a settled
fact, and erasing it made the static type lie about the runtime value
(the JIT marshal-panic class).

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
  (`Fresh::Scheme`), so the call instantiates them.

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
  arguments enforce the narrowing.
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
  unbound and unconstrained after the whole tc0 phase binds ⊥
  (`TVar::settle_or_bottom`): nothing ever produced or constrained it.
- A site's settle leaves what an enclosing definition's signature
  reaches when the settle runs, not when the gate closed: an impl
  head's `'k` reaches a method's scheme only once the impl relates
  them, after the method's gate closed (`PendingSettle::Site::sigs`).
  It also leaves every quantifier of a function type its signature
  holds (`FnType::inner_quantifiers`): a rank-2 formal's `'b` is its
  callers' to pick.
- The check decides every cell it creates: the drain runs the site
  settles, then the operator operand and `let`-over-⊥ settles, then the
  rules judged over them (`PendingSettle`). Elaboration never settles
  a cell an earlier compile task created (`tvar::earlier_task`): what
  the check left open, it decided open.
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
- A cell remembers that ⊥ reached it while open (`TCell::bottom_fed`,
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

### `Concrete`: a conjunct that is a predicate

`'b: Concrete` (`Type::Concrete`, parsed only as a bound) says whatever
binds the cell is fully known: no open cell and no ⊥ in it. The
type-directed builtins declare it on their target (`str::parse`, the
json/toml/pack/sqlite reads, `sys::net::subscribe`/`call`, the db
trees); their `Apply::typecheck1` hooks only extract the type they cast
to and never refuse.

- `Concrete ⊇ t` is a probe: no ⊥ in `t`, bound cells judged by their
  bindings, open cells admitted. A bind of a `Concrete` cell hands the
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
cells). `apply(|x| x * x)` checks; `apply(|x| x + 1)` and
`apply(|x: i64| ..)` are refused, since they hold only where `'b` is
`i64`. A call of `apply` copies `'b` generic
(`FnType::generic_inner_quantifiers`): the argument's cells the check
aliases to it stay its own scheme's, which each call of `f` copies,
and the passing site's settle leaves it.

`FnType::contains` checks a declared bound only for a BOUND variable
(`bounds_hold`). An open one stays open, standing for one type the
bound admits; its cell enforces the bound at every binding. Binding
it to its bound would type the argument's parameter as the whole set
(`Number`), which is a different claim: values of several numeric
types at once.

### `FnType` derives its constraint list from the cells

The `constraints` list is gone from `FnType` (`typ/fntyp.rs`). Every
former consumer derives from the cells:

- `constraint_view()` — `(tvar, conjunct)` pairs, one per conjunct of
  each declared quantifier's cell, normalized, sorted by name then
  conjunct and deduped; a `+` bound prints as `fn<'a: A + B>`. Feeds
  Display, Eq/Ord/Hash (Hash reads the shape only), and the contains
  bound check: a variable with one bound meets it (an open one binds to
  it); with several, only a bound variable is checked, since an open
  conjunction settles by a witness and the cell enforces it at every
  binding meanwhile. `cell_constraint_pairs()` is the same listing over
  every reachable cell, declared or not; it is the Pack wire slot
  (decode re-seeds cells; `add_cell_constraint` dedups).
- `FnType.quantifiers: Arc<[ArcStr]>` — the names the `fn<...>` header
  declared, in source order. Names only, excluded from identity; the
  constraint types stay in the cells. A self-referential constraint
  `fn<'a: fn(x: 'a) -> _>(…)` is legal, and once seeded the declaring
  header and an inner fn that mentions `'a` reach the same cell and
  conjunct; each listing walk holds a per-signature reentrancy guard, so
  the regress stops at the signature already being listed.
- `replace_auto_constrained` reads inferred `'_N` pairs from the cells
  directly (inference declares no header); `sig_matches_int` requires
  each of the signature's declared conjuncts among the impl cell's
  conjunction, and its impl-constraint check is SATISFACTION — the
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
  is never aliased by name: `reset_tvars` keeps its topology by cell and
  only freezes the cells that repeat (`FnType::freeze_shared_tvars`), so
  a callback argument holding another function's `'b` stays apart from
  the callee's own. The only discriminator is the
  marked cell's STATE at terminal settle (open and unconstrained →
  error), which the dependency-ordered settle controls
  deterministically. Nested-fn name flatness within one signature is
  rank-1 polymorphism and load-bearing (`inlang_map`, the def-gate
  param knot, self-referential quantifier constraints); do not
  resurrect per-Fn-boundary scoping or cell provenance without a live
  witness.
