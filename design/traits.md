# Traits for Graphix — a critique of the 2026-08-22 sketch

Status: DISCUSSION DRAFT. Eric sketched a Rust-style trait system
(traits with required + default methods, `impl Trait for Type`, traits
as constraints, `Trait::method` paths, trait generics and associated
types). This doc is Claude's assessment of that sketch against the
type system as built, and a staged proposal.

## 0. What the io::Stream pain actually proves

`io::Stream<'a>` is `StreamValue { inner: Arc<Mutex<Option<StreamKind>>> }`
where `StreamKind` is a closed enum of nine tokio types
(`graphix-package-sys/src/lib.rs:49`); read/write dispatch is a Rust
`match`, and the `'a: [`File, `Tcp, ...]` parameter is a phantom tag
that types `shutdown`/`peer_addr` over `[`Tcp, `Tls]`. No package
outside sys can produce one.

Half of that pain has a one-day fix with no language change: replace
the enum with `Box<dyn AsyncRead + AsyncWrite + Unpin + Send>` plus a
small Rust trait for the socket-only operations, export `wrap_stream`,
and keep the tag. Any package WITH RUST CODE can then produce a Stream.

What that does not fix is the other half, and it is the half that
justifies a language feature: a stream written IN GRAPHIX — a
`BufReader` over a `Stream`, a gzip reader, a record framer, a mock
for tests — can never be a `Stream`, because the only way to be one
is to be a Rust value. The same wall stands in front of every
abstraction a package might want to leave open: `Show` for
interpolation, `Ord` for a user-defined sort key, `Hash`, a serializer.
Stand the feature on THAT, not on io::Stream; io is the first client.

## 1. The sketch fits the type system better than it might look

A tvar constraint is already an open predicate: `fn<'a: Number>` seeds
`'a`'s cell with a conjunct, and binding the cell checks every
conjunct with `contains` (`typ/contains.rs`, `cell_constraints_ok`).
`Number` is a closed set (`Type::Primitive(BitFlags<Typ>)`), and `+`
is, in effect, its method with builtin impls.

So "a trait in constraint position expands to the set of every type
that implements it" is the existing mechanism with ONE change:
membership is OPEN — declared by `impl`, not enumerated by the
compiler. A trait is a `Type` whose `contains(T)` answer is "is there
an impl of this trait for T". `Read + Write` is a two-conjunct cell,
which already exists (the interface printer currently refuses to list
multi-conjunct cells — `FnType::constraint_view` — a detail that has
to change).

The one thing to hold firm on: it must stay a PREDICATE, never an
eager union. If `action`'s signature froze `Read` into
`[File, Tcp, Tls]` when `action` was checked, a package loaded later
(or an `impl` typed at the REPL) could never call `action`. A
constraint is checked at INSTANTIATION, against the table as it stands
then, so generic code written before an impl existed accepts it.

**Scope (Eric's rule, as Rust):** a trait is a NAME and resolves like
any other — by declaration, `use io::Read;` (or a full path), or the
prelude. Impls are not names; they are global facts (§4). So: the
trait name (in `'a: Read`, `Read::read(f, n)`, `impl Read for File`)
needs the trait in scope; a method called BARE (`read(f, n)`) needs
the item (`use io::Read::read;` or `use io::Read::*;` — traits register
as module-like scopes, the existing import engine does the rest; two
bare `read`s collide under the existing rules); constraint discharge,
the generated union select (§3) and parameterized-head lookup (§4)
resolve by trait IDENTITY through the global table, so a generic
`action` declared where `Read` is in scope is callable from a module
that never imports `Read`, and `==`/interpolation find the core four
with no name at all (they are in core's prelude anyway). There is no
method-call syntax — `f.read(n)` stays field access — so the language
never searches scope for a method by the receiver's type.

`Read` in an argument position = a fresh `'a: Read` per occurrence
(`fn(a: Read, b: Read)` is two variables — Rust's `impl Trait` rule).
In any other position (return type, struct field, array element) it
cannot mean that and is an error; a struct parameterizes
(`type Server<'a: Read> = {conn: 'a}`).

## 2. Static dispatch is right, and it is the only sound choice

The sketch makes an unresolvable method call a compile error. Agreed:
a self type that is still an unbound tvar after typecheck has no
runtime witness to fall back on — a gx-defined abstract type
(`type Counter = i64`) ERASES to its representation — and the only
alternative would be a closure record (dictionary passing), which is a
new value shape and a DynCall at every method call. A self type that
is a UNION of implementors is a different, decidable case: §3.

Two further things fall out of static resolution for free:

- Effect inference sees the callee. A trait method's Sync/Async
  classification is per IMPL (`File::read` is async, a mock's is not);
  a statically-bound site classifies exactly like any other call.
- Fusion sees the callee. A `Show`/`Ord`-style trait over scalars is
  an ordinary static call and fuses; nothing about "trait" reaches the
  JIT. This is the predictable-fusion value in action.

The enabler is per-callsite elaboration: each instance of a generic
lambda re-checks its body with the call's types, and
`CallSite::typecheck1`'s static resolution happens there. Method
resolution is the same pass: the self argument's type must be settled
by typecheck1 of the instance. In practice the error fires in one
shape only — a POLYMORPHIC lambda that is never called through a
static site (stored in a struct field or a `&`, called through a
dynamic CallSite). The message must name the call site that could not
resolve and the unbound variable.

## 3. Dispatch over a union: generate the select (Eric's call)

```graphix
let s = select use_tls { true => tls::connect(..)?, false => tcp::connect(..)? }
```

types as `[Tls, Tcp]`. A trait call on it — `read(s, n)` — desugars
to the select the programmer would otherwise write, with a static
call in each arm:

```graphix
select s { Tls as t => Tls::read(t, n), Tcp as t => Tcp::read(t, n) }
```

Everything follows from that being the desugaring:

- Semantics need no new ruling. The generated select is a sleep
  boundary the user did not write — an impl body holding `count` or
  `once` restarts when a `[i64, string]` scrutinee alternates types —
  but the hand-written select does exactly the same, so the trait
  introduces nothing. Organic firing, arm sleep, the scrutinee ride:
  all inherited.
- It fuses: static calls in select arms, arm-region fusion applies.
  No closure record, no DynCall.
- Primitive and structural members (`Show` on `[i64, string]`) are the
  COMMON case and cost nothing: select has those predicates today.

The select has to discriminate at runtime, and that is decidable per
member kind:

- primitives and structural shapes: the existing type predicates;
- Rust-backed abstract values (`Tls`, `Tcp`): `Value::Abstract`
  carries a TypeId — a runtime test exists, it is not yet exposed to
  select;
- gx-defined abstract types: with `nominal_abstract_types.md` every
  graphix-minted abstract is a `Value::Abstract` tagged with its
  type's id, so the test is a tag comparison, same as the Rust-backed
  case. (Without it they erase to their representation and dispatch
  is decidable only when members' reps are pairwise disjoint — an
  abstraction leak. That rule is superseded; the box is the fix.)

One mechanism — the tag test — serves trait dispatch and lifts
select's refusal of abstract type predicates for user code.

What this leaves uncovered is a value whose implementor set is
unknowable at the type — Rust's `dyn`. In a language that types every
value the union is always inferable at the call site, and a library
struct parameterizes (`type Server<'a: Read> = {conn: 'a}`) like a
Rust generic struct. No `dyn` until a real program demands one; the
closure-record encoding is the fallback if one ever does, and a
programmer can write it by hand today.

## 4. Impl targets: abstract anywhere; anything else only in the trait's package

Impls are GLOBAL once their module loads — scope governs only whether
a method's NAME may be written bare (`use io::Read::read`), never
whether an impl applies. Coherence demands it: a value must be equal
to the same things and print the same way everywhere (a `Set` built
under one `Eq` and queried under another is silent corruption), and
the core four are used implicitly by `==` and interpolation through
the prelude.

In a structural language a type IS its shape, so an impl on a
structural target applies to every type of that shape, program-wide.
That is a BOMB when strangers can write it: `impl Display for Point`
with `type Point = {x: i64, y: i64}` in package A changes how package
B's unrelated `{x: i64, y: i64}` prints the moment A loads, and a
second such impl in B is a conflict between two packages that never
heard of each other. The one legitimate non-abstract case tells you
the rule: a user trait is useless without impls over builtin shapes
(`impl ToJson for i64`, `for string`, `for Array<'a: ToJson>`,
`for Map<string, 'a: ToJson>` — Rust's `impl<T: Tr> Tr for Vec<T>`),
and those are written by the TRAIT'S AUTHOR, who is answerable for
the trait's semantics over every shape.

**The rule:** an impl target is either

- an ABSTRACT type (`Abstract<rep>` or Rust-backed) — impl'd in the
  type's package or the trait's (the orphan rule); or
- ANY other type (primitive, constructor with constrained element
  tvars, struct, tuple, variant, alias) — but ONLY inside the trait's
  own package.

Unions are never targets (members resolve first, §3); a bare tvar as
the whole target (blanket impl) is v2 at the earliest. Coherence is
one key — one impl per (trait, canonical type) — and a conflict can
only ever be between two impls in one package: the author's own bug,
never a load-time surprise between strangers.

Consequences. Core writes NO impls: its structural default for the
core four IS the typed walk (§8), so outside core those traits are
implementable for abstract types only. A struct or ADT that should
carry behaviour gets a name — `type Shape = Abstract<[`Circle(f64),
`Rect(f64, f64)]>` — at the cost of one unwrap before each `select`;
that is where structural variants bite hardest (ML's and Haskell's
classes live on nominal `data`), and the cost is paid rather than
making variants nominal.

Worked case (Eric's):

```graphix
type Point = {x: i64, y: i64};   impl Display for Point { .. }
type T     = {x: i64, y: i64};   impl Display for T { .. }
let f = |x: {x: i64, y: i64}| print(x)
```

Refused at the first impl: "`Display` is implemented for abstract
types only outside core; make `Point` `Abstract<...>`." NAME-DIRECTED
dispatch on a transparent alias (Point's impl for things called
Point) is not offered because it cannot be honest: `Type::Ref` keeps
the alias name for printing and compression but unification expands
it (`contains` binds the expanded form), so whether a value "is still
a Point" at the print site would depend on the inference path. Only
`Abstract<...>` makes a name matter — that is what the newtype is for.

### Parameterized targets (Eric's case)

```graphix
type Point<'a> = {x: 'a, y: 'a};
impl<'a: SomeTrait> SomeTrait for Point<'a> { .. }
```

Allowed, inside `SomeTrait`'s package — the instance-with-context
form (`instance Show a => Show (Point a)`, `impl<T: Tr> Tr for
Point<T>`), and what makes structural impls USEFUL: the author saying
"any pair-of-equal-things is a SomeTrait when its element is". Four
rules, all standard:

1. Head tvars bind the constraints: every quantified tvar appears in
   the target; constraints discharge with `'a :=` what the use site's
   shape has there. Each required impl is for a strictly smaller
   component, so resolution terminates.
2. Lookup is by UNIFICATION, not exact key: `{x: i64, y: i64}` unifies
   with the head, binds `'a := i64`, then requires `SomeTrait for
   i64` — the author's own primitive impl — or fails naming the
   missing one. `{x: i64, y: string}` does not unify (equal fields
   demanded) and falls through. Exact-key hash first, then the
   parameterized heads — a short list, trait-author-only.
3. No overlap within the package: two heads that unify (`Point<'a>`
   and `{x: i64, y: i64}`) are refused — no specialization. The one
   place `contains`-style reasoning returns, confined to one
   package's own impl list.
4. A union satisfies a constraint iff EVERY member does:
   `Point<[i64, string]>` needs impls for `i64` and `string`; the
   call on the field is §3's generated select. Needed anyway for
   `Array<[i64, string]>: ToJson`.

Monomorphization does the rest: the body is a lambda generic in `'a`,
each use elaborates it with `'a := i64`, and the inner
`SomeTrait::m(self.x)` resolves statically — so it fuses. Symmetry:
for the core four this IS the typed walk — on `{x: Counter, y:
Counter}` it recurses into the fields and calls `Counter`'s `Display`
there; core's "impl" is `impl<'a: Display> Display for {..'a..}` for
every shape at once, in Rust.

The table is global, keyed by trait path + canonical target; impls
register when their MODULE loads, so a package's impls must be
reachable from its root. A REPL re-`impl` replaces, like re-`let`.

## 5. Trait generics — stage it

**v1: no parameters, no associated types.** Every method's type is the
trait's declared signature with `self := 'a`. That covers io in full,
plus `Show`/`Ord`/`Hash`. The property that makes v1 clean: TYPING
never needs resolution — the call's type is known from the trait
alone, and impl selection is a typecheck1 codegen decision, the same
place `try_static_resolve` lives. No chicken-and-egg with inference.

**v2: trait parameters, one impl per self type.** `trait WithErr<'e>`,
`impl WithErr<`MyErr> for File`. With the coherence rule "one impl per
self type" the parameters are OUTPUTS of impl selection — exactly the
associated-type behavior (`type Err;`) without projection types
(`'a::Err`) in unification, which is the hard part of associated types
everywhere. The cost v2 pays that v1 does not: selecting the impl now
BINDS tvars, so typing depends on resolution, and resolution order
matters. Worth it, later.

**Not planned: higher-kinded self (`self<'a>`).** The `Map` trait needs
`self` to be a type CONSTRUCTOR — kinds in unification, decomposition
of `Array<i64>` into `Array` applied to `i64`. Rust deliberately does
not have it. Two Graphix-specific reasons on top: `['a, null]` (Option)
is a structural union and cannot be a target; and the collection HOFs
are compiler intrinsics with per-slot reactive semantics
(`node/collection.rs`) — a `Map` trait abstracting over them is a
different project, not a trait feature.

## 6. Costs downstream of adopting this for io

- `StreamKind` dissolves into per-package custom values (`File` in
  fs, `TcpStream` in tcp, `TlsStream` in tls, `Pipe` in process,
  `Stdio` in io), each with `impl Read/Write/Close/Seek` bodies that
  are builtin references (`'sys_fs_read`). The phantom tag disappears;
  `shutdown`/`peer_addr` become a `Socket` trait over Tcp and Tls.
- Rust builtins that consume streams today (`json::read`, `xls::*`,
  `toml`, `pack`) read the `StreamKind` directly; under traits they
  cannot see another package's representation. Each splits into
  gx-side IO through the trait (`read_all` as a default method) and a
  Rust-side parse over `bytes`. Less Rust, more Graphix; real work.
- `.gxi` needs `impl Read for File;` (no bodies) so a consumer
  typechecks against the interface without the implementation.
- `Trait::method` paths: traits register as module-like scopes so
  `use io::Read::read` rides the existing import engine (`Env.names`).

## 7. Small notes on the sketch

- `val read` returns `bytes` in the trait and `u64` in the `File`
  impl — typo.
- `open` returns `File`, not `io::Stream<`File>`; `Stream` is gone.
- A default method is a lambda; per-call-site instantiation and effect
  inference treat it like any other. Nothing new.
- The trait should declare the self form (`self` vs `&self`): a
  gx-defined implementor that mutates (`impl Inc for Counter`) needs
  the reference, and the signature is the only place to say it.
- Method resolution keys on the SELF argument's type only. A second
  argument's type never selects an impl (no multi-dispatch); that
  keeps coherence a one-key lookup.

## 8. Core traits: Eq, Ord, Hash, Display (Eric, 2026-08-22)

Equality and printing are hacks on `Value`: there is no way to make a
type compare equal any way but structurally, or print any way but as
Graphix syntax. The nominal-abstract ruling makes this acute — a boxed
`Counter` gets `(id, payload)` equality and printing that nobody can
override, and the first newtypes anyone writes (a set as a sorted
array, a case-folded key, a handle whose identity is its id, a `Color`
printing as `#ff0000`) are exactly the cases where that default is
wrong. So the first traits are the canonical four.

**The rule (Eric's):** at a print or `==` site, look up the static
type; if it has an impl, call it; if not, the type-directed structural
case, recursing with the element types. That is the existing typed
walk with one hook — the typed printer already carries the static
type beside the value at every step (`TVal`), and `==` becomes the
same shape by carrying the type too. It is Haskell's derived instance
(`show [a]` calling `show a` per element) done by the walk instead of
by a materialized blanket impl per composite, so the structural case
stays ONE RUST LOOP. Outside core the four are implementable for
abstract types only (§4); core's structural default is the walk.

- Call-out is SYNCHRONOUS: an impl body is a node graph and the walk
  runs it inside the cycle. A call site's `update` IS a synchronous
  evaluation for a Sync body, so the walk owns a per-site call site
  per resolved impl (site identity, as with DynCall) and delivers the
  args. The trait declares these methods `#[sync]` and effect
  inference enforces it — no timers in `fmt`. The JIT helper calls
  the impl's kernel; both engines must agree bit-for-bit — a
  differential target.
- Union members resolve FIRST: `==` on `[Point, i64]` determines the
  member at runtime (the printer does this today), then looks up the
  member's impl.
- Under `Any`, a nominal abstract still finds its impl — the runtime
  tag IS the type id; a structural-shape impl needs the static type.
  The right asymmetry, and free.
- Sites with no static type — `Map` keys (the chunkmap comparator),
  the netidx wire, the JIT's `Value` total order — stay structural on
  `(id, payload)`. Rust has the same division (`BTreeMap` is generic
  over `Ord`; Graphix's `Map` is not). A documented v1 limit.
- Every type has a derived `Eq`, so `|a, b| a == b` inferring
  `'a: Eq` breaks nothing.

## 9. Rating against the alternatives (2026-08-22)

- Abstract fix: unconditional, and PRE-release — it is the breaking
  change (inside-module transparency ends). Additive features can
  follow a release; breaking ones cannot.
- Functors: no. The half Graphix can use (a bundle of types and
  values passed explicitly) it has structurally; the half it lacks
  (expression-level resolution by type) functors do not give — OCaml
  needed modular implicits on top.
- Type classes vs traits: the same thing (single-param, associated
  types, coherence, defaults) in Rust spelling, MONOMORPHIZED — which
  matches per-callsite instances and fuses; dictionary passing would
  de-fuse. What we forgo is higher-kinded classes, by choice.
- Nothing: off the table, because the abstract fix creates the need
  (§8). The dogfood log (netidx `graphix-admin-findings.md`) had hit
  the abstract problem and not asked for traits — but it also could
  not yet write a newtype whose `==` was wrong.

## 10. Proposal (revised 2026-08-22: io migrates NOW, API break accepted)

Eric: use the sketch's traits (`Read`/`Write`/`Close`/`Seek` over
`File`, `TcpStream`, …) for io in this arc, not the old `Stream`;
other types can then implement the io traits. The Rust `dyn
StreamKind` fix is DEAD — it would have been replaced immediately.

1. `nominal_abstract_types.md` (prerequisite; the breaking change).
2. v1 traits: declarations, required + default methods, impls over
   §4's targets (abstract anywhere; other types only in the trait's
   package), traits as constraints via the existing cell
   conjunctions, static resolution in typecheck1, union dispatch as a
   generated select (§3), `Trait::method` paths + `use`, `.gxi` impl
   declarations. Compile error on unresolved self.
3. The four core traits via the hooked typed walk (§8), and the
   fuzzer generator vocabulary for user `Eq`/`Display` impls — part of
   the feature, not after it.
4. io migration (§6) on the sketch traits: `StreamKind` dissolves
   into per-package Rust-backed abstracts, each with builtin-bodied
   impls; the stream-consuming Rust builtins (json/xls/toml/pack)
   split into gx-side IO through the trait + Rust-side parse.
5. After the release: v2 trait parameters with one-impl-per-self
   coherence.

## 11. v1 as built (2026-08-22, branch `nominal-abstracts`)

The §10 step-2 feature set, as it landed. Durable map; history is in
`git log`.

**Syntax** (`expr/parser/traitexp.rs`): `trait T { val m: fn(self, ..)
-> R [= default]; .. }`, `impl[<'a: C + D, ..>] T for Target [{ let m =
..; .. }]` (the bodiless form declares an impl in a `.gxi` or
implements an all-defaults trait), both also `SigKind` items. The
receiver type is spelled `self` — parsed as the type variable named
`self` (`typexp::self_tvar`), so same-named occurrences alias like any
quantifier; a fn-type positional written bare `self` is `self: self`;
`self` is also legal as a lambda parameter name and as a bare
expression (`resolve_visible` treats the single segment as a value
name). Bounds join with `+` (`typexp::bound`, flattened to one
`(tvar, conjunct)` pair per member everywhere a constraint list is
consumed). A value path may carry an uppercase interior segment
(`Read::read`, `io::Read::read` — `valpath`).

**Identity and registries** (`env.rs`): `TraitId::of(scope, name)` is
path-derived like `AbstractId`, so an interface's declaration and the
implementation's re-declaration are one trait. `Env.traits` maps names
per scope (lexical, like `typedefs`); `trait_defs` (by id),
`trait_methods` (dispatcher `BindId` → `(trait, method)`), and `impls`
(per trait, a list of `ImplDef`) are GLOBAL like `names`. The first
registration is the definition of record; a re-declaration adds its
compiled default bodies through `set_trait_defaults`. The trait's own
scope `<mod>::T` is entered in `env.modules` and its dispatcher
bindings live there — which is all it takes for `T::m` paths and `use
T::m` to ride the existing import engine. Default bodies compile as
typed bindings in a block under the DECLARING module (so they see its
items) with the trait scope glob-imported (so siblings are callable
bare); impl methods likewise under `<mod>::#impl<id>`. A method lambda
takes the declared signature (at the target) as its own parameter
annotations (`annotate_lambda`), which is what lets `|c| c.0` see
`c: Counter`.

**Constraint discharge** (`typ/contains.rs`): a trait `Ref` on the
left of `contains` is the predicate `trait_contains`: ⊥ yes, `Any` no,
a union iff every member, an open cell yes (the tvar merge carries the
conjunct) unless it is RIGID without the conjunct, a typedef by its
expansion, anything else by `Env::find_impl` — abstract targets by
id, other heads by unification against a fresh instantiation (head
bounds discharge through the cells, which is the `impl<'a: T> T for
P<'a>` rule) then equivalence; a type with an open interior cell never
matches. A trait on the right is contained only by `Any` or itself.
`settle` never picks a trait conjunct as a witness; a cell bounded by
traits alone stays open. `trait_of_ref` walks the table only for refs
whose resolution cell is empty (a typedef ref fills it on first use).

**Dispatch** (`node/callsite.rs`): `try_static_resolve` finds no
lambda behind a dispatcher `Ref` and calls `resolve_trait_call`: the
instantiated signature's self-argument type, resolved and
alias-expanded, selects the impl (or the trait's default); the call's
function node is RE-POINTED at that binding (`Ref::new`) and pre-bound
statically when the lambda is known — after which it is an ordinary
static call (fuses, effect-classified per impl). An open self type
inside a definition gate is the polymorphic case, left for the
instances; open at depth zero is the compile error §2 demands. A union
self type lowers the call to `{ let #s = self; let #a_i = arg_i; ..;
select #s { M as #t => #bind::N(#t, #a_i, ..), .. } }` — the
synthesized expression compiles under the call's scope, `#bind::N`
being the compiler's private spelling for a binding by id, and the
`CallSite` delegates every `Update` method to the lowered node. A
trait method passed as a HOF argument registers the instance's
parameter binding in `trait_methods` for the elaboration, and a
collection's runtime slots call the prototype's resolved definition as
a constant (`prototype_def`) rather than binding on the dispatcher's
(absent) runtime value.

**Argument-position traits** (`Type::rewrite_trait_args`): a trait as
a parameter's type becomes a fresh quantifier named `#<param>` bound by
the trait (rigid in the def gate, printed back as the trait); a trait
anywhere else is an error. Applied at `Lambda::compile`, `bind_sig`,
and `deftype`.

**Targets**: `check_target` enforces §4 — an abstract type in the
type's or the trait's package, anything else only in the trait's
package (`package_root` of the scopes — a whole program outside any
package is one package, so sibling modules may implement for
primitives), never a union, a bare variable, `Any` or ⊥;
`register_impl` refuses overlapping heads (`heads_overlap`: fresh
instantiations contain each other either way) except that an
implementation replaces the interface's `declared` entry for the same
head.

**Interfaces**: `bind_sig` registers a `trait` item like a typedef
(the declaration is prepended to the implementation's body by
`add_interface_modules`, like typedefs — a written re-declaration must
match) and an `impl T for X;` item as a `declared` impl whose method
bindings are minted with the signatures at the target; `check_sig`
requires the implementation to have replaced it.

**Found on the way — a pre-existing hole, fixed:** `Type::scope_refs`
re-minted type variables WITHOUT their cell constraints, so a
quantifier bound written in a `let` annotation or a `.gxi` `val`
(`fn<'a: Number>(x: 'a)`) was vacuous — `f("hi")` typechecked. The
re-mint now carries the (scoped) conjunction. Pinned by
`annotation_bound_enforced`.

**Known limits (v1):** no trait parameters or associated types (§5);
`type T = A + B` trait aliases are not built (write the bound
inline); an `impl` declared by a DYNAMIC module's interface has
method bindings but no proxy to the loaded source yet, so a consumer
compiled against it cannot dispatch statically; the union-dispatch select
de-fuses while abstract patterns do; `Hash` (§8) is not a trait —
nothing consults one in v1 (map keys stay structural), so it would
be dead API.

**A value occurrence is a call site (Eric, 2026-08-22):** a reference
to a GENERALIZED binding — a let-bound lambda, an interface `val`, a
trait dispatcher, or a `let g = f` forwarding one (`Env::poly_binds`)
— instantiates the signature afresh in `Ref::typecheck0`, exactly as
`CallSite::typecheck0` does for a call, with the same knots kept on
the definition's own cells (a self-reference inside the definition's
gate, a fn-typed parameter during its gate, the instance being
elaborated). Typecheck time, not compile time: the definition's gate
must have recorded the body's facts first (a compile-time copy gave a
callback instance a ⊥ parameter), and the call site typechecks a
`Ref` argument ahead of its operand pre-bind so the pre-bind never
sees the definition's cells. Before this, a polymorphic lambda used as
a value at one type pinned its own cells for every later use
(`array::map([1], f); array::map([1.5], f)` was refused) — the same
for dispatchers. Pinned by `poly_value_two_types` and
`trait_method_value_then_generic`.

## 12. The core traits as built (2026-08-23, branch `nominal-abstracts`)

§8 landed as `Eq`, `Ord` and `Display`, declared in core's interface
(`graphix-package-core/src/graphix/mod.gxi`: `trait Eq { val eq:
fn(self, other: self) -> bool }`, `trait Ord { val cmp: fn(self,
other: self) -> Ordering }` with `type Ordering = [`Less, `Equal,
`Greater]`, `trait Display { val fmt: fn(self) -> string }`). Their
ids are path-derived like every trait's, so the compiler names them
without a registration handshake (`node::coretraits::CoreTrait`).

**THE VALUE SEAM (Eric's design).** The first build hooked each SITE
(a plan over the static type per `==`, per interpolation part, per
print builtin) — and a site-by-site system cannot reach the places
ordering matters most: a map is keyed by the chunkmap comparator over
`Value`, which no plan can see. The shipped design hooks the VALUE
instead: netidx's abstract vtable routes `Value::eq`,
`Value::partial_cmp` and `{:?}` for a `Value::Abstract` to the
wrapped type's own Rust impls, and `GxAbstract`'s impls
(`abstract_value.rs`) consult a thread-local dispatch handle. One
seam covers every consumer at once — map keys (insert, lookup,
iteration order), `array::sort`, `min`/`max`, `uniq`, the comparison
operators on both engines (the JIT's `graphix_value_eq` helper calls
`Value::eq`), the typed printer (`TVal`'s abstract arm renders via
`{g:?}`), the naked printer, `dbg`'s and netidx's own debug output —
and `a == b`, `(a, x) == (b, y)`, and a map keyed by `a` all mean
the same thing by construction. The per-site plan machinery is
DELETED (`CmpDispatch`, the eq/cmp walks, `TVal::fmt_planned`,
`Shown`, `PlanNode::Dynamic` — under the seam a value dispatches on
its runtime tag wherever it sits, `Any` included, with no plan at
all).

**The ExecCtx hurdle: the loan.** `GxAbstract::{eq, cmp, Debug}` run
at arbitrary depth inside operations that can't take a context. The
frame that HOLDS `&mut ExecCtx`/`&mut Event` and is about to run a
comparing or printing operation loans them into the thread-local as a
type-erased handle for that operation's duration
(`coretraits::with_value_hooks` — the `DYN_DISPATCH_HANDLE` pattern,
per-holder reborrow, save/restore so loans nest). Armed sites: the
six comparison operators, the whole `EvalCached` family
(`CachedArgs::update` — min/max/all/sort/the map builtins in one
place), `uniq`, the map literal and `m{key}` nodes,
`Kernel::update`'s invocation, string interpolation, and the print
family's renders. No loan — another thread, a context with no core
impls (checked before arming: three map probes, nothing armed) —
means the structural case: publisher dedup, the wire, and the
REPL's handle-side echo stay structural, which is the conservative
answer for representation machinery.

**The dispatch.** A per-context registry (`ExecCtx.core_hook_sites`,
keyed `(trait, AbstractId)`) holds hook CALL SITES — a `genn::apply`
of the impl's method binding over synthesized argument bindings,
delivered through `event.variables` like a collection slot's
callback; built on first use, resolved-or-`None` STICKY, a POOL per
key so a re-entrant comparison (an impl whose body compares its own
type) mints a fresh site per activation. Every dispatch calls
`reset_replay` on its site first: a dispatch is a fresh logical
invocation, and a reused site otherwise carries replay history
across dispatches — the scrutinee ride re-emitted the PREVIOUS
pair's answer when a pair's computation bottomed (caught by the
bottom-key fixture; `reset_replay` is the frames mechanism for
exactly this — replay caches clear, semantic state survives).

**THE BOTTOM-KEY RULE (Eric's ruling).** A bottoming implementation
inside a Value comparison cannot bottom the chunkmap, and a
structural fallback per PAIR breaks the total order (two orders mixed
is intransitive) — as does any constant answer for bottoming pairs.
Per KEY it is total, and it is the NaN rule: a key the implementation
bottoms on sorts below every real key and equal to its fellow bottom
keys; real pairs answer by the implementation. Bottomness is detected
by SELF-PROBES (`cmp(k, k)`), run only on the bottom path; a pair
that bottoms while neither key self-bottoms is an inconsistent
implementation — warn and answer Equal (deterministic, symmetric).
`eq` follows the same shape (bottom keys equal each other, nothing
real); a bottoming `fmt` renders structurally with a warning
(printing has no algebra to preserve).

**Uniformity delta.** `a == b` on a hooked type with a bottoming impl
used to bottom the operator; under the seam it answers by the
bottom-key rule like every other consumer (Eric accepted the
uniformity trade). The operators' emission is UNCHANGED and `==`/`!=`
on abstracts FUSE via `graphix_value_eq` — better coverage than the
deleted root-lowering, with no lowering at all.

**What survives from the first build**: the dispatchers are the
operators (`CallSite::lower_core_call`: `Eq::eq(a, b)` ≡ `a == b`,
`Display::fmt(x)` ≡ `"[x]"`, `Ord::cmp` a select over `<`/`>`), so
the core traits hold as bounds for every type and the dispatchers
work on every value; `trait_contains` answers true for the three;
core-trait methods are implicitly `#[sync]` with prototype call
sites on the `Impl` node (`NodeView::Impl`) so the analysis covers
and enforces it; `bind::lower_over_operands` (built for the deleted
cmp lowering) remains the lowering device for union dispatch and the
dispatcher sugar — operand NODES move into `let #x` bindings, never
recompiled (recompiling source at typecheck1 cannot see a lambda's
parameters — the union-dispatch-in-a-lambda bug, pinned by
`trait_union_dispatch_in_lambda`).

**Trust and consistency (rulings, 2026-08-23):** no purity policing —
Rust doesn't forbid consulting a global in an `Ord` impl and neither
do we; an impl that isn't a consistent total order corrupts its maps
exactly as in Rust. An `Ord`-keyed map consults `Ord` only (like
`BTreeMap`); keeping `Eq` consistent with it is the implementor's
duty. No `Hash` trait — nothing consults one in v1; `GxAbstract`'s
structural `Hash` beside a hooked `Eq` means hash-keyed INTERNALS
(if any appear) would distinguish what `Eq` unifies — think about it
when and if `Hash` lands. Registry entries are sticky per context —
an impl loaded by a dynamic module after a tag's first comparison is
not picked up.

**Rust-backed abstracts** (`File`, `TcpStream`, …) don't route
through `GxAbstract`, so their comparisons stay structural until the
io migration registers them with path-derived UUIDs — at which point
either they wrap through `GxAbstract` or implement the same
thread-local consult; decide there.

## 13. The io migration as built (2026-08-23, branch `nominal-abstracts`)

§10 step 4 / §6, as it landed. io was the feature's first client and
its acceptance test: the sketch's `Read`/`Write`/`Close`/`Seek` over
per-package types, with `Stream<'a>`'s phantom tag gone.

**Five nominal types, one representation.** `sys::fs::File`,
`sys::tcp::TcpStream`, `sys::tls::TlsStream`, `sys::process::Pipe` and
`sys::io::Stdio` are five Rust-backed abstract types (declared
body-less in their `.gxi`s). Behind them `StreamKind` survives as ONE
enum — read/write/close is the same code whatever the descriptor is,
and dissolving it into five copies would have bought nothing the
nominal split doesn't already buy. What makes them five distinct RUST
types (which is what the abstract registry keys a UUID on) is a marker
parameter: `Stream<K: StreamMark>` with a `stream_kinds!` list minting
the markers, the wrappers and the accessor
(`graphix-package-sys/src/lib.rs`). `get_stream` reaches the shared
cell from any of the five, so the io builtins are shared: the TYPE
says which operations are legal and the trait implementations are what
enforce it.

**The traits** (`sys/graphix/io.gxi`): `Read { read; read_exact =
default; read_all = default }`, `Lines { lines; lines_batched }`,
`Write { write; write_exact = default; flush }`, `Close { close }`,
plus `sys::fs::Seek { seek }` and `sys::tcp::Socket { shutdown;
peer_addr; local_addr }` (implemented by `TlsStream` too — a TLS
session is still a socket, which is what §6 asked for). `read` is the
only method a `Read` implementation must supply; `read_exact` and
`read_all` are written in Graphix over it, and the system streams
OVERRIDE `read_exact`/`write_exact` with the builtin, which does the
loop under one lock. That split is the payoff: a stream written in
Graphix — a decoder, a framer, a test mock — gets the derived methods
for free, and the native ones keep their exact behavior.

`Lines` is a trait of its own rather than a `Read` default because
framing is at the BYTE level: a multi-byte character split across a
read boundary is destroyed by decoding each chunk on its own, and
nothing the caller controls decides where the boundary falls. Deriving
it in Graphix would need a byte-level `find`/`slice` vocabulary
(`buffer::` has `len`/`concat` and bytes slice, not search) and would
change the delivery cadence; a self-framing stream is an honest
separate capability. Fold it into `Read` if that vocabulary lands.

**The defaults are reactive loops**, and the connect in them must be
gated on the chunk: `acc <- b ~ buffer::concat(acc, b)`, never `acc <-
buffer::concat(acc, b)`. A connect fires when its RHS fires, so an
ungated accumulator re-fires on its own write — which is exactly the
documented counter idiom (`x <- x + 1`), an accumulator by accident.
The ungated form read 55 bytes from a 5-byte stream before EOF stopped
it.

**Consumers split** (§6): `json`, `toml`, `pack` and `xls` parse from
`bytes`/`string` and serialize to them — the stream input arm and
`write_stream` are gone, and with them their dependency on
`graphix-package-sys` entirely. Reading a document from a stream is
now `json::read(Read::read_all(f)?)` and writing one is
`Write::write_exact(f, json::write_bytes(v)?)`, which is the same code
for a file, a socket and a pipe.

**Rust-backed abstracts now register path-derived UUIDs.** Every
Rust-backed abstract type in the stdlib registers its wrapper under
`abstract_uuid(<its graphix path>)` — `graphix_package_core::
abstract_wrapper!` is the one-liner, and `impl_abstract_arc!` grew the
same form. That is what makes a runtime type test on one exact, which
in turn is what makes trait dispatch over a UNION of them work
(`Socket` over `[TcpStream, TlsStream]`, pinned by
`socket_union_dispatch`). Two consequences:

- The 2026-08-18 refusal of explicit predicates on Rust-backed
  abstract types is LIFTED (`node/pattern.rs`): its premise — the
  check can never succeed — was true only while no package registered
  a path-derived UUID. The contract is now the package's:
  `abstract_wrapper!` or your values match no type test, in your own
  tests. The tag test remains a NOMINAL test and not a full type
  check — an abstract type's parameters are not carried at runtime, so
  `Box<i64> as b` also matches a `Box<string>`, for minted and
  Rust-backed alike.
- A CORE trait implementation for a Rust-backed abstract is REFUSED
  (`traits::check_target`): the core traits ride the value through
  `GxAbstract`, and a Rust-backed value has no payload for the
  implementation to read, so such an impl would compile and never be
  consulted. Their equality, ordering and printing stay the ones their
  package defined. (This settles §12's open question.)

**Two things the migration found in the compiler.** An interface's
`impl` declaration is never spliced into the implementation, so it can
anchor nothing in `add_interface_modules` — everything declared after
one landed at the END of the module body, invisible to the code above
(`sys::process::Redirect`; fixed, pinned by `interface_type_after_impl`).
And a `//` comment between two select arms, or above a method inside
an `impl` block, was a parse error — comments attach to expressions,
and an arm's PATTERN is not one (the impl body bypassed the expression
entry altogether), so `0 => 1, // note \n _ => 2` failed with "can't
use keyword as a function or variable name" pointing at the next
`select`. Fixed the day after (2026-08-24): decorations captured above
a select arm's pattern, an impl method, or a struct-literal field
attach to the expression that follows (the arm's body, the method's
binding, the field's value), the printers put them back above the
pattern or the name, and the round-trip proptest now generates
comments at every such position — which is what caught the pretty
printer laying block items out by kind and dropping their comments.

The same pass closed the tree-sitter grammar's older hole: it had no
`#[..]` rule at all, so an attributed program was one big ERROR node
in every editor. `attribute` is now an `extra` alongside
`line_comment` — the grammar is permissive about where a decoration
sits and leaves the judgement to the compiler, as it already did for
comments — and `#[` is a single token, so it beats a labeled
argument's `#` by longest match. The proptest generates attributes now
too, which makes that lane the gate. `Decorations.trailing` went away
with the same change: the parser rejects a dangling comment, so
nothing ever filled it.

**API break** (accepted in §10): `io::read(s, n)` is `Read::read(s,
n)`, `fs::seek` is `Seek::seek`, `tcp::shutdown` is `Socket::shutdown`,
`process::Stdio` (the redirect config) is `process::Redirect` — the
name freed for `io::Stdio`, the handle — and `Child`'s pipe fields are
`[Pipe, null]`. A TLS upgrade now CONSUMES the TCP handle: the session
moves into the returned `TlsStream` and the handle passed in is left
empty, so a stray plaintext read on it errors instead of silently
reading the encrypted session. A failed upgrade leaves it untouched.
