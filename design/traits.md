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

**The plan** (`coretraits::Plan`): the hook is decided from the STATIC
type, once, at a site. `Plan::build(env, trait, typ)` walks the type
the way the typed printer walks it (deref, typedef expansion with a
per-ref memo so recursive types close into a cycle, union members in
set order, struct fields in type order) asking `Env::find_impl` at
every node; a node is *hooked* iff a hook is reachable from it — a
fixpoint over the arena, since a recursive type's nodes form a cycle,
which is what keeps a `List<Counter>` honest where an optimistic
collapse would have declared the cons cell structural. `build`
answers `None` when the root isn't hooked, and short-circuits to
`None` before any walk when the program registers no impl of the
trait at all — the fast path, and the whole cost for every program
that implements none of the three.

**The walk**: three functions over a plan, each the structural rule
with the implementation substituted at hooks. `coretraits::eq` and
`cmp` mirror `Value::eq` / `Value::partial_cmp` (depth-first
lexicographic, length as the tiebreak, struct field names and variant
tags structural) and delegate WHOLE unhooked subtrees to the `Value`
operators — so where no hook sits, the answer is the structural
answer by construction, not by reimplementation. `Display` is the
typed printer itself: `TVal::fmt_planned` carries the plan step
beside the type step, and `TVal`'s `Display` impl is the same walk
with `NoHooks`. A hook is a call site the node OWNS
(`coretraits::Hooks`: one `genn::apply` per hook over synthesized
argument bindings, delivered through `event.variables` the way a
collection slot feeds its callback, first dispatch under a forced
init view) — site identity per use site, as with DynCall. A hook
that produces nothing bottoms the comparison or the print: bottom in,
bottom out.

**The sites**: the six comparison nodes choose a `CmpDispatch` in
`typecheck1` — `Structural`; `Lowered` when the WHOLE operand type
has an implementation (the operands re-compile as the arguments of a
static call to the impl's method — `Eq::eq(l, r)`, negated for `!=`,
`Ord::cmp(l, r) == \`Less` and its three siblings — which fuses like
any call); `Walk` when an implementation sits inside the type (the
region de-fuses; an ASPIRE). `StringInterpolate` builds a plan per
part at `typecheck1` (and re-reads each part's type there — a cell
still open at tc0, a rec definition's return, used to freeze the part
as `Any` and print it naked). `print`/`println`/`dbg`/`log` format
through `Shown` (package-core), which builds the plan on the FIRST
render from the argument's settled type: a builtin's type-derived
state must exist after `init` + `typecheck0` alone, because the
DynCall mint runs no `typecheck1` — so the hook works from inside a
fused kernel's dispatch too. The node walkers (`for_each_node`) see
hook sites as children, so effect analysis reaches the impl bodies
from any compile-time site.

**Sync enforcement**: a core trait's method is implicitly `#[sync]`
(the `Impl` node adds the attribute to each method binding), and the
`Impl` node builds one never-run PROTOTYPE call site per method
(`Impl::prototypes`, a `NodeView::Impl` child) so the analysis covers
the body whether or not any compile-time site calls it — the
existing `check_def_assertions` then refuses an async body at
compile time (`core_method_async_refused`).

**The dispatchers are the operators**: `Eq::eq(a, b)` lowers to
`a == b`, `Display::fmt(x)` to `"[x]"`, `Ord::cmp(a, b)` to a select
over `(a < b, a > b)` (`CallSite::lower_core_call`), and
`trait_contains` holds the three for every type — so a core trait
works as a bound on anything, the dispatcher works on anything, and
an implementation is reached exactly where the operator reaches it.
The union case falls out: the operator's walk selects the member.

**Limits (v1)**: the shell's REPL echo prints structurally (it has an
`Env`, not a cycle); `Any`-typed values print/compare structurally
even when the runtime value is an abstract with an impl (the
"runtime tag is the type id" clause of §8 is not built — a site with
no static type has no plan); `array::sort`, `min`/`max`, map keys, the
wire and the JIT's `Value` order stay structural as §8 says; the
hooked walk de-fuses its region.
