# Traits

Status: built 2026-08-24 (v1, the core traits `Eq`/`Ord`/`Display`, the io traits)
Pins: `stdlib/graphix-tests/src/lang/traits.rs` (`annotation_bound_enforced`, `poly_value_two_types`, `trait_method_value_then_generic`, `trait_union_dispatch`, `trait_union_dispatch_in_lambda`, `trait_dynamic_impl_*`, `trait_dynamic_core_impl`, `trait_interface_outer_default`, `dynamic_module_typecheck1`, `core_impl_interface_declared`, `core_impl_interface_rust_backed_refused`, `interface_type_after_impl`), `stdlib/graphix-tests/src/lib_tests/tls.rs` (`socket_union_dispatch`)
Supersedes: interfaces.md

## 1. Why a language feature

The forcing case was io. `io::Stream<'a>` was a closed Rust enum of
tokio types with a phantom tag parameter typing the socket-only
operations; no package outside sys could produce one. Half of that had
a Rust-only fix (a `dyn` stream plus an exported wrapper). The other
half is what justifies a language feature: a stream written IN GRAPHIX
— a buffered reader over a stream, a decoder, a record framer, a test
mock — could never be a `Stream`, because the only way to be one was to
be a Rust value. The same wall stood in front of every abstraction a
package might want to leave open: printing, ordering for a user-defined
sort key, a serializer. The feature stands on that; io is the first
client.

The nominal-abstract ruling (`nominal_abstract_types.md`) made it
acute: a boxed `Counter` gets `(id, payload)` equality and printing
nobody can override, and the first newtypes anyone writes (a set as a
sorted array, a case-folded key, a handle whose identity is its id, a
`Color` printing as `#ff0000`) are exactly where that default is wrong.

Alternatives rejected: **functors** — the half Graphix can use (a
bundle of types and values passed explicitly) it has structurally, and
the half it lacks (expression-level resolution by type) functors do not
give; **dictionary passing** — a new value shape and a dynamic call at
every method call, which de-fuses; **nothing** — off the table once the
abstract fix created the need. Type classes and traits are the same
thing in Rust spelling, MONOMORPHIZED, which matches per-call-site
instances and fuses; higher-kinded classes are forgone by choice
(`self<'a>` is the one constructor form, §5).

## 2. A trait is an open predicate on a cell

A tvar constraint is already an open predicate: `fn<'a: Number>` seeds
`'a`'s cell with a conjunct, and binding the cell checks every conjunct
with `contains` (`typ/contains.rs`, `cell_constraints_ok`). `Number` is
a closed set and `+` is, in effect, its method with builtin impls. A
trait is the same mechanism with one change: membership is OPEN —
declared by `impl`, not enumerated by the compiler. `Read + Write` is a
two-conjunct cell.

It must stay a PREDICATE, never an eager union. If a signature froze
`Read` into `[File, Tcp, Tls]` when it was checked, a package loaded
later (or an `impl` typed at the REPL) could never call it. A
constraint is checked at INSTANTIATION against the table as it stands
then, so generic code written before an impl existed accepts it.

**Scope (Rust's rule).** A trait is a NAME and resolves like any other
— by declaration, `use io::Read;`, or the prelude. Impls are not names;
they are global facts (§4). The trait name (in `'a: Read`,
`Read::read(f, n)`, `impl Read for File`) needs the trait in scope; a
method called BARE (`read(f, n)`) needs the item (`use io::Read::read`
or `use io::Read::*` — traits register as module-like scopes and the
import engine does the rest); constraint discharge, the generated
union select (§3) and parameterized-head lookup (§4) resolve by trait
IDENTITY through the global table, so a generic `action` declared where
`Read` is in scope is callable from a module that never imports `Read`,
and `==`/interpolation find the core traits with no name at all. There
is no method-call syntax — `f.read(n)` stays field access — so the
language never searches scope for a method by the receiver's type.

`Read` in an argument position is a fresh `'a: Read` per occurrence
(`fn(a: Read, b: Read)` is two variables — Rust's `impl Trait` rule).
In any other position (return type, struct field, array element) it
cannot mean that and is an error; a struct parameterizes
(`type Server<'a: Read> = {conn: 'a}`).

## 3. Dispatch is static, and a union self generates the select

An unresolvable method call is a compile error: a self type still an
unbound tvar after typecheck has no runtime witness to fall back on,
and the only alternative would be dictionary passing. Static resolution
also gives, for free: effect inference sees the callee (a trait
method's Sync/Async classification is per IMPL — `File::read` is
async, a mock's is not), and fusion sees the callee (a `Display`/`Ord`
call over scalars is an ordinary static call and fuses).

The enabler is per-call-site elaboration: each instance of a generic
lambda re-checks its body with the call's types, and static resolution
happens in `CallSite::typecheck1`; method resolution is the same pass.
The error fires in one shape only — a polymorphic lambda never called
through a static site (stored in a struct field or a `&`, called
through a dynamic CallSite) — and names the site and the open
variable.

A self type that is a UNION of implementors is a different, decidable
case. `let s = select use_tls { true => tls::connect(..)?, false =>
tcp::connect(..)? }` types as `[TlsStream, TcpStream]`, and
`read(s, n)` desugars to the select the programmer would otherwise
write, with a static call in each arm:

```graphix
select s { TlsStream as t => TlsStream::read(t, n), TcpStream as t => TcpStream::read(t, n) }
```

Everything follows from that being the desugaring: semantics need no
new ruling (the generated select is a sleep boundary the user did not
write, but the hand-written select does exactly the same); primitive
and structural members (`Display` on `[i64, string]`) cost nothing
because select has those predicates; abstract members discriminate by
the runtime tag — Rust-backed and Graphix-minted alike carry a
path-derived id (`nominal_abstract_types.md`). One mechanism, the tag
test, serves trait dispatch and lifts select's refusal of abstract
predicates.

What this leaves uncovered is a value whose implementor set is
unknowable at the type — Rust's `dyn`. In a language that types every
value the union is always inferable at the call site, and a library
struct parameterizes like a Rust generic struct. No `dyn` until a real
program demands one; the closure-record encoding is the fallback and a
programmer can write it by hand today.

## 4. Impl targets: abstract anywhere; anything else only in the trait's package

Impls are GLOBAL once their module loads — scope governs only whether
a method's NAME may be written bare, never whether an impl applies.
Coherence demands it: a value must be equal to the same things and
print the same way everywhere (a map built under one `Eq` and queried
under another is silent corruption), and the core traits are used
implicitly by `==` and interpolation.

In a structural language a type IS its shape, so an impl on a
structural target applies to every type of that shape, program-wide.
That is a bomb when strangers can write it: `impl Display for Point`
with `type Point = {x: i64, y: i64}` in package A changes how package
B's unrelated `{x: i64, y: i64}` prints the moment A loads, and a
second such impl in B is a conflict between two packages that never
heard of each other. The one legitimate non-abstract case names the
rule: a user trait is useless without impls over builtin shapes
(`impl ToJson for i64`, `for string`, `for Array<'a: ToJson>` —
Rust's `impl<T: Tr> Tr for Vec<T>`), and those are written by the
TRAIT'S AUTHOR, who is answerable for the trait's semantics over every
shape.

**The rule:** an impl target is either an ABSTRACT type (`Abstract<rep>`
or Rust-backed), impl'd in the type's package or the trait's (the
orphan rule); or ANY other type (primitive, constructor with
constrained element tvars, struct, tuple, variant, alias) — but ONLY
inside the trait's own package. Unions are never targets (members
resolve first, §3); a bare tvar as the whole target (blanket impl) is
not offered; `Any` and ⊥ refuse. Coherence is one key — one impl per
(trait, canonical type) — and a conflict can only ever be between two
impls in one package: the author's own bug, never a load-time surprise
between strangers.

Consequences. Core writes NO impls: its structural default for the
core traits IS the typed walk (§6), so outside core those traits are
implementable for abstract types only. A struct or ADT that should
carry behaviour gets a name — `type Shape = Abstract<[`Circle(f64),
`Rect(f64, f64)]>` — at the cost of one unwrap before each `select`.
NAME-DIRECTED dispatch on a transparent alias (`Point`'s impl for
things called `Point`) is not offered because it cannot be honest:
`Type::Ref` keeps the alias name for printing and compression but
unification expands it, so whether a value "is still a Point" at the
print site would depend on the inference path. Only `Abstract<...>`
makes a name matter.

**Parameterized targets** — `type Point<'a> = {x: 'a, y: 'a}; impl<'a:
SomeTrait> SomeTrait for Point<'a> { .. }` — are allowed inside
`SomeTrait`'s package: the instance-with-context form, and what makes
structural impls useful. Head tvars bind the constraints (every
quantified tvar appears in the target; each required impl is for a
strictly smaller component, so resolution terminates); lookup is by
UNIFICATION against a fresh instantiation of the head, not exact key
(`{x: i64, y: string}` does not unify and falls through); two heads
that unify within the package are refused — no specialization; a union
satisfies a constraint iff EVERY member does. Monomorphization does the
rest: the body is a lambda generic in `'a`, each use elaborates it,
and the inner `SomeTrait::m(self.x)` resolves statically — so it
fuses.

## 5. Staging

**v1 (built): no trait parameters, no associated types.** Every
method's type is the trait's declared signature with `self := 'a`. The
property that makes v1 clean: TYPING never needs resolution — the
call's type is known from the trait alone, and impl selection is a
typecheck1 decision, the same place `try_static_resolve` lives.

**v2 (deferred): trait parameters, one impl per self type.** With the
coherence rule "one impl per self type" the parameters are OUTPUTS of
impl selection — associated-type behaviour without projection types in
unification, which is the hard part of associated types everywhere.
The cost v1 does not pay: selecting the impl BINDS tvars, so typing
depends on resolution and resolution order matters. Build it against a
real module when one asks. The first candidate is **arithmetic as
traits**: `datetime - datetime` is refused by design (arithmetic is
`fn('a: Number, 'a) -> 'a`; datetime/duration arithmetic is
`sys::time` functions). The principled endgame is what `Eq`/`Ord`
already are for `==` and `<` — the operators dispatch statically on the
left operand's type, primitives keep the native fast path, user
abstract types get operators (`Meters + Meters`; `Meters + Seconds`
refused), datetime/duration become core impls. The useful cases are
HETEROGENEOUS (`datetime - datetime -> duration`, `datetime + duration
-> datetime`, `duration * f64 -> duration`), so the trait is Rust's
`Sub<Rhs> { type Output }`: a trait parameter for the right operand and
an associated result type. A homogeneous `trait Arith` covers only
`duration + duration`.

**Constructor self (`self<'a>`)** is built as the last-parameter hole:
`trait Collection { val fold: fn(self<'a>, ..) .. }` in core's
interface, with `Type::App` for an application whose constructor is
open and `'_` as the hole spelling; a filled application IS its filled
type (`Type::app_filled`). Design and rationale in
`recursive_activations.md` §7. `['a, null]` (Option) is a structural
union and cannot be a target.

Also not built: trait aliases (`type T = A + B`; write the bound
inline) and `Hash` — nothing consults one (map keys are `Ord`-keyed),
so it would be dead API.

## 6. Core traits: `Eq`, `Ord`, `Display`

Declared in core's interface (`graphix-package-core/src/graphix/
mod.gxi`): `trait Eq { val eq: fn(self, other: self) -> bool }`,
`trait Ord { val cmp: fn(self, other: self) -> Ordering }` with
`type Ordering = [`Less, `Equal, `Greater]`, `trait Display { val fmt:
fn(self) -> string }`. Their ids are path-derived like every trait's,
so the compiler names them without a registration handshake
(`node::coretraits::CoreTrait`).

**The rule:** at a print or comparison site, if the value's type has
an impl, call it; if not, the type-directed structural case, recursing
with the element types — Haskell's derived instance done by the walk
instead of by a materialized blanket impl per composite, so the
structural case stays ONE Rust loop. Every type has the derived
behaviour, so `|a, b| a == b` inferring `'a: Eq` breaks nothing; the
core traits hold as bounds for every type.

**THE VALUE SEAM.** The first build hooked each SITE (a plan over the
static type per `==`, per interpolation part, per print builtin), and
a site-by-site system cannot reach the place ordering matters most: a
map is keyed by the chunkmap comparator over `Value`, which no plan
can see. The shipped design hooks the VALUE instead: netidx's abstract
vtable routes `Value::eq`, `Value::partial_cmp` and `{:?}` for a
`Value::Abstract` to `GxAbstract`'s own Rust impls
(`abstract_value.rs`), which consult a thread-local dispatch handle.
One seam covers every consumer at once — map keys (insert, lookup,
iteration order), `array::sort`, `min`/`max`, `uniq`, the comparison
operators on both engines (the JIT's `graphix_value_eq` calls
`Value::eq`), the typed and naked printers, `dbg` — and `a == b`,
`(a, x) == (b, y)` and a map keyed by `a` mean the same thing by
construction, wherever the value sits, `Any` included. `==`/`!=` on
abstracts fuse with no lowering at all.

**The loan.** `GxAbstract::{eq, cmp, Debug}` run at arbitrary depth
inside operations that cannot take a context. The frame that HOLDS
`&mut ExecCtx`/`&mut Event` and is about to run a comparing or printing
operation loans them into the thread-local as a type-erased handle for
that operation's duration (`coretraits::with_value_hooks`, the
`DYN_DISPATCH_HANDLE` pattern — per-holder reborrow, save/restore so
loans nest). Armed sites: the six comparison operators, the
`CachedArgs` family (min/max/all/sort/the map builtins), `uniq`, the
map literal and `m{key}` nodes, `Kernel::update`'s invocation, string
interpolation, and the print family. No loan — another thread, a
context with no core impls (probed before arming) — means the
structural case: publisher dedup, the wire and the REPL's handle-side
echo stay structural, the conservative answer for representation
machinery.

**The dispatch.** A per-context registry (`ExecCtx.core_hook_sites`,
keyed `(trait, AbstractId)`) holds hook CALL SITES — a `genn::apply` of
the impl's method binding over synthesized argument bindings, delivered
through `event.variables` like a collection slot's callback; built on
first use, resolved-or-`None` STICKY (an impl loaded by a dynamic
module after a tag's first comparison is not picked up), a POOL per
key so a re-entrant comparison (an impl whose body compares its own
type) mints a fresh site per activation. Every dispatch calls
`reset_replay` on its site first: a dispatch is a fresh logical
invocation. Core-trait methods are implicitly `#[sync]`, with
prototype call sites on the `Impl` node (`NodeView::Impl`) so the
effect analysis covers and enforces it — no timers in `fmt`.

**THE BOTTOM-KEY RULE.** A bottoming implementation inside a Value
comparison cannot bottom the chunkmap, and a structural fallback per
PAIR breaks the total order (two orders mixed is intransitive), as does
any constant answer for bottoming pairs. Per KEY it is total, and it is
the NaN rule: a key the implementation bottoms on sorts below every
real key and equal to its fellow bottom keys; real pairs answer by the
implementation. Bottomness is detected by SELF-PROBES (`cmp(k, k)`),
run only on the bottom path; a pair that bottoms while neither key
self-bottoms is an inconsistent implementation — warn and answer
`Equal`. `eq` follows the same shape; a bottoming `fmt` renders
structurally with a warning (printing has no algebra to preserve).
`a == b` on a hooked type with a bottoming impl therefore answers by
the bottom-key rule like every other consumer rather than bottoming
the operator — uniformity over per-site behaviour.

**Trust and consistency.** No purity policing — Rust does not forbid
consulting a global in an `Ord` impl and neither do we; an impl that
is not a consistent total order corrupts its maps exactly as in Rust.
An `Ord`-keyed map consults `Ord` only (like `BTreeMap`); keeping `Eq`
consistent with it is the implementor's duty. A core-trait
implementation for a Rust-backed abstract is REFUSED
(`traits::check_target`): the core traits ride the value through
`GxAbstract`, and a Rust-backed value has no payload for the impl to
read, so such an impl would compile and never be consulted. The
refusal is the IMPLEMENTATION's, never an interface declaration's: at
signature time a hidden `type X;` has no representation yet
(`check_target`'s `declared` flag).

The dispatchers are the operators (`CallSite::lower_core_call`:
`Eq::eq(a, b)` ≡ `a == b`, `Display::fmt(x)` ≡ `"[x]"`, `Ord::cmp` a
select over `<`/`>`), so `trait_contains` answers true for the three
and the dispatchers work on every value. `bind::lower_over_operands`
is the lowering device for union dispatch and the dispatcher sugar —
operand NODES move into `let #x` bindings, never recompiled source
(recompiling at typecheck1 cannot see a lambda's parameters).

## 7. v1 as built

**Syntax** (`expr/parser/traitexp.rs`): `trait T { val m: fn(self, ..)
-> R [= default]; .. }`, `impl[<'a: C + D, ..>] T for Target [{ let m =
..; .. }]` (the bodiless form declares an impl in a `.gxi` or
implements an all-defaults trait), both also signature items. The
receiver type is spelled `self` — parsed as the type variable named
`self` (`typexp::self_tvar`), so same-named occurrences alias like any
quantifier; a fn-type positional written bare `self` is `self: self`;
`self` is also legal as a lambda parameter name and a bare expression.
Bounds join with `+` (`typexp::bound`, flattened to one `(tvar,
conjunct)` pair per member). A value path may carry an uppercase
interior segment (`Read::read`, `io::Read::read` — `valpath`).

**Identity and registries** (`env.rs`): `TraitId::of(scope, name)` is
path-derived like `AbstractId`, so an interface's declaration and the
implementation's re-declaration are one trait. `Env.traits` maps names
per scope (lexical, like `typedefs`); `trait_defs` (by id),
`trait_methods` (dispatcher `BindId` → `(trait, method)`) and `impls`
(per trait, a list of `ImplDef`) are GLOBAL like `names`. The first
registration is the definition of record; a re-declaration adds its
compiled default bodies through `set_trait_defaults`. The trait's own
scope `<mod>::T` is entered in `env.modules` and its dispatcher
bindings live there — which is all it takes for `T::m` paths and `use
T::m` to ride the import engine. Default bodies compile as typed
bindings in a block under the DECLARING module (so they see its items)
with the trait scope glob-imported (so siblings are callable bare);
impl methods likewise under a per-impl scope. A method lambda takes the
declared signature (at the target) as its own parameter annotations
(`annotate_lambda`), which is what lets `|c| c.0` see `c: Counter`.

**Constraint discharge** (`typ/contains.rs`): a trait `Ref` on the
left of `contains` is the predicate `trait_contains`: ⊥ yes, `Any` no,
a union iff every member, an open cell yes (the tvar merge carries the
conjunct) unless it is RIGID without the conjunct, a typedef by its
expansion, anything else by `Env::find_impl` — abstract targets by id,
other heads by unification against a fresh instantiation (head bounds
discharge through the cells) then equivalence; a type with an open
interior cell never matches. A trait on the right is contained only by
`Any` or itself. `settle` never picks a trait conjunct as a witness; a
cell bounded by traits alone stays open. `trait_of_ref` walks the table
only for refs whose resolution cell is empty.

**Dispatch** (`node/callsite.rs`): `try_static_resolve` finds no
lambda behind a dispatcher `Ref` and calls `resolve_trait_call`: the
instantiated signature's self-argument type, resolved and
alias-expanded, selects the impl (or the trait's default); the call's
function node is RE-POINTED at that binding and pre-bound statically
when the lambda is known — after which it is an ordinary static call.
An open self type inside a definition gate is the polymorphic case,
left for the instances; open at depth zero is the compile error. A
union self type lowers the call to `{ let #s = self; let #a_i = arg_i;
..; select #s { M as #t => #bind::N(#t, #a_i, ..), .. } }` — `#bind::N`
being the compiler's private spelling for a binding by id — and the
`CallSite` delegates every `Update` method to the lowered node (the
generated select de-fuses: coverage residue, `FuseExpect::None`). A
trait method passed as a HOF argument registers the instance's
parameter binding in `trait_methods` for the elaboration, and a
collection's runtime slots call the prototype's resolved definition as
a constant (`prototype_def`) rather than binding on the dispatcher's
absent runtime value.

**A value occurrence is a call site.** A reference to a GENERALIZED
binding — a let-bound lambda, an interface `val`, a trait dispatcher,
or a `let g = f` forwarding one (`Env::poly_binds`) — instantiates the
signature afresh in `Ref::typecheck0`, exactly as `CallSite::typecheck0`
does for a call, with the same knots kept on the definition's own cells
(a self-reference inside the definition's gate, a fn-typed parameter
during its gate, the instance being elaborated). Typecheck time, not
compile time: the definition's gate must have recorded the body's facts
first, and a call site typechecks a `Ref` argument ahead of its operand
pre-bind so the pre-bind never sees the definition's cells. Without
this a polymorphic lambda used as a value at one type pinned its cells
for every later use (`array::map([1], f); array::map([1.5], f)` was
refused).

**Argument-position traits** (`Type::rewrite_trait_args`): a trait as
a parameter's type becomes a fresh quantifier named `#<param>` bound by
the trait (rigid in the def gate, printed back as the trait); a trait
anywhere else is an error. Applied at `Lambda::compile`, `bind_sig` and
`deftype`, after `scope_refs` — which re-mints type variables WITH
their cell constraints, so a bound written in a `let` annotation or a
`.gxi` `val` is enforced (`annotation_bound_enforced`).

**Targets**: `check_target` (`node/traits.rs`) enforces §4 —
`package_root` of the scopes decides the package; a whole program
outside any package is one package, so sibling modules may implement
for primitives. `register_impl` refuses overlapping heads
(`heads_overlap`: fresh instantiations contain each other either way).

**Interfaces — a declared impl is the entry of record.** `bind_sig`
registers a `trait` item like a typedef (the declaration is prepended
to the implementation's body by `add_interface_modules`; a written
re-declaration must match) and an `impl T for X;` item as a `declared`
impl whose method bindings are minted from the trait's signatures at
the target. The implementation's `impl T for X { .. }` in the same
module FULFILS it (`register_impl` returns the declaration instead of
registering the implementation) and `check_sig` proxies each declared
method binding to the binding behind it — the implementation's own for
a method it writes, the trait's default for one it leaves — exactly as
a `val` proxies (`node::module::Proxy`; the default's binding is
shared, so its production is copied, not moved). Every consumer
therefore resolves to the SAME bindings whether it compiled before or
after the implementation exists, which is what a dynamic module needs:
its consumers compile against the signature alone, and a reload mints
fresh implementation bindings behind unchanged declared ones. Static
modules take the same path. A dynamically loaded source is
`typecheck1`'d and has the signature's declarations spliced in like a
file, so a sig-declared trait is in scope in the source it governs.

## 8. The io traits

io was the feature's first client and its acceptance test; the old
`Stream<'a>` and its phantom tag are gone and the API break was
accepted pre-release.

**Five nominal types, one representation.** `sys::fs::File`,
`sys::tcp::TcpStream`, `sys::tls::TlsStream`, `sys::process::Pipe` and
`sys::io::Stdio` are Rust-backed abstract types (body-less in their
`.gxi`s). Behind them `StreamKind` survives as ONE enum — read/write/
close is the same code whatever the descriptor, and five copies would
buy nothing the nominal split does not. What makes them five distinct
Rust types (which is what the abstract registry keys a UUID on) is a
marker parameter: `Stream<K: StreamMark>` with a `stream_kinds!` list
minting the markers, wrappers and accessor
(`graphix-package-sys/src/lib.rs`); `get_stream` reaches the shared
cell from any of the five, so the io builtins are shared. The TYPE
says which operations are legal and the trait implementations enforce
it.

**The traits** (`sys/graphix/io.gxi`): `Read { read; read_exact =
default; read_all = default }`, `Lines { lines; lines_batched }`,
`Write { write; write_exact = default; flush }`, `Close { close }`,
plus `sys::fs::Seek { seek }` and `sys::tcp::Socket { shutdown;
peer_addr; local_addr }` (implemented by `TlsStream` too — a TLS
session is still a socket). `read` is the only method a `Read`
implementation must supply; `read_exact` and `read_all` are Graphix
over it, and the system streams OVERRIDE `read_exact`/`write_exact`
with the builtin, which loops under one lock. That split is the
payoff: a stream written in Graphix gets the derived methods for free,
and the native ones keep their exact behaviour.

`Lines` is its own trait rather than a `Read` default because framing
is at the BYTE level: a multi-byte character split across a read
boundary is destroyed by decoding each chunk on its own, and nothing
the caller controls decides where the boundary falls. Deriving it in
Graphix would need a byte-level search vocabulary and would change the
delivery cadence; fold it into `Read` if that vocabulary lands.

**The defaults are reactive loops**, and their accumulator connect
must be gated on the chunk: `acc <- b ~ buffer::concat(acc, b)`, never
`acc <- buffer::concat(acc, b)`. A connect fires when its RHS fires,
so the ungated form re-fires on its own write — the counter idiom
(`x <- x + 1`) by accident.

**Consumers split**: `json`, `toml`, `pack` and `xls` parse from
`bytes`/`string` and serialize to them — no stream input arm, and no
dependency on `graphix-package-sys`. Reading a document from a stream
is `json::read(Read::read_all(f)?)` and writing one is
`Write::write_exact(f, json::write_bytes(v)?)`, the same code for a
file, a socket and a pipe.

**Rust-backed abstracts register path-derived UUIDs**
(`abstract_wrapper!`, `impl_abstract_arc!`'s path form), which makes a
runtime type test on one exact and trait dispatch over a union of them
work (`Socket` over `[TcpStream, TlsStream]`,
`socket_union_dispatch`). Explicit predicates on Rust-backed abstract
types are therefore accepted; the contract is the package's —
`abstract_wrapper!` or your values match no type test. The test is
NOMINAL, not a full type check: parameters are not carried at runtime.

**API**: `Read::read(s, n)`, `Seek::seek`, `Socket::shutdown`;
`process::Redirect` is the redirect config (the name `Stdio` freed for
the handle); `Child`'s pipe fields are `[Pipe, null]`. A TLS upgrade
CONSUMES the TCP handle: the session moves into the returned
`TlsStream` and the handle passed in is left empty, so a stray
plaintext read on it errors instead of silently reading the encrypted
session; a failed upgrade leaves it untouched.
