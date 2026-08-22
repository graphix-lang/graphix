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

## 4. Impl targets in a structural type system

`impl Read for File` needs `File` to have an identity. Abstract types
and primitives have one; that is already the language's nominal escape
hatch, so impls over them are natural. For a structural shape —
`impl Show for {x: f64, y: f64}` — the honest semantics is the
structural one: the impl applies to EVERY type of that shape, aliases
being transparent. That is acceptable if it is said out loud.

Coherence = no two impls of one trait whose targets overlap;
overlap is `contains` in either direction, checked at impl
registration (structural subtyping makes it decidable). v1 targets:
abstract types (with params), primitives, builtin constructors
(`Array<'a>`, `Map<'k,'v>`, `bytes`, ...), exact structural shapes.
Refused in v1: unions (§3) and tvars (blanket impls — the overlap
check stops being a `contains` query).

Orphan rule by package: an impl lives in the trait's package or the
target's package. The table is global, keyed by trait path + target;
impls register when their MODULE loads, so a package's impls must be
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
stays ONE RUST LOOP. Impl targets are §4's — nominal abstracts,
primitives, exact structural shapes — not abstracts only.

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

## 10. Proposal

1. Now, cheaply: the Rust `dyn` fix for `StreamKind` + exported
   `wrap_stream`. `io::read(s, n)` call syntax is forward-compatible
   with `trait Read` + `impl Read for Stream`, so the io MIGRATION
   (§6) can follow the release without an API break.
2. `nominal_abstract_types.md` (prerequisite; the breaking change).
3. v1 traits: declarations, required + default methods, impls over
   nominal/primitive/exact-shape targets, traits as constraints via
   the existing cell conjunctions, static resolution in typecheck1,
   union dispatch as a generated select (§3), `Trait::method` paths
   + `use`, `.gxi` impl declarations. Compile error on unresolved self.
4. The four core traits via the hooked typed walk (§8), and the
   fuzzer generator vocabulary for user `Eq`/`Display` impls
   — part of the feature, not after it.
5. After the release: io migration (§6); v2 trait parameters with
   one-impl-per-self coherence.
