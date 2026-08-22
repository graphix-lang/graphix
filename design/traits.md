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
cannot mean that, and should be an error pointing at `dyn Read` (§3).

## 2. Static dispatch is right, and it is the only sound choice

The sketch makes an unresolvable method call a compile error. Agreed,
for a reason stronger than taste: there is no runtime witness. A
gx-defined abstract type (`type Counter = i64`) ERASES — the value is
an `I64`, indistinguishable from any other — which is why select
refuses abstract type predicates today. Rust-backed abstract values do
carry a witness (`Value::Abstract` + `downcast_ref`), but a dispatch
rule that works for sys's types and fails for the user's is a trap,
not a feature.

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

## 3. A union of implementors is not an implementor — io needs `dyn`

This is the gap in the sketch. Today

```graphix
let s = select use_tls { true => tls::connect(..)?, false => tcp::connect(..)? }
```

has type `Stream<[`Tls, `Tcp]>` and every io function accepts it,
because the Rust enum dispatches at runtime. Under static traits
`[Tls, Tcp]` does not implement `Read` — `'a: Read` needs ONE impl, and
a union has none — so `read(s, n)` is a compile error. That program
is not exotic; it is how every server chooses its transport.

Rust solves it with `dyn`, and the vtable is attached at the COERCION
site, where the concrete type is known. Graphix has no coercion sites:
subtyping is `contains`-subsumption, a `File` flows into a `Read`
position with no node in between, and inserting one at every
subsumption edge (unions included — `[File, i64]` into `[Read, i64]`)
is a large and hidden change. So make it explicit:

- `dyn Read` is a TYPE (and `dyn Read + Write`).
- Packing is explicit, at a site where the type is concrete:
  `cast<dyn Read>(f)` reads naturally with the existing cast syntax.
  Explicit is in keeping with "no hidden allocation".
- Representation: a record of closures over the witness, generated
  from the impl — `{read: |n| File::read(f, n), read_exact: ...}`.
  That is exactly what a programmer can write by hand today, which is
  the proof it is not a new runtime concept; the feature is that the
  compiler writes it from the impl, and that `read(d, n)` on a
  `dyn Read` resolves to the field.
- A call through `dyn` is a call through a closure value: it does not
  fuse, and the `dyn` in the source says so. Static for the fast
  path, `dyn` where heterogeneity is genuinely dynamic.

With `dyn` the io story closes: `json::read` takes `'a: Read`; the
tls-or-tcp program packs once at the select.

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

## 8. Proposal

1. Now, cheaply: the Rust `dyn` fix for `StreamKind` + exported
   `wrap_stream`, so packages with Rust can produce Streams while the
   language feature is designed. Independent of everything below.
2. v1 traits: declarations, required + default methods, impls over
   nominal/primitive/exact-shape targets, traits as constraints via
   the existing cell conjunctions, static resolution in typecheck1,
   `Trait::method` paths + `use`, `.gxi` impl declarations. Compile
   error on unresolved self.
3. `dyn Trait` + `cast<dyn T>(x)` with the closure-record encoding.
   Ship with v1 — io cannot migrate without it (§3).
4. Migrate io to it; split the stream-consuming Rust builtins.
5. v2: trait parameters with one-impl-per-self coherence.
