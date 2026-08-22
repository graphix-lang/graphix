# Nominal abstract types — box at the constructor

Status: PROPOSAL (Eric, 2026-08-22, refined in discussion). Companion
to `traits.md` §3, but its payoff is wider than trait dispatch.

## The rule

An abstract type (`type T;` in a `.gxi`) is ONE of:

1. **Rust-backed**: no representation on the gx side (`type T;` there
   too, or nothing). Values are minted by Rust through
   `AbstractWrapper::wrap`, already `Value::Abstract` with a registered
   UUID. Unchanged — 18 of the 21 abstract types today.
2. **Graphix-minted**: the gx side gives a representation
   (`type Counter = i64`). Values are `Value::Abstract` carrying
   `(type uuid, payload)`, minted ONLY by the type's constructor.

There is no third kind: a gx representation never flows as its bare
rep. That is the whole change, and everything below follows from it.

## Construction and destruction

The type's name is its constructor, in expression and pattern
position, visible exactly where the definition is today (the defining
module and its subtree):

```graphix
// counter.gxi
type Counter;
val make: fn(x: i64) -> Counter;
val get: fn(c: Counter) -> i64;

// counter.gx
type Counter = i64;
let make = |x| Counter(x);
let get = |c| { let Counter(x) = c; x };   // irrefutable destructure
```

`select v { Counter(x) => ... }` destructures; `Counter as c` is the
type TEST and is legal ANYWHERE, since it is a tag comparison — this
is what lifts select's refusal of abstract predicates and what trait
dispatch over a union of abstracts needs (`traits.md` §3).
Parameterized types construct with inference (`Box(x)`); the runtime
tag does not carry the parameters (erased, as in Rust), so
`[Box<i64>, Box<string>]` members are not distinguishable by tag —
the same limit a variant has today.

Type and value names are separate namespaces (`Env.typedefs`), so
`Counter(x)` resolves to the constructor when no VALUE `Counter` is in
scope; a declaration shadows, as everywhere.

## Why this is the prize

Today an abstract type has TWO VIEWS: transparent inside its module
(`Counter` IS `i64` there — `let get = |c: Counter| -> i64 c`), opaque
outside. Every hard abstract-type bug has been the two views meeting:
`AbstractOpaque` and the `privatize_type` retries in
`CallSite::typecheck0` / `check_instance_type`, the dual-view sig
registrations in `check_sig`, `resolve_abstract` and the cell-expanding
`freeze_for_abi` Ref arms in fusion, the `list::List` double-id
(`list_fold_list_acc_interprets`) — 128 sites across 21 compiler
files.

Under the rule there is ONE view. `Counter` is nominal everywhere;
`contains` on two Abstracts is id + params, full stop; the rep is
reachable only through the constructor's and destructure's types. The
retry/privatize apparatus has nothing left to bridge and can be
deleted. The runtime box is what makes the nominal type HONEST —
select-able, dispatchable, serializable with its tag — but the
compile-time simplification is the bigger win, and it is the reason to
do this even if traits never happen.

## Identity

`AbstractId::new()` is a process counter minted AT PARSE
(`expr/parser/typexp.rs:406`, re-minted on deserialize): two parses
of one interface are two types. Replace it with a deterministic id —
a hash of the canonical path `package::module::Name` — used for BOTH
the compile-time `Type::Abstract` id and the runtime UUID. Consequences:
one identity per type however many times its interface is read;
`detcheck` stays quiet; a `Counter` published over netidx decodes as
`Counter` in another process or build (a receiver without the type
holds it opaquely, which `Value::Abstract` already supports).

Runtime shape: one Rust type `GxAbstract { id: Uuid, payload: Value }`
registered once with netidx-value; eq/ord/hash/Pack/Debug derive from
`(id, payload)`. `==` between `Counter` and `i64` is a type error, as
it is outside the module today.

## Cost

One `Arc` allocation per construction — a scalar newtype is no longer
a register scalar. Fusion still covers it (a 2-word owned Value shape;
`Counter(x)` is an allocating helper, `let Counter(x) = c` a borrowing
one), but `type Meters = f64` in a hot loop pays for its box. The
guidance that follows: an abstract type is a HANDLE or a NEWTYPE, not
a hot data structure.

That names the one migration question. `list::List<'a>` is a
recursive variant — already self-describing by its `Cons`/`Nil` tags
— and boxing would add an `Arc` per cons cell to a representation
whose per-element allocation is already the ~15x gap to the array
twins. `List` is abstract only to hide its rep; ML and Haskell expose
theirs. Recommendation: make `List` transparent (`Cons`/`Nil` public),
which also aligns with the planned move of List into the compiler
beside Array. `Color` and `Shortcut` (structs, constructed at init)
take the box with no visible cost.

## Migration

Three gx-defined abstracts exist: `gui::Color`, `gui::menu::Shortcut`,
`list::List` (→ transparent). Each module's internals gain explicit
`T(x)` / `let T(x) = v`. The Rust-backed 18 do not move.

## Sequence

1. Deterministic ids (parse-time counter → path hash). Independent,
   fixes the double-id class on its own.
2. The constructor/destructure syntax + `GxAbstract` runtime box;
   inside-module transparency OFF.
3. Delete the two-view machinery; select accepts abstract predicates.
4. Migrate the three types; `List` goes transparent.
