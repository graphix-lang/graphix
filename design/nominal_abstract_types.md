# Nominal abstract types — box at the constructor

Status: RULED 2026-08-22 (Eric): every graphix-defined abstract type is
boxed; `list::List` goes transparent. BUILT the same day on branch
`nominal-abstracts` (see CLAUDE.md "Nominal abstract types" for the
as-built map). Companion to `traits.md` §3, but its payoff is wider
than trait dispatch.

## The rule

A type whose body is `Abstract<rep>` is NOMINAL: its name is a runtime
tag, and its values are `Value::Abstract` carrying `(type uuid,
payload)`, minted only by the type's constructor. Whether the body is
HIDDEN is the interface's business, as for any type — the two are
orthogonal:

| gxi                          | gx                        | meaning                                   |
|------------------------------|---------------------------|-------------------------------------------|
| `type T;`                    | `type T = Abstract<u64>;` | hidden newtype (today's abstract type)    |
| `type T = Abstract<u64>;`    | —                         | public newtype: anyone constructs         |
| —                            | `type T = Abstract<u64>;` | module-private nominal type, no gxi needed|
| `type T;`                    | `type T;` / nothing       | Rust-backed: Graphix never constructs     |
| `type T;`                    | `type T = {x: i64};`      | ERROR: a hidden type is Abstract or Rust  |

There is no other kind: a rep never flows bare. A gxi `type T;` over
a transparent gx body is refused outright (RULED 2026-08-22) — that
was the one remaining way to give a bare rep an opaque name, i.e. the
two-view case itself. The Rust-backed row is 18 of the 21 abstract
types today and does not change.

## The three faces

```graphix
// counter.gxi
type Counter;
val make: fn(x: u64) -> Counter;
val get: fn(c: Counter) -> u64;

// counter.gx
type Counter = Abstract<u64>;
let make = |x| Counter(x);                     // construct
let get = |c| c.0;                             // payload
let bump = |c| { let Counter(x) = c; Counter(x + 1) };   // destructure
```

1. `Abstract<...>` is legal only as the ENTIRE body of a named `type`:
   the name is the tag. Nested (`Array<Abstract<u64>>`) or anonymous
   is an error — nothing to name it.
2. `T(v)`, `x.0`, and the pattern `T(x)` (irrefutable in `let`, an arm
   in `select`) are visible exactly where the DEFINITION is. The type
   test `T as t` is visible wherever `T` is — it is a tag comparison,
   which is what lifts select's refusal of abstract predicates and
   what trait dispatch over a union needs (`traits.md` §3).
3. `.0` is the payload whatever its shape: `Abstract<(u64, string)>`
   → `x.0.1`; `Abstract<{a: u64}>` → `x.0.a`; update is
   `T({x.0 with a: 1})`.
4. Parameters flow through: `type Box<'a> = Abstract<'a>`, constructor
   `fn<'a>(x: 'a) -> Box<'a>`. The runtime tag does not carry `'a`
   (erased, as in Rust), so `[Box<i64>, Box<string>]` members are not
   distinguishable by tag — the same limit a variant has.
5. The constructor is an ordinary fn value (`array::map(xs, Counter)`).
   Type and value names are separate namespaces (`Env.typedefs`), so
   `Counter(x)` resolves to the constructor when no VALUE `Counter` is
   in scope; a declaration shadows, as everywhere.

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
theirs. RULED: `List` goes transparent (`Cons`/`Nil` public), which
also aligns with the planned move of List into the compiler beside
Array; everything else is boxed. `Color` and `Shortcut` (structs,
constructed at init) take the box with no visible cost.

## Migration

Three gx-defined abstracts exist: `gui::Color`, `gui::menu::Shortcut`,
`list::List` (→ transparent). Each module's internals gain
`type T = Abstract<rep>` and explicit `T(x)` / `.0`. The Rust-backed 18 do not move.

## Sequence

1. Deterministic ids (parse-time counter → path hash). Independent,
   fixes the double-id class on its own.
2. `Abstract<rep>` bodies, the three faces, the `GxAbstract` runtime
   box; inside-module transparency OFF.
3. Delete the two-view machinery; select accepts abstract predicates.
4. Migrate the three types; `List` goes transparent.
