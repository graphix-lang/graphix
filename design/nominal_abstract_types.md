# Nominal abstract types — box at the constructor

Status: built 2026-08-22
Pins: `stdlib/graphix-tests/src/lang/interfaces.rs` (`abstract_*`: `abstract_construct_outside_refused`, `abstract_payload_outside_refused`, `abstract_hidden_alias_refused`, `abstract_public_newtype`, `abstract_pattern_let`, `abstract_pattern_select`, `abstract_type_test_union`, `abstract_equality_and_print`)

## The rule

A type whose body is `Abstract<rep>` is NOMINAL: its name is a runtime
tag, and its values are `Value::Abstract` carrying `(type id, payload)`,
minted only by the type's constructor. Whether the body is HIDDEN is
the interface's business, as for any type — the two are orthogonal:

| gxi                          | gx                        | meaning                                    |
|------------------------------|---------------------------|--------------------------------------------|
| `type T;`                    | `type T = Abstract<u64>;` | hidden newtype                             |
| `type T = Abstract<u64>;`    | —                         | public newtype: anyone constructs          |
| —                            | `type T = Abstract<u64>;` | module-private nominal type, no gxi needed |
| `type T;`                    | `type T;` / nothing       | Rust-backed: Graphix never constructs      |
| `type T;`                    | `type T = {x: i64};`      | ERROR: a hidden type is Abstract or Rust   |

There is no other kind: a rep never flows bare. A gxi `type T;` over a
transparent gx body is refused outright — that was the one remaining
way to give a bare rep an opaque name, i.e. the two-view case itself.

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
   is an error — there is nothing to name it.
2. `T(v)`, `x.0`, and the pattern `T(x)` (irrefutable in `let`, an arm
   in `select`) compile exactly where the DEFINITION is visible
   (`Env::abstract_reps`, gated by `AbstractRep::public` or the scope
   prefix). Every compile of one definition (its check, each instance)
   registers the same representation, so deleting one compile's `type`
   leaves it (`Env::undeftype` drops only the name). The type test
   `T as t` is visible wherever `T` is — it is
   a tag comparison, which is what lets `select` accept abstract
   predicates and what trait dispatch over a union needs
   (`traits.md`).
3. `.0` is the payload whatever its shape: `Abstract<(u64, string)>`
   → `x.0.1`; `Abstract<{a: u64}>` → `x.0.a`; update is
   `T({x.0 with a: 1})`.
<!-- CR claude for claude: [doc-drift] Point 4 is stale. GxAbstract carries the type
arguments it was constructed at (graphix-types/src/abstract_value.rs:92-95) and
Type::is_a compares them (graphix-types/src/typ/cast.rs:676-681), so `Box<i64> as b`
does not match a Box<string>, as the pin lang::traits::abstract_test_matches_parameters
asserts. The runtime-shape paragraph (98-101) leaves out `params` and points at
graphix-compiler/src/abstract_value.rs; the file is graphix-types/src/abstract_value.rs.
The comment above `match &type_predicate` at
graphix-compiler/src/node/pattern.rs:1196-1198 repeats the stale claim and should go.
(t-cast-setops-16) -->
<!-- 2026-10-07 claude: the runtime-shape paragraph is fixed (t-expr-core-09); point 4
and the pattern.rs comment still stand. -->
4. Parameters flow through: `type Box<'a> = Abstract<'a>`, constructor
   `fn<'a>(x: 'a) -> Box<'a>`. The runtime tag does not carry `'a`
   (erased, as in Rust), so `Box<i64> as b` also matches a
   `Box<string>` — the same limit a variant has. Constraints must
   match the gxi (`type NumBox<'a: Number>;`).
5. The constructor is an ordinary fn value (`array::map(xs, Counter)`).
   Type and value names are separate namespaces, so `Counter(x)`
   resolves to the constructor when no VALUE `Counter` is in scope; a
   declaration shadows, as everywhere.

Equality is same tag + equal payloads (overridable through the core
traits, `traits.md`); a value prints as `Counter(5)`.

## Why: one view instead of two

Before this rule an abstract type had TWO VIEWS: transparent inside its
module (`Counter` IS `i64` there), opaque outside. Every hard
abstract-type bug was the two views meeting — the `AbstractOpaque`
retries in call-site checks, the dual-view signature registrations, the
cell-expanding freeze arms in fusion, the double-id class — 128 sites
across 21 compiler files.

Under the rule there is ONE view. `Counter` is nominal everywhere;
`contains` on two Abstracts is id + params, full stop; the rep is
reachable only through the constructor's and destructure's types. The
retry/privatize apparatus had nothing left to bridge and was deleted.
The runtime box is what makes the nominal type HONEST — select-able,
dispatchable, serializable with its tag — but the compile-time
simplification is the bigger win.

## Identity

`AbstractId::declared(scope, name, ..)` (`typ/mod.rs`) is a v5 UUID
of the canonical path less the components a function body or block mints
(whose ids differ per instance and per process), and, for a type a body
declares, of the declaration's source and position too; minted at
`Env::deftype`, used for BOTH the
compile-time `Type::Abstract { id, params }` and the runtime tag. One
identity per type however many times its interface is read; the
fusion-shape determinism gate stays quiet; a `Counter` published over
netidx decodes as `Counter` in another process or build (a receiver
without the type holds it opaquely, which `Value::Abstract` supports).
The parse-time process counter it replaced made two parses of one
interface two types.

Runtime shape: `GxAbstract { id, name, params, payload }`
(`graphix-types/src/abstract_value.rs`), registered once with
netidx-value; `params` are the type arguments it was built at. Eq and
ord follow `(id, payload)` unless a core-trait impl rides the value;
hash is the id alone (a user `Eq` no payload hash could agree with);
Pack writes every field. `Type::Abstract` carries the id and the
params, so `Display` consults a process-global `AbstractId → name`
registry filled at `AbstractId::of` — diagnostics print `Box`, not the
word "abstract".

Rust-backed abstracts register the same path-derived UUIDs
(`abstract_wrapper!`, `impl_abstract_arc!`'s `= "pkg::mod::Type"`
form), which is what makes a type test on one exact.

## Cost, and what an abstract type is for

One `Arc` allocation per construction — a scalar newtype is no longer
a register scalar. Fusion still covers it (an opaque 2-word
`AbiKind::Value`), but `type Meters = f64` in a hot loop pays for its
box. The guidance: an abstract type is a HANDLE or a NEWTYPE, not a hot
data structure. That is why `List` is not one: a cons cell boxed per
construction would hand back everything its slim representation saves,
so `List<'a>` is a compiler-known constructor beside `Array`
(`list_native.md`). `gui::Color` and `gui::menu::Shortcut` (structs,
constructed at init) take the box with no visible cost.
