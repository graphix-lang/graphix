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
| (no interface file)          | `type T = Abstract<u64>;` | public newtype: no interface hides anything |
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
let get = |c: Counter| c.0;                    // payload
let bump = |c: Counter| { let Counter(x) = c; Counter(x + u64:1) };   // destructure
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
   The payload pattern checks against the declared representation;
   over a union representation a constructor narrows to its member,
   as under any written type, so `Box(`A(x))` and `Box(`B(y))` are
   two refutable arms that together cover `Box` (the literal pool
   reads the payload's heads). In `let` the payload must cover the
   representation. A constructor pattern with no type test tests the
   tag alone at run time: its payload compiles at parameters of its
   own, so a literal in it never decides the arm's parameters, which
   come from the scrutinee, and after the select narrows the arm the
   payload's representation is narrowed against the scrutinee
   member's (`StructPatternNode::narrow_payloads`), typing its binds.
   Over `[Box<bool>, Box<i64>]`, `Box(b)` matches both with
   `b: [bool, i64]` and covers both.
3. `.0` is the payload whatever its shape: `Abstract<(u64, string)>`
   → `x.0.1`; `Abstract<{a: u64}>` → `x.0.a`; update is
   `T({x.0 with a: 1})`.
4. Parameters flow through: `type Box<'a> = Abstract<'a>`, constructor
   `fn<'a>(x: 'a) -> Box<'a>`. A value carries the parameters it was
   constructed at and a type test compares them, so `Box<i64> as b`
   does not match a `Box<string>`. A Rust-backed abstract's value
   carries its id alone, so a select over two instantiations of one is
   refused (they share a runtime form, `Type::rep_collision`).
   Constraints must match the gxi (`type NumBox<'a: Number>;`).
   Parameters are invariant: the representation is hidden, so nothing
   outside it can say a `Sink<i64>` is a `Sink<[i64, string]>`, and a
   Rust-backed handle (`db::Tree<K, V>`) may store what it is given.
   A constructed value's payload bounds its parameters from below
   (`TCell::lower`, `ContainsFlags::Lower`): `Box(1)` is `Box<'p>` with
   `'p` at least `i64`, so the type the value flows into decides `'p`
   (`let b: Box<[i64, string]> = Box(1)`, a return, an element, a
   field, select arms, a connect, a union of instantiations, which
   takes the member the payload fits), and the statement's settle binds
   one nothing decided to the payload's type. A use that binds `'p` to a
   type with open parts passes the bound on to them: `array::len(b.0)`
   over `B(["a"])` makes `'p` an `Array<'a>` with `'a` at least
   `string`. A definition that
   constructs (`|x| Box(x)`) keeps the bound in its signature, so each
   call's context decides its own, unless its body binds the parameter:
   then the argument is held to that binding, and the call's context
   decides nothing the argument fixed (`|x| { let b = Box(x);
   array::len(b.0); b }` is `fn(x: Array<'a>) -> Box<Array<'a>>`, so
   `let q: Box<Array<[string, i64]>> = g(["a"])` is refused where the
   same lines written inline are accepted). A statement's settle binds a
   parameter a ⊥ was produced into as well to the payload's type, never
   ⊥. A trait bound or a dispatch on the
   value reads its parameters and so settles them. An
   unknown constructor's argument (`'c<a>`) is invariant too. Declared
   variance is the extension if a covariant abstract container is ever
   needed.
5. The constructor is syntax, not a value: `Counter(x)` constructs, and
   a function that builds one is written `|x| Counter(x)`. Type and
   value names are separate namespaces, so `Counter(x)` resolves to the
   constructor when no VALUE `Counter` is in scope; a declaration
   shadows, as everywhere.

Equality is same tag, same parameters, then equal payloads (the payload
part overridable through the core traits, `traits.md`); a value prints as
`Counter(5)`. The parameters compare as regular trees
(`typ::regular::key`: every resolved reference expanded, recursion
included, unions flattened, then refined to canonical ranks), so an alias,
a cast's target as written and a construction's frozen type name one
parameter, and the orderings and hashes agree with equality.

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
declares, of the declaration's source and position too; for a dynamic
module whose path crosses a body, of the place its `mod` statement is
written (`declaration_place`, recorded on the module's `ScopeNames::site`
before each compile of its text), so two dynamic modules named alike in two
blocks are two types, while a file module is its file, one type wherever it
is loaded; minted at
`Env::deftype`, used for BOTH the
compile-time `Type::Abstract { id, params }` and the runtime tag. One
identity per type however many times its interface is read; the
fusion-shape determinism gate stays quiet; a `Counter` published over
netidx decodes as `Counter` in another process or build (a receiver
without the type holds it opaquely, which `Value::Abstract` supports).
The parse-time process counter it replaced made two parses of one
interface two types.

Runtime shape: `GxAbstract { id, name, params, payload }`
(`graphix-types/src/abstract_value.rs`), registered with netidx-value
when an execution state is built, before any value can decode; `params`
are the type arguments it was built at. Eq and
ord follow `(id, payload)` unless a core-trait impl rides the value;
hash covers the id and the payload, and the id alone where a hook loan
is installed and an `Eq` impl applies to the id (`eq_applies`: a user
`Eq` no payload hash could agree with). Hash, eq and ord all follow the
loan state, so a hashed or ordered container is filled and probed under
one loan state. Pack writes every field; abstract values nest at most 1024
deep in bytes (each level is a reader around the last), and the writer
refuses deeper ones with `TooBig`, as the reader would; only an image
session decodes one. Data never carries an abstract value: a type
crosses data through its `Cast` implementation, which only its defining
module writes (design/abstract_cast.md). `Type::Abstract`
carries the id and the params, so `Display` consults a process-global
`AbstractId → name` registry, filled when an id is minted and when a
restored environment decodes — diagnostics print `Box`, not the word
"abstract".

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
