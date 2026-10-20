# Distinct runtime forms for tuples, structs and variants

Status: built 2026-10-18 (Eric: marker tags, netidx unchanged).
Pins: `stdlib/graphix-tests/src/lang/types.rs`
(`discernible_refuses_one_runtime_form`), `stdlib/graphix-tests/src/lang/select.rs`
(`same_form_references_refused` and the open-member pins after it),
`lib_tests::json::untyped_arrays_cast_to_lists_whole`, must-reject
family 10 (`design/must_reject.md`).

## The problem

Graphix values are `netidx_value::Value`, which has no tuple, struct or
variant kind. Without a mark of its own, a tuple, a struct, a payload
variant and a list cell would all be arrays and a bare variant a string,
so no run-time test could tell them apart from an array, a string or
each other, and the checker would have to refuse every select, dispatch
or comparison that needs to (`Type::rep_collision`, "one runtime
form"). New netidx `Value` variants would have changed the wire
protocol, Pack, the archive formats and every Rust reader in both
repositories; the design below changes none of them.

## The design (Eric 2026-10-18)

Netidx is not changed. A Graphix tuple, struct, variant and list stays a
`Value::Array`, with a kind marker flattened into element 0:

- `[TMARK, a, b]` for a tuple;
- `[SMARK, ["x", 1], ["y", 2]]` for a struct;
- `[VMARK, "A", p..]` for a payload variant;
- `[VMARK, "A"]` for a bare variant;
- `[LMARK, h, t]` for a list cell.

**The markers.** Each marker is a Rust-registered netidx abstract, one
per kind, that no Graphix type can name. It crosses no data (the `Cast`
rules refuse Rust-backed abstracts) and decodes only under an image. So
no array's element 0 is ever a marker, and arrays need no tag.

**Cost.**

- One allocation per tuple, struct and payload variant, with one slot
  for the marker.
- A bare variant becomes an array: constants are shared, while variants
  built at run time or read from data allocate.

**What every boundary does.**

- Writers strip the markers (the `to_data` walk, for every composite).
- Reads add them through their cast, so a cast to a struct always
  rebuilds.
- Rust readers (the derives, and the gui/tui hand impls that read a
  bare variant as a string) either see stripped values or learn the
  marker.

`rep.rs` and `list.rs` are the only code that knows the forms. This
removes the cross-kind collisions:

- a tuple, struct, variant or list against an array;
- a variant against a string;
- kinds against each other.

**Same-kind collisions remain.** Two maps, two errors, two references,
two functions, and two composites whose parts may collide (`[]` is any
array) are each one kind on both sides, so only types tell them apart.
For `Result<'a, E>` it is a predicate bound on the typedef,
`type Result<'r: NoError, 'e> = ['r, Error<'e>]` (built,
`design/tvar_constraints.md`). Any other
generic union with an open member beside one of these is refused at the
definition, as `==` refuses it today.

## As built

**Forms** (`graphix-types/src/rep.rs`, `list.rs`). One registered abstract,
`Marker(Kind)`, `Kind` one of `Tuple`, `Struct`, `Variant`, `List`; a static
`Value` per kind (`rep::marker`), so pushing one is a refcount bump. A
marker packs as its kind byte and decodes only under an image session
(`ids::in_session`). `rep::kind` reads a value's marker; `TUPLE_BASE`,
`STRUCT_BASE` and `VARIANT_TAG` are 1. A list cell is `[LMARK, h, t]` and
nil is `[LMARK]`, one static value, so `[]` and `[<>]` differ. The strict
readers (`tuple_elems`, `struct_fields`, `variant`, `array_elems`) see only
their own kind; `read_tuple`/`read_struct`/`read_variant` also take data
and report `Parts::Data`, which a cast rebuilds; `GraphixShape` (the
derives) builds marked and reads leniently.

**Data.** `rep::strip` writes a value as data: a tuple as an array, a
struct as its pairs, a bare variant as its tag string, a payload variant
as `[tag, payload..]`, a list as a flat array. `abstract_value::to_data`
strips once, then converts abstracts. Every reader casts, so `FLAT_LISTS`
and `cast_data` are gone, and db reads every stored value back through
its cast (`data_target`).

**Casts.** A union target tries the members of the value's own class
first, a marked value's own class being its kind and a data string's
being `string` (`in_own_class`, `carries_tag` read strictly): data `"A"`
cast to `` [string, `A] `` stays a string, to `` [`A, `B] `` becomes the
variant. `cast<string>` of a bare variant is its tag; no other marked
value has a primitive form, and the primitive `array` holds arrays only
(`contains`, `is_a`, `setops`). A list casts to an array element by
element, and an untyped array to a list likewise.

**Types.** `rep_overlaps`/`arr_overlaps` compare kinds first, then arity,
names, tag and parts; `MemberFacts` keys the shallow discriminant by
kind. What still shares a form: two references (under a select arm; `==`
compares them by what they name), two function types, a Rust-backed
abstract at two instantiations. `Discernible` therefore refuses only
function unions and the open cells beside them. The must-reject family 10
mutates a select into a reference test (`design/must_reject.md`).

**Printing.** The naked printer renders a marked value in Graphix syntax
(`(..)`, `{f: ..}`, `` `A(..) ``, `[<..>]`) and descends into errors, as
the typed printer does; interpolating a bare variant gives `` `A ``.

**JIT.** Constructors push the marker through
`graphix_value_buf_push_marker`; tuple and array literals emit apart
(`emit_seq_new_node`); a bare variant is an array constant; tuple reads,
HOF destructure leaves and the tuple fold accumulator index from
`TUPLE_BASE`.

**Open members** (x-typecheck-patterns-04/05, Eric 10-18: refuse at the
definition). A select arm's type test against what else reaches it is
judged again at the statement's settle with open cells read as any type
they may yet bind (`PendingSettle::Told`), and a union dispatch reads its
open members the same way (`dispatch_members` keeps them): beside a
reference, a function, an error, a map, an array or a Rust-backed abstract
an open member is refused ("annotate it"). A `NoError` cell shares no
error's form, so a select over `Result<'a, E>` stays legal.

**Seq machines.** A machine's `pc` holds its step labels as bare variants
(`seq_machine.rs`, `SeqAbort`).

**Tests.** `run!` checks the value against its static type
(`testing::observed`) and hands the predicate `testing::data`;
`next_update` returns data, `next_update_raw` and `Fixture::new` the raw
value for Rust readers (the gui/tui harnesses). Embedders building
values in Rust use `rep` or a `GraphixShape` derive.
