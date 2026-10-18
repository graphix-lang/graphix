# Distinct runtime forms for tuples, structs and variants

Status: cost assessment, 2026-10-18. Decided 2026-10-18 (Eric): the
representation is the marker design below ("Decided: marker tags"), not
new netidx variants. Built: see "As built".

## The question

Graphix values are `netidx_value::Value`. Today four different kinds of
Graphix value share two `Value` forms:

| Graphix value | `Value` form today |
|---|---|
| tuple `(1, "a")` | `Array[1, "a"]` |
| struct `{x: 1}` | `Array[["x", 1]]`, pairs sorted by name |
| bare variant `` `A `` | `String("A")` |
| payload variant `` `A(1, 2) `` | `Array["A", 1, 2]` |
| list `[<1, 2>]` | cons cells `Array[1, Array[2, Array[]]]`; the empty list is the empty array |

So no run-time test can tell these apart from an array or a string. The
checker therefore refuses any select, dispatch or comparison that would
need to (`Type::rep_collision`, "one runtime form"). For an open union
member (`[string, 'b]`) that refusal either falls at the definition,
refusing most generic code, or needs a new interface-visible bound
(x-typecheck-patterns-05). This document costs giving tuples, structs and
variants (and optionally lists) forms of their own.

## The design costed

New `Value` enum variants that wrap the same payloads as today:

- `Tuple(ValArray)`
- `Struct(ValArray)`, still sorted `[name, value]` pairs
- `Variant(ValArray)`, the tag at index 0
- a bare tag, either `Tag(ArcStr)` or a one-element `Variant`
- optionally `List(..)`

This means:

- no extra allocation, and `Value` stays 16 bytes;
- netidx has 7 free one-hot `Typ` bits above `COPY_MAX` (`0x0001_0000`
  to `0x0040_0000`) and free Pack wire tags 28 to 63;
- a value's kind is its discriminant, as for every other kind.

The alternative, `Value::Abstract` wrappers, costs an `Arc` and a `Box`
per value and about 18 extra bytes on the wire. Its Eq, Ord and Hash go
through a vtable, and it prints as `abstract:<base64>`. Its one
advantage is that old netidx peers keep unknown abstracts as opaque
bytes instead of disconnecting. Not recommended.

## What it buys

- **Collisions removed.** From `rep_collision` and the open-member rule:
  - tuple, struct or variant against an array;
  - bare variant against a string;
  - tuple against struct against variant, now told apart by
    discriminant rather than by length or a sniff of the names.
- **Type tests become exact discriminant checks.** `is_a`, `has_form`,
  `member_of`, `shallow_discriminant` and `MemberFacts` stop sniffing
  shapes. Untyped printing and the untyped json writer stop guessing
  whether an array is a struct.
- **Simpler and cheaper JIT tests.** A kernel tests a tuple or struct by
  its disc. A bare variant is a disc test plus a tag compare. The string
  compare stays for the tag, but nothing has to ask "is this array a
  variant".
- **Lists, if included.** The spine sniff (the open CR at
  `cast.rs` List arm, t-expr-core-01) and "an empty list is the empty
  array" go away.

## What it does not buy

Maps against maps, errors against errors, references against references
and functions against functions still share a form. So:

- `Result<'a, E>` (`['a, Error<E>]`) still collides when `'a` may be an
  error;
- `[Map<K, V>, 'b]`, `[&T, 'b]` and `[fn(..), 'b]` still collide.

x-typecheck-patterns-05 still needs its call-site bound (option 2) or a
rule about errors. This change shrinks the problem; it doesn't close it.

## Cost: netidx

| Area | Work |
|---|---|
| `netidx-value` `Value`/`Typ` | New variants and `Typ` bits. About 16 exhaustive `Value` matches and 8 `Typ` matches that the compiler flags. `clone`/`clone_from` refcounted arms, `COPY_MAX` unchanged. |
| silent hazards in `op.rs` | `PartialEq` ends `(_, _) => false` and `PartialOrd` ends `unreachable!()`. Both must be extended by hand, or a new kind compares false or panics. Ord compares by `Typ` first, so the bits chosen fix where the new kinds sort. |
| Pack | New wire tags and decode frames: the decoder is an iterative state machine with Array/Map/Error frames. |
| text format | `print.rs` and `parser.rs` need syntax that does not collide with maps (`{`), e.g. `tuple:[..]`, `struct:[..]`, `` `A `` / `variant:[..]`. The netidx shell tools print `Typ` names. |
| derives | `FromValue`/`IntoValue` produce pair arrays and string tags today. About 128 derive uses across both repos, plus 40 hand impls in `convert.rs` (Rust tuples 2..20 map to `Value::Array`). The derive changes once; readers should accept the old forms too (old archives, old publishers). A cross-repo test, `netidx-tools/tests/derive_graphix_compat.rs`, pins derive output to Graphix's. |
| RPC | `netidx-protocols` rpc arguments are a pair array, Graphix's struct form; `sys::net::call` builds them. Convert at that boundary or teach rpc the struct form. |
| ffi | `ValueType` in `netidx-ffi/src/value.rs` and the cbindgen headers gain values and accessors; the C/C++ examples switch on the type. |
| wsproxy | The serde shape changes on its own (`{"type": "Struct", ..}`); the JS example and any JS clients must handle the new types. |

**Compatibility is the real netidx cost.**

- The wire handshake is a hard-coded version `3` with an exact-equality
  check and no feature negotiation.
- An old peer that receives one value of a new kind gets `UnknownTag`,
  and that tears down the whole publisher–subscriber connection, every
  subscription on it.
- New archive and container files are unreadable by old binaries.

The clean way is to bump the protocol version (an old peer then refuses
to connect, instead of dropping a live connection mid-stream) and the
archive and container file versions. Old files stay readable, since the
change only adds tags.

## Cost: Graphix

| Area | Work |
|---|---|
| `rep.rs`, `list.rs` | Become the real choke points: they claim to be, but about 20 compiler sites bypass them. Constructors: `node/data.rs` (struct, tuple, variant, struct-with), `node/error.rs` (the chain struct, `NullError`), `node/op.rs` (`ArithError`), `node/place.rs`, `fusion/lowering.rs` constant folding, `collection.rs` pairs, `graphix-rt` `field_id`. |
| `err!`/`errf!` | Build `Error(Array[tag, msg])`; 191 call sites, but only the two macro bodies change. |
| casts | `typ/cast.rs` has about 35 `Value::Array` sites. Tuple, Struct and Variant arms read the new forms. Data readers (json, toml and sqlite build pair arrays and strings) convert to them, so the casts that are identity today allocate a new outer value; the payload `ValArray` can be moved, not copied. |
| type algebra | `rep_collision`, `rep_overlaps`, `ArrView`, `SHARED_PRIMS`, `shares_form`, `rep_prims`, `MemberFacts` and `contains`' rule that a primitive `Array` contains tuples, structs and payload variants (`contains.rs:1435`) shrink. 26 test mentions of "same runtime form" flip from refused to accepted. |
| printers, Eq/Ord/Hash | `tval.rs` printers keyed by the new forms. Sort and map-key order change: today every bare variant sorts after every payload variant, whatever the tags. db key order follows. |
| JIT | `value_disc` gains `TUPLE`, `STRUCT` and variant discs. `LocalKind::Composite` carries its kind (its disc is rebuilt as `ARRAY` in `owned_words` and `array_result`). The constructors in `emit/nodes.rs` change. `call.rs`'s composite return check aborts on a non-`ARRAY` disc, so every Rust builtin returning a tuple or struct must return the new form. About 6 helpers accept only `Value::Array`, some silently answering empty, and need the kind. About 36 hard-coded `ARRAY`/`STRING` uses and 80 `AbiKind::{Tuple, Struct, Variant}` references to review. Image format bump. |
| stdlib | Rust code that reads or builds the forms by hand: gui (58 derives, 28 hand impls), tui (31 derives, 38 hand impls that match `Value::String` for bare variants), sys (10 derives, 12 inline tags), db (inline `Insert`/`Remove`/`Mismatch`, on-disk Pack values), core (`shape`, `map_key`, sort direction), and tuple producers in array, list, str and map. About 208 `"Tag" =>` arms survive; the read around them changes. netidx-admin has 83 derive lines. |
| data boundaries | json and toml write a struct as an object and a bare variant as a string, unchanged, and read them back through casts. pack and db write the new tags. Old db stores and packed bytes hold pair arrays and strings, which a read casts as today if the casts keep accepting the old forms. |
| fuzzer | `trace.rs` (`normalize`, `holds_lambda`, `same`) and `value_has_tag` (finds `` `TwinDiverged `` as a string). |

## Semantics that change

- **`cast<string>` of a bare variant** stops being the identity, and so
  does `cast<`A>("A")`. Under the cast rule ("a cast keeps a value's
  kind") a variant to string would need a rule of its own. Used in
  `examples/gui/data_table_dashboard.gx` (`cast<SortDirection>(v)`,
  `cast<string>(sort_dir)`).
- **Interpolation of a bare variant** writes `A` today because it is a
  string. It would print `` `A `` through `TVal` unless special-cased.
- **Variant map keys** written to json as `"A"` read back through the
  cast, as now.
- **Order:** sort, map-key and db-key order change for the new kinds.
- **Untyped data:** a struct received from a non-Graphix netidx publisher
  or an old file is a pair array and reads as a struct only through a
  cast, which keeps working if the casts accept both forms.

## Size

The work is comparable to the `&T`/`&mut T` campaign or larger:

- every engine, the JIT ABI, two repos, a wire-protocol version bump and
  every Rust package that reads Graphix structs or variants;
- most of the edits are mechanical once `rep.rs` and the derives are the
  real choke points;
- the risky parts are the JIT helpers that silently answer empty for an
  unknown form, and the two `op.rs` fallbacks.

## Recommendation

The design is sound and removes most of the "one runtime form" rule,
but it doesn't close x-typecheck-patterns-05 on its own. If you want it:

1. Make `rep.rs`/`list.rs` and the derives the only places that know
   the forms, first and on their own (no behaviour change). That makes
   the switch itself small and finds the bypasses.
2. Then switch the forms, with the protocol and file versions bumped.
3. Decide x-typecheck-patterns-05 for what remains (maps, errors,
   references, functions) separately.

## Decided: marker tags (Eric 2026-10-18)

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

- One allocation per tuple, struct and payload variant, as today,
  with one extra slot.
- A bare variant becomes an array: constants are shared, while variants
  built at run time or read from data allocate.

**What every boundary does.**

- Writers strip the markers (the `to_data` walk, for every composite).
- Reads add them through their cast, so a cast to a struct always
  rebuilds.
- Rust readers (the derives, and the gui/tui hand impls that read a
  bare variant as a string) either see stripped values or learn the
  marker.

**Order.**

1. Make `rep.rs`/`list.rs` the only code that knows the forms, with no
   behaviour change.
2. Switch to markers.

This removes the cross-kind collisions:

- a tuple, struct, variant or list against an array;
- a variant against a string;
- kinds against each other.

**Same-kind collisions remain.** Two maps, two errors, two references,
two functions, and two composites whose parts may collide (`[]` is any
array) are each one kind on both sides, so only types tell them apart.
For `Result<'a, E>` the proposal is a predicate bound on the typedef,
`type Result<'r: NoError, 'e> = ['r, Error<'e>]` (open). Any other
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

**Seq machines.** A machine's `pc` holds its step labels as bare variants
(`seq_machine.rs`, `SeqAbort`).

**Tests.** `run!` checks the value against its static type
(`testing::observed`) and hands the predicate `testing::data`;
`next_update` returns data, `next_update_raw` and `Fixture::new` the raw
value for Rust readers (the gui/tui harnesses). Embedders building
values in Rust use `rep` or a `GraphixShape` derive.
