# Native List: slim representation, literals, patterns

Status: built 2026-08-31
Pins: `stdlib/graphix-tests/src/lang/lists.rs`, `graphix-types/src/expr/parser/test.rs` (`list_is_a_reserved_type_name`), `bench/collection/lfold_rec.gx`, `bench/collection/lfold_rec_100k.gx`

## Motivation

A cons cell used to be `Value::Array([tag, head, tail])` — three slots,
one of them an interned `"Cons"` tag whose only job was discrimination,
paid again on every traversal step as a string compare; nil was
`Value::String("Nil")`, another string compare. The representation
predated the collection intrinsics and the JIT knowing List as a shape;
both special-cased it, so the tag bought nothing.

## Representation

The rep is PRIVATE to `graphix-types/src/list.rs` (`nil`/`cons`/`split`/
`is_nil`), the single seam that knows the layout:

- **cons** = `Value::Array([LMARK, head, tail])`, the list kind's
  marker in element 0 (`rep::build(Kind::List, ..)`,
  `design/distinct_composite_forms.md`), read back through
  `rep::marked(v, Kind::List)`.
- **nil** = `[LMARK]`, the marker alone: one static value, a refcount
  clone to construct.
- Discriminant: the marker, then the payload length 0 vs 2. O(1), no
  string compare.

**Why nil is not `Value::Null`:** `[List<'a>, null]` would collapse —
an empty list becomes indistinguishable from "absent", degenerating
Option-of-List, and lists are returned optionally all the time. Nor is
it the empty array: an empty list is no array, so `[]` and `[<>]`
differ.

Because the rep is private, users touch lists only through syntax and
the API, and the rep can change again (chunked cells, memoized length)
without breaking a program.

## The type is compiler-known, not `Abstract<>`

"Hidden from the user" is right; `Abstract<>` is the wrong spelling of
it — a Graphix abstract is a boxed value, one extra allocation per
construction, which would hand back per cell everything the slim rep
saves. So `List<'a>` is a primitive type constructor like `Array<'a>`:

- `Type::List(Arc<Type>)` beside `Type::Array` — type grammar, printer,
  contains/cast/is_a, `kernel_abi` (a List value is a 2-word
  `AbiKind::Value` carrier with a KNOWN interior shape). Covariant
  element, no primitive-bit relation.
- `List` is a RESERVED type name beside `Array`/`Map`
  (`expr/parser/mod.rs`): a user typedef of the name refuses at parse
  instead of dying later in typecheck against the native type.
- The list package keeps the function API (`list::fold`, `uncons`,
  `to_array_rev`, …) over the compiler rep; the Collection impl lives
  in core as intrinsic markers.

Variant TAGS are backtick-namespaced, not type names: `` `List `` is a
legal tag in expression, type and pattern position alike (the pattern
parser used to consult the type-name rule and refuse what the other two
positions accepted).

## Syntax

Literals (`ExprKind::List`; the JIT emits the tuple relay +
`graphix_valarray_into_list`):

```graphix
[<>]                    // nil
[<1, 2, 3>]             // cons(1, cons(2, cons(3, nil)))
```

Patterns are a FLAVOR of the array slice machinery (`list: bool` on
the AST `Slice`, `SliceKind` on the node):

```graphix
select l {
    [<>] => ...,                 // nil
    [<a, b>] => ...,             // exactly two
    [<h, rest..>] => ...         // head + TAIL — rest binds a List
}
```

- `[<` / `>]` are unambiguous in expression and pattern position: `<`
  is binary-only, so it cannot begin an expression after `[`; a bare
  `>` immediately before `]` is the literal closer, never the
  comparison. Tree-sitter spells the delimiters `'['`+immediate `'<'`
  and `'>'`+immediate `']'` — a 2-char token shadowed `[` in value
  strings.
- `rest..` binds the TAIL as a List — O(1), sharing structure. This is
  the recursion spelling that replaced `` `Cons(x, rest) ``.
- **The suffix form `[<init.., x>]` is refused.** On an array it is
  O(1) at both ends; on a linked list it is an O(n) walk plus a
  rebuild, and a silent cost cliff inside a pattern is a
  predictable-performance violation. Spell it (`list::reverse`,
  `to_array_rev`).
- Coverage is the array rule: unguarded all-bind arms whose lengths
  cover 0..∞ are exhaustive (`[<>]` + `[<h, t..>]` needs no wildcard);
  shadowed arms are dead-arm errors; every List member of the
  scrutinee has its own ladder, as every Array member does.

## Printing and the wire

`TVal` and the naked printer render the literal form (`[<1, 2>]`).
JSON, TOML and netidx strip a list to a flat array (`rep::strip` under
`abstract_value::to_data`), so `json::write_str([<1, 2>])` is `"[1,2]"`,
and a read rebuilds cells through its cast. Pack, db values and session
images preserve marked cells. Binary data validation walks these cells iteratively.

## Fusion

- List patterns fuse over a `Value`-kind scrutinee (`graphix_list_match`
  + kind-safe `graphix_list_get_*`/`graphix_list_tail`; the rest bind
  rides the Value tail rebind), so the `[<>]`/`[<h, t..>]` ladder is a
  native loop: `lfold_rec` beats the list intrinsic at 100k.
- Tail loops carrying a List use the Value-pair rebind; the flatten
  boundary (`graphix_list_to_valarray`/`graphix_valarray_into_list`)
  and the ListMap/ListFold scaffolds carry no tag writes or tests.
- `@`-binds on list arms fuse (`emit_list_pattern_cond` binds `all` as
  a Value clone), and so does every head pattern (a literal, an
  or-pattern, a variant, a tuple or struct, `[<0, rest..>]`: each head is
  a leaf, `emit_leaf_cond` at `LeafAt::ListHead`) and a list pattern at a
  tuple, struct, array or payload leaf (pins:
  `fusion_review::select_nested_leaves_fuse`, the `#[native]` list arms
  in `fusion_review`).

## Open

Whether `list::init/map/...` keep their package-side intrinsic markers
or fold further into the compiler — orthogonal to this design, revisit
with the intrinsics-deletion endgame.
