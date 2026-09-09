# Native List: slim representation, literals, patterns

Status: built 2026-08-31
Pins: `stdlib/graphix-tests/src/lang/lists.rs`, `graphix-compiler/src/expr/parser/test.rs` (`list_is_a_reserved_type_name`), `bench/collection/lfold_rec.gx`, `bench/collection/lfold_rec_100k.gx`

## Motivation

A cons cell used to be `Value::Array([tag, head, tail])` — three slots,
one of them an interned `"Cons"` tag whose only job was discrimination,
paid again on every traversal step as a string compare; nil was
`Value::String("Nil")`, another string compare. The representation
predated the collection intrinsics and the JIT knowing List as a shape;
both special-cased it, so the tag bought nothing.

## Representation

The rep is PRIVATE to `node/collection.rs::list` (`nil`/`cons`/`split`/
`is_nil`), the single seam that knows the layout:

- **cons** = `Value::Array([head, tail])` — two slots, 32 bytes.
- **nil** = a refcount clone of the static EMPTY `ValArray` — free to
  construct, free to test.
- Discriminant: array length 0 vs 2. O(1), no string compare; in
  kernels an inline length test.

**Why nil is not `Value::Null`:** `[List<'a>, null]` would collapse —
an empty list becomes indistinguishable from "absent", degenerating
Option-of-List, and lists are returned optionally all the time. The
empty array costs the same and keeps the union honest.

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
  shadowed arms are dead-arm errors; `array_members` collects List
  members too.

## Printing and the wire

`TVal` renders the literal form (`[<1, 2>]`). The wire and the REPL's
structural echo carry the raw rep (nested 2-arrays, empty array);
json/toml/pack serializations of list values have the same shape.

## Fusion

- List patterns fuse over a `Value`-kind scrutinee (`graphix_list_match`
  + kind-safe `graphix_list_get_*`/`graphix_list_tail`; the rest bind
  rides the Value tail rebind), so the `[<>]`/`[<h, t..>]` ladder is a
  native loop: `lfold_rec` beats the list intrinsic at 100k.
- Tail loops carrying a List use the Value-pair rebind; the flatten
  boundary (`graphix_list_to_valarray`/`graphix_valarray_into_list`)
  and the ListMap/ListFold scaffolds carry no tag writes or tests.
- Nested element patterns and `@`-binds on list arms de-fuse
  (coverage residue).

## Open

Whether `list::init/map/...` keep their package-side intrinsic markers
or fold further into the compiler — orthogonal to this design, revisit
with the intrinsics-deletion endgame.
