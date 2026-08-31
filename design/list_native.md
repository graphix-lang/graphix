# Native List: slim representation, literals, patterns

**Status: phases A (rep + `Type::List`), B (literals + patterns +
tree-sitter) and B3 (fused pattern lowering — the ladder beats the
intrinsic at 100k) LANDED 2026-08-31. Open: the overnight roundtrip proptest before the
arc is called done. The book chapter LANDED 2026-08-31 (Fundamental
Types gets a List section, Select gets List Patterns, stdlib/list
re-synced from the gxi, the recursive-types cons-list example renamed
to `Lst`). Name reservation ENFORCED 2026-08-31 (Eric's call): `List`
joined RESERVED beside `Array`/`Map` — a user typedef of the name
refuses at parse instead of dying later in typecheck with a mismatch
against the native type; tui's widget state type renamed `List` ->
`ListBox` for it (its `` `List `` TAG and the Rust compile keyed on
the tag are unchanged). Fallout fix: variant TAGS are
backtick-namespaced, not type names — the pattern-position tag parser
consulted `typname()` and refused reserved words the expression and
type positions accept (`` `Array `` was constructible but not
matchable); it now uses `ident(true)` like the other two, pinned in
`list_is_a_reserved_type_name`. The fuzz generator EMITS list syntax since 2026-08-31
(`GenType::List`: literals, from_array/cons/map/filter producers, the
pattern-ladder select mode, list accessors; gen-check ~99.7% across
seeds — the residuals are the pre-existing nullable-literal and
free-union classes; detcheck 0 flaps). Implements the 2026-07-17 ruling "List
moves into the compiler like Array — core data type."**

> Phase-A reality check: the old typedef was TRANSPARENT (bench and
> fixtures matched `` `Cons `` directly), so the planned rep-only P1
> was impossible — the rep swap and `Type::List` landed together, and
> structural consumers migrated to the API (`list::uncons` selects)
> until phase B's patterns arrive. A user typedef NAMED `List` is now
> broken like one named `Array` (two fixtures renamed theirs to `L`;
> tui's `list::List` widget-state type survives because its uses are
> qualified).

## Motivation

A cons cell today is `Value::Array([tag, head, tail])` — three slots
(48 bytes of payload), one of them an interned `"Cons"` tag whose only
job is discrimination, paid again on every traversal step as a string
compare (`node/collection.rs::list::split`). Nil is
`Value::String("Nil")` — another string compare. The representation
predates the collection intrinsics and the JIT knowing List as a
shape; both already special-case it, so the tag buys nothing.

## Representation (ruled)

- **cons** = `Value::Array([head, tail])` — two slots, 32 bytes.
- **nil** = a refcount clone of the **static empty `ValArray`**
  (`Value::Array(len 0)`) — free to construct, free to test.
- Discriminant: array length 0 vs 2. O(1), no string compare; in
  kernels an inline len test instead of the `graphix_variant_tag_eq`
  helper call.

**Why nil is not `Value::Null`** (the other candidate): `[List<'a>,
null]` would collapse — an empty list becomes indistinguishable from
"absent", degenerating Option-of-List (the Option<Option> collapse,
but lists are returned optionally all the time). The empty array costs
the same and keeps the union honest.

Structurally the rep is `[('a, List<'a>), <empty>]` — but that
spelling is DOCUMENTATION of the private layout, not the public type.

## The type is compiler-known, not `Abstract<>`

"Hidden from the user" is right; the `Abstract<>` mechanism is the
wrong spelling of it — a Graphix abstract is a vtable'd box, one extra
allocation per construction, which would hand back per-cell everything
the slim rep saves. Instead `List<'a>` becomes a **primitive type
constructor in the compiler**, like `Array<'a>`:

- `Type::List(Arc<Type>)` beside `Type::Array` (type grammar, printer,
  contains/cast/is_a, kernel_abi — a List value is a 2-word
  `AbiKind::Value`-class carrier with a KNOWN interior shape).
- The name lands in the core prelude like `Array`; the **list package
  keeps the function API** (`list::fold`, `to_array_rev`, …) over the
  compiler rep (`node/collection.rs::list` stays the single rep seam —
  it is already the only place that knows the layout).
- Users touch lists only through syntax and API; the rep can change
  again later (chunked cells, memoized length) without breaking a
  single program.

## Syntax (ruled)

Literals, expression position:

```graphix
[<>]                    // nil
[<1, 2, 3>]             // cons(1, cons(2, cons(3, nil)))
```

Patterns, mirroring the array slice grammar:

```graphix
select l {
    [<>] => ...,                 // nil
    [<a, b>] => ...,             // exactly two
    [<h, rest..>] => ...         // head + TAIL — rest binds a List
}
```

- `[<` / `>]` are unambiguous in expression and pattern position (`<`
  is binary-only, so it cannot begin an expression after `[`; `>]`
  cannot occur there today).
- `rest..` binds the **tail as a List — O(1), sharing structure**.
  This is the recursion spelling that replaces `` `Cons(x, rest) ``.
  (On arrays the rest bind is also O(1) — a `ValArray` subslice view —
  so the two grammars agree on cost for the prefix form.)
- **The suffix form (`[<init.., x>]`) is refused.** On an array it is
  O(1) both ends; on a linked list it is an O(n) walk plus a rebuild —
  a silent cost cliff inside a pattern is a predictable-performance
  violation. Spell it explicitly (`list::reverse`, `to_array_rev`) if
  needed.
- Coverage: length ladders, exactly the array rule — unguarded
  all-bind arms whose lengths cover 0..∞ are exhaustive (`[<>]` +
  `[<h, t..>]` needs no wildcard); shadowed arms are dead-arm errors.

## Printing and the wire

- `TVal` renders the literal form (`[<1, 2>]`) — the 2026-07-17 ruling
  already flagged today's naked `["Cons", 1, …]` print as a gap.
- The wire and the REPL's structural echo carry the raw rep (nested
  2-arrays, empty array) — a 0.9-line wire break for list values,
  acceptable pre-1.0; json/toml/pack serializations of list values
  change shape the same way.

## Fusion

Everything gets cheaper or stays put:

- select over a List: len-test discriminant (inline) + head bind
  (element type) + tail bind (List, a 2-word value — the non-scalar
  payload machinery, 2026-08-30).
- tail loops carrying a List: the Value-pair rebind (2026-08-30).
- the flatten boundary (`graphix_list_to_valarray` / `into_list`) and
  the ListMap/ListFold scaffolds simplify (no tag writes/tests).
- Expect `lfold_rec` / `flatmap_cons`-class improvements; re-run
  `bench/collection` and update the README table after each phase.

## Blast radius

- Parser + printers + **the round-trip proptest, run overnight** (the
  standing rule for syntax changes).
- The fuzz **generator and typemorph must learn the syntax** in the
  same arc, or the corpus never exercises it.
- Pattern machinery: a list-slice `StructPatternNode` variant through
  matching, coverage, dead-arm, refs, and both engines' select
  lowerings.
- Stdlib: the list package rewires to the new rep seam (API
  unchanged); `collection_bodies_*`, bench, book examples update.
- User-visible break: structural matches on `` `Cons``/`` `Nil `` stop
  compiling — the new patterns are the migration.

## Sequencing

After aug30h's round-day (gates are not the fuzzer):

1. **P1 — rep swap only.** `nil`/`cons`/`split`/`is_nil` in the one
   seam + the JIT discriminant/flatten helpers. No syntax, no type
   change; everything reaches lists through the API, so the suite and
   fuzzer exercise it as-is. Bench, soak.
2. **P2 — `Type::List` + literals + patterns + TVal printing.**
   Parser/printer/proptest overnight; both select lowerings; fixtures
   (`FuseExpect` annotated both ways).
3. **P3 — generator + typemorph + book** (chapter updates: variants
   example in udt/collection.md switches to the native patterns where
   it uses `list::List`; the user cons-list example stays as-is — it
   is about user types).

Each phase gates on bare `cargo test`; the syntax phase adds the
overnight proptest; redeploy the fleet on each landing.

## Open (minor)

- Whether `list::init/map/...` keep their package-side intrinsic
  markers or fold further into the compiler — orthogonal to this arc,
  revisit with the intrinsics-deletion endgame.
