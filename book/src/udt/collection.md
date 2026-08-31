# Implementing Collection

The built-in collections implement [`Collection`](./traits.md#constructor-traits-collection)
through compiler intrinsics, and those aren't going anywhere — they
are the fastest path for `Array` and `Map`. But the trait needs only
three methods, and all three are ordinary Graphix: any structure you
can fold, you can give the whole collection API. This chapter is the
reference for what those implementations look like written out, and
what they cost.

## The operations as loops

Over an array-backed structure, the operations are index tail
recursions — one activation reused per iteration, compiled to a
native loop:

```graphix
{{#include ../examples/collection/native_array_ops.gx}}
```

`fold` is the primitive, and the hand-written loop runs within ~6x of
the intrinsic per element. `find` as a loop *stops at the match* — an
early exit the fold-derived default cannot express, which is why it
beats the intrinsic (an all-slots scan by rule) on an average hit.
`map` by construction over the length is at parity with the
intrinsic.

## flat_map without the quadratic

The tempting derivation — fold with `array::concat` — copies the
accumulator on every step and is O(n²) by construction. Cons onto a
`List` instead and finish with one reverse walk:

```graphix
{{#include ../examples/collection/native_flat_map.gx}}
```

This is linear, and the nested fold fuses into one kernel with the
outer loop — about 3.5x the intrinsic, all of it the per-element
`Cons` construction.

## A full implementation for your own type

A program-defined linear (or tree) structure implements the three
required methods as natural recursions over its constructors and
inherits `map`, `filter`, `find`, `find_map` and `len` from the
defaults:

```graphix
{{#include ../examples/collection/cons_list.gx}}
```

The tail-recursive `fold_l` compiles to a native loop over the
conses; the value-building recursions (`filter_map_l`, `append_l`)
are an activation per element — see
[Recursion](../functions/recursion.md). This shape is not a toy: the
same spelling over the built-in `List` — written with list patterns,
`[<>]` and `[<x, rest..>]` — runs *faster* than the list fold
intrinsic at 100k elements (the intrinsic pays a
convert-to-array boundary; the recursion just walks the conses).

The measured comparisons behind the numbers above live in
`bench/collection/README.md` in the Graphix repository, one row per
operation and derivation, re-run as the compiler moves.
