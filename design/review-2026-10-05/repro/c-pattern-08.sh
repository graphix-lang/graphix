#!/usr/bin/env bash
# c-pattern-08: slice elements compile against the union of all element
# patterns, so `[x, _] => x + 1` is refused.
#
# compile_slice (graphix-compiler/src/node/pattern.rs:419-422) compiles every
# element pattern against the slice's one element type. Under an inferred
# predicate that type is infer_slice's union of every element's inference
# (graphix-types/src/expr/pattern.rs:201-216): a `_` element (Any) makes it
# Any, so every bind beside it is typed Any and a structure beside it is
# refused; elements of different shapes make it a union, and Tuple, Variant
# and Struct compilation (pattern.rs:562, 587, 641-643) refuse anything that
# is not exactly their constructor. That exact-constructor
# rule also refuses `Array<[`A, `B]> as [`A, `B]` (explicit predicate) and a
# variant payload pattern on an abstract whose representation is a union.
# Controls: `[x, y]`, `[x, ..]`, `(x, _)` and `Array<i64> as [x, _]` pass.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/c-pattern-08.sh
#
# expected: every case passes --check (exit 0); run, each would print the
#   value in brackets: ignore [5], list_ignore [5], tuple_ignore [7],
#   two_tags [1], two_tags_explicit [1], tag_and_bind [`B],
#   two_tuples_mixed [11], struct_and_bind [5], abstract_union_rep [3].
#
# observed (HEAD c722befe, debug build): the controls exit 0; every other
#   case exits 1:
#   ignore, list_ignore:  cannot compute Any + i64: arithmetic is ...
#   tuple_ignore:         tuple patterns can't match Any
#   two_tags, two_tags_explicit:  variant patterns can't match [`A, `B]
#   tag_and_bind:         variant patterns can't match ['_..: unbound, `A]
#   two_tuples_mixed:     tuple patterns can't match [(i64, '_..: unbound),
#                         ('_..: unbound, i64)]
#   struct_and_bind:      non exhaustive struct matches require type annotations
#   abstract_union_rep:   variant patterns can't match [`A(i64), `B(i64)]
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
declare -A progs=(
    [control_named]='let f = |a: Array<i64>| select a { [x, y] => x + 1, _ => 0 }; f([4, 5])'
    [control_explicit]='let f = |a: Array<i64>| select a { Array<i64> as [x, _] => x + 1, _ => 0 }; f([4, 5])'
    [ignore]='let f = |a: Array<i64>| select a { [x, _] => x + 1, _ => 0 }; f([4, 5])'
    [list_ignore]='let f = |a: List<i64>| select a { [<x, _, t..>] => x + 1, _ => 0 }; f([<4, 5>])'
    [tuple_ignore]='let f = |a: Array<(i64, i64)>| select a { [_, (x, y)] => x + y, _ => 0 }; f([(1, 2), (3, 4)])'
    [two_tags]='let f = |a: Array<[`A, `B]>| select a { [`A, `B] => 1, _ => 0 }; f([`A, `B])'
    [two_tags_explicit]='let f = |a: Array<[`A, `B]>| select a { Array<[`A, `B]> as [`A, `B] => 1, _ => 0 }; f([`A, `B])'
    [tag_and_bind]='let f = |a: Array<[`A, `B]>| select a { [`A, x] => x, _ => `A }; f([`A, `B])'
    [two_tuples_mixed]='let f = |a: Array<(i64, i64)>| select a { [(1, x), (y, 2)] => x + y, _ => 0 }; f([(1, 5), (6, 2)])'
    [struct_and_bind]='type S = {a: i64, b: i64}; let f = |a: Array<S>| select a { [{b, ..}, x] => b + x.a, _ => 0 }; f([{a: 1, b: 2}, {a: 3, b: 4}])'
    [abstract_union_rep]='type Box = Abstract<[`A(i64), `B(i64)]>; let f = |b: Box| select b { Box(`A(x)) => x, Box(`B(y)) => y * 10 }; f(Box(`A(3)))'
)
for name in control_named control_explicit ignore list_ignore tuple_ignore two_tags \
    two_tags_explicit tag_and_bind two_tuples_mixed struct_and_bind abstract_union_rep; do
    printf '%s\n' "${progs[$name]}" > "$dir/$name.gx"
    timeout -s KILL 60 "$GRAPHIX" --check "$dir/$name.gx" > "$dir/out" 2>&1
    echo "$name: exit $?"
    grep -E "can't match|cannot compute|require type annotations" "$dir/out" \
        | sed 's/^ *[0-9]*: /    /'
done
