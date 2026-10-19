#!/bin/sh
# Element-position pattern leaves (tuple, slice, struct field) lower only a
# bind or an exact-scalar literal: a string literal, a literal over a
# nullable/union element and an or-pattern all de-fuse, though each fuses as a
# top-level arm.
# run: sh tests-select-element-leaf-defuses.sh
# expected: no output (every `#[native]` builds; the top-level twins do)
# observed:
#   emit_clif: non-scalar literal pattern leaf String("a")         (tuple)
#   emit_clif: non-scalar literal pattern leaf String("a")         (slice)
#   emit_clif: literal pattern leaf prim I64 doesn't match the leaf's static type ... I64 | Null
#   emit_clif: or-pattern leaf not lowerable                        (tuple)
#   emit_clif: or-pattern leaf not lowerable                        (slice)
#   emit_clif: nested variant payload pattern not lowerable         (`A(1 | 2))
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
for prog in \
  'let f = |a: (string, i64)| -> i64 #[native] select a { ("a", n) => n, _ => 0 }; f(("a", 2))' \
  'let f = |a: Array<string>| -> i64 #[native] select a { ["a", ..] => 1, _ => 0 }; f(["a"])' \
  'let f = |a: Array<[i64, null]>| -> i64 #[native] select a { [1, ..] => 5, _ => 0 }; f([1])' \
  'let f = |v: (bool, [`A, `B])| -> i64 #[native] select v { (true, `A | `B) => 1, (false, _) => 2 }; f((true, `B))' \
  'let f = |a: Array<i64>| #[native] select a { [x, 1 | 2] => x, _ => 0 }; f([3, 2])' \
  'let f = |v: [`A(i64), `B]| #[native] select v { `A(1 | 2) => "y", _ => "n" }; f(`A(2))' \
  'let f = |a: string| -> i64 #[native] select a { "a" => 1, _ => 0 }; f("a")' \
  'let f = |x: [i64, null]| -> i64 #[native] select x { 1 => 5, _ => 0 }; f(1)' \
  'let f = |v: [`A, `B]| -> i64 #[native] select v { `A | `B => 1 }; f(`B)'
do
  printf '%s\n' "$prog" > "$D/p.gx"
  timeout -s KILL 30 "$G" --no-cache --expand "$D/p.gx" 2>&1 | grep 'emit_clif'
done
rm -rf "$D"
