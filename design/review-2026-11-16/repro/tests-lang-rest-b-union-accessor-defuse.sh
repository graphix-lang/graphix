#!/bin/sh
# Title: a field read, tuple index, struct-with or let destructure over a union
#   of one-shape composites never fuses, while the select it is shorthand for does.
# Command: sh design/review-2026-11-16/repro/tests-lang-rest-b-union-accessor-defuse.sh
# Expected: every program builds (#[native] holds), as the select spelling does.
# Observed (HEAD e1be2fbf2, debug build):
#   1, 2: emit_clif: accessor source of type [{ a: i64, b: i64 }, { a: i64, c: i64 }] isn't Struct
#   3:    emit_clif: struct-with source isn't a struct
#   4:    emit_clif: slice/tuple/struct select pattern over [(i64, string), (string, i64)] not lowerable
#   5:    accessor source of type [{ act: `A, n: i64 }, { act: `B, n: i64 }] isn't Struct
#   6:    accessor source of type '_NNNN isn't Struct   (a select of a typedef and its expansion)
#   7:    builds (the select spelling of 1 fuses)
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
n=0
for body in \
 'let f = |s: [{a: i64, b: i64}, {a: i64, c: i64}]| #[native] (s.a + 1); f({a: 1, b: 2})' \
 'let f = |t: [(i64, i64), (i64, i64, i64)]| #[native] (t.1 + 1); f((1, 2))' \
 'let f = |s: [{a: i64, z: i64}, {z: i64}]| #[native] {s with z: 10}; f({z: 3})' \
 'let f = |v: [(i64, string), (string, i64)]| #[native] { let (x, y) = v; "[x] [y]" }; f((1, "s"))' \
 'let xs = [{act: `A, n: 1}, {act: `B, n: 2}]; #[native] array::map(xs, |x| x.n)' \
 'type Dom = {name: string}; let o: [i64, null] = null; let a: {d: Dom} = {d: {name: "x"}}; let b = {d: {name: "y"}}; let r = select o { null as _ => b, _ => a }; #[native] r.d.name' \
 'let f = |s: [{a: i64, b: i64}, {a: i64, c: i64}]| #[native] select s { {a: i64, b: i64} as {a, ..} => a + 1, {a: i64, c: i64} as {a, ..} => a + 1 }; f({a: 1, b: 2})'
do
  n=$((n+1)); echo "$body" > $D/p$n.gx
  echo "== $n: $body"
  timeout -s KILL 60 $G --no-cache --expand $D/p$n.gx 2>&1 | grep emit_clif || echo "   builds"
done
rm -rf $D
