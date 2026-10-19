#!/bin/sh
# t-fntyp-nested-settle: a fn-typed parameter settles the signature's deferred unions early
# usage: sh t-fntyp-nested-settle.sh [path/to/graphix]
# Three interfaces that differ only in where an unrelated fn-typed parameter `g` sits.
# The implementation orders its union of variables its own way (['c, 'd] with y: 'd, z: 'c),
# which the deferred union pairing (SigPairs::settle) is meant to accept.
# expected: a, b and c all check (exit 0).
# observed: a exit 0, c exit 0, b exit 1: "in argument 2: type variable usage mismatch:
#   expected 'b: unbound, got 'a: unbound" -- the nested FnType::sig_matches_int for `g`
#   ran tvar_map.settle() before y and z had paired, fixing the union's pairing by stored order.
G=${1:-graphix}
D=$(mktemp -d)
for v in a b c; do mkdir -p $D/$v; printf 'mod inner;\n0\n' > $D/$v/main.gx; done
printf "val f: fn(x: ['a, 'b], y: 'a, z: 'b) -> i64;\n" > $D/a/inner.gxi
printf "let f = |x: ['c, 'd], y: 'd, z: 'c| -> i64 0;\n" > $D/a/inner.gx
printf "val f: fn(x: ['a, 'b], g: fn(n: i64) -> i64, y: 'a, z: 'b) -> i64;\n" > $D/b/inner.gxi
printf "let f = |x: ['c, 'd], g: fn(n: i64) -> i64, y: 'd, z: 'c| -> i64 0;\n" > $D/b/inner.gx
printf "val f: fn(x: ['a, 'b], y: 'a, z: 'b, g: fn(n: i64) -> i64) -> i64;\n" > $D/c/inner.gxi
printf "let f = |x: ['c, 'd], y: 'd, z: 'c, g: fn(n: i64) -> i64| -> i64 0;\n" > $D/c/inner.gx
for v in a b c; do echo "== $v"; timeout -s KILL 60 $G --check $D/$v/main.gx; echo "exit $?"; done
rm -rf $D
