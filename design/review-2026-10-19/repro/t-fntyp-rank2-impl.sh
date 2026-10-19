#!/bin/sh
# t-fntyp-rank2-impl: an interface accepts a rank-2 implementation of a rank-1 parameter
# usage: sh t-fntyp-rank2-impl.sh [path/to/graphix]
# inner.gxi: val f: fn(g: fn(x: i64) -> i64) -> f64
# inner.gx:  let f = |g: fn<'b: Number>(x: 'b) -> 'b| -> f64 g(2.5)
# The implementation calls g at f64; a caller through the interface passes fn(x: i64) -> i64.
# expected: the interface check refuses the implementation (its 'b is a rank-2 quantifier the
#   caller never picks; it cannot stand for the signature's i64).
# observed: --check exit 0 on both programs; the static call's build refuses
#   "an instance at fn(g: fn(x: i64) -> i64) -> f64 of a definition typed fn(g: fn<'b: Number>..",
#   and the dynamic call fails at run time: "binding the callee failed: a run-time bind ... did not type".
G=${1:-graphix}
D=$(mktemp -d)
printf "val f: fn(g: fn(x: i64) -> i64) -> f64;\n" > $D/inner.gxi
printf "let f = |g: fn<'b: Number>(x: 'b) -> 'b| -> f64 g(2.5);\n" > $D/inner.gx
printf 'mod inner;\ninner::f(|x: i64| x * 2)\n' > $D/static.gx
printf 'mod inner;\nlet fs = [inner::f];\nlet h = fs[0]$;\nh(|x: i64| x * 2)\n' > $D/dynamic.gx
for p in static dynamic; do
  echo "== $p --check"; timeout -s KILL 60 $G --check $D/$p.gx; echo "exit $?"
  echo "== $p run"; timeout -s KILL 10 $G --no-cache $D/$p.gx; echo "exit $?"
done
rm -rf $D
