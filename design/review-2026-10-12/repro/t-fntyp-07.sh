#!/bin/sh
# t-fntyp-07: sig_matches checks a declared bound on one matched impl cell, chosen by name order
# Usage: sh t-fntyp-07.sh [path-to-graphix]
# Two implementations of `val f: fn<'a: Number>(x: 'a, y: 'a) -> 'a` that differ only
# in what their type variables are called.
# Expected: both get the same verdict (the names of the impl's variables decide nothing).
# Observed: a (x: 'b, y: 'c, 'c: Number) is refused "missing constraint 'a: Number in
#           implementation", exit 1; b (x: 'c, y: 'b, 'b: Number) is accepted, exit 0.
G=${1:-graphix}
D=$(mktemp -d)
for v in a b; do
  mkdir -p $D/$v
  printf 'mod inner;\ninner::f(1, 2)\n' > $D/$v/main.gx
  printf "val f: fn<'a: Number>(x: 'a, y: 'a) -> 'a\n" > $D/$v/inner.gxi
done
printf "let f = 'c: Number |x: 'b, y: 'c| -> 'c y\n" > $D/a/inner.gx
printf "let f = 'b: Number |x: 'c, y: 'b| -> 'b y\n" > $D/b/inner.gx
for v in a b; do
  echo "== $v"; timeout -s KILL 60 $G --check $D/$v/main.gx; echo "exit $?"
done
rm -rf $D
