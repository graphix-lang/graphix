#!/bin/sh
# Title: a nested lambda's own derived quantifiers (a trait-typed
# parameter's `#x`, a constructor quantifier's `'c#elem`) are aliased to
# the enclosing definition's same-named cells, so renaming a parameter or
# a quantifier changes whether a program checks.
# Command: sh c-lambda-nested-quantifier-name-capture.sh
# Expected: all four programs run; A and B print 1, C and D print 5.
# Observed: A refused ("fn(x: '#x: unbound within Display) -> .. does not
#   contain fn(x: '#x: string) -> '#x: string"), B prints 1; C refused
#   ("List<'_N: unbound> does not contain List<string>"), D prints 5.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
printf '%s\n' 'let f = |x: Display| { let g = |x: Display| x; g(1) };' 'f("s")' > $d/a.gx
printf '%s\n' 'let f = |x: Display| { let g = |y: Display| y; g(1) };' 'f("s")' > $d/b.gx
cat > $d/c.gx <<'GX'
use Collection::*;
let f = 'c: Collection |xs: 'c| -> i64 {
  let g = 'c: Collection |ys: 'c| -> i64 fold(ys, 0, |a, _| a + 1);
  g([<"a", "b">]) + fold(xs, 0, |a, _| a + 1)
};
f([1, 2, 3])
GX
sed "s/let g = 'c: Collection |ys: 'c|/let g = 'd: Collection |ys: 'd|/" $d/c.gx > $d/d.gx
for p in a b c d; do
  echo "== $p"
  timeout -s KILL 5 $G --no-cache $d/$p.gx 2>&1 | tail -1
done
rm -rf $d
