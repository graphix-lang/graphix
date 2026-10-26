#!/bin/sh
# Title: a lambda's hidden quantifiers (a trait-typed parameter's `#s`, a
# constructor-trait quantifier's `'c#elem`) are aliased to an enclosing
# definition's same-named ones by Lambda::compile's enclosing_tvars step.
# Command: sh c-lambda-nested-quantifier-alias.sh /path/to/graphix
# Case 1 expected "int 1"; observed: --check exits 0, the run refuses with
#   type mismatch fn(s: '#s: unbound within Show) -> '_N: unbound does not
#   contain fn(s: '#s: i64) -> string   (at `|s: Show| Show::show(s)`)
#   (renaming the outer parameter to `t`, or GRAPHIX_NO_SUBST=1, prints it)
# Case 2 expected 5; observed: the check refuses
#   type mismatch List<'_N: unbound> does not contain List<string>
#   (renaming the outer quantifier to 'd prints 5)
B=${1:-graphix}
D=$(mktemp -d)
cat > $D/case1.gx <<'GX'
trait Show { val show: fn(self) -> string };
impl Show for i64 { let show = |x| "int [x]" };
let f = |s: Show| {
  let g = |s: Show| Show::show(s);
  g(s)
};
f(1)
GX
cat > $D/case2.gx <<'GX'
use Collection::*;
let f = 'c: Collection |y: 'c| {
  let g = 'c: Collection |x: 'c| len(x);
  g([<"a", "b">]) + len(y)
};
f([1, 2, 3])
GX
echo "== case1 --check"; timeout -s KILL 20 $B --check $D/case1.gx; echo "exit $?"
echo "== case1 run"; timeout -s KILL 20 $B --no-cache $D/case1.gx 2>&1 | tail -1
echo "== case2 run"; timeout -s KILL 20 $B --no-cache $D/case2.gx 2>&1 | tail -1
rm -rf $D
