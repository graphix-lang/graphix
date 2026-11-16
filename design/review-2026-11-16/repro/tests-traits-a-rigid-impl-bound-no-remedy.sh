#!/bin/sh
# Title: a declared variable whose bounds lack an impl head's bound is refused
#   "Array<'a> does not implement Show", naming neither the unmet bound nor the
#   annotation, though an impl for Array<'b> exists; a function call's bound in
#   the same position names the fix.
# Command: sh design/review-2026-11-16/repro/tests-traits-a-rigid-impl-bound-no-remedy.sh
# Expected: (1) says the impl's bound 'b: Show is not met by 'a and to write
#   'a: Number + Show, as (2) does for a function's bound.
# Observed: (1) "Array<'a> does not implement Show"; (2) "the body needs 'a within
#   Number, which the signature does not declare: write 'a: Ordered + Number";
#   (3) the remedy (1) should have named passes.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
cat > $d/1-impl-bound.gx <<'GX'
trait Show { val show: fn(self) -> string };
impl Show for i64 { let show = |x| "i" };
impl<'b: Show> Show for Array<'b> { let show = |xs| "arr" };
let h = 'a: Number |x: Array<'a>| -> string Show::show(x);
h([1])
GX
cat > $d/2-function-bound.gx <<'GX'
let g = 'b: Number |x: 'b| x;
let h = 'a: Ordered |x: 'a| g(x);
h(1)
GX
sed "s/'a: Number |/'a: Number + Show |/" $d/1-impl-bound.gx > $d/3-annotated.gx
for f in 1-impl-bound 2-function-bound 3-annotated; do
  echo "== $f"
  out=$(timeout -s KILL 30 $G --no-cache --check $d/$f.gx 2>&1); rc=$?
  echo "$out" | tail -1; echo "exit $rc"
done
rm -rf $d
