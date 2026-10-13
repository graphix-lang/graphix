#!/bin/sh
# t-tvar-02: Singleton narrowing gives a declared (rigid) variable a
# concrete conjunct, changing its scheme silently.
# (A .sh because t-tvar-02.gx already holds a different reviewer sketch.)
#
# Command: sh design/review-2026-10-12/repro/t-tvar-02.sh [path/to/graphix]
#
# Expected: the definition of f is refused in its body: ['r, i64] is one
# type only where 'r is i64, and 'r is rigid (as the control,
# h(x, 1) with h = 'a: Number |x: 'a, y: 'a| x, is refused).
# Observed (HEAD 7324207f):
#   1. --check of f(3, false) passes, but the build refuses it:
#      "an instance at fn(x: [i64, i64]) -> [i64, i64] of a definition
#       typed fn<'a: Singleton>(x: 'a) -> 'a"
#   2. f(2.5, false), which fits the declared 'r: Number, is refused at
#      the caller: "type mismatch 'r: unbound within Number & i64 does
#      not contain f64"
#   3. the control is refused in the body, as expected.
G=${1:-graphix}
D=$(mktemp -d)
cat > $D/a.gx <<'GX'
let g = 'a: Singleton |x: 'a| -> 'a x;
let f = 'r: Number |x: 'r, b: bool| g(select b { true => x, false => 1 });
sys::exit(sys::time::after_idle(duration:100.ms, 0));
f(3, false)
GX
cat > $D/b.gx <<'GX'
let g = 'a: Singleton |x: 'a| -> 'a x;
let f = 'r: Number |x: 'r, b: bool| g(select b { true => x, false => 1 });
f(2.5, false)
GX
cat > $D/control.gx <<'GX'
let h = 'a: Number |x: 'a, y: 'a| x;
let f = 'r: Number |x: 'r| h(x, 1);
f
GX
echo "== 1. check of f(3, false)"; timeout -s KILL 30 $G --no-cache --check $D/a.gx; echo "exit $?"
echo "== 1. build of f(3, false)"; timeout -s KILL 30 $G --no-cache $D/a.gx; echo "exit $?"
echo "== 2. check of f(2.5, false)"; timeout -s KILL 30 $G --no-cache --check $D/b.gx; echo "exit $?"
echo "== 3. control"; timeout -s KILL 30 $G --no-cache --check $D/control.gx; echo "exit $?"
rm -rf $D
