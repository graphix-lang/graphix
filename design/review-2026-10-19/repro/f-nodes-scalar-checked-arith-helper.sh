#!/bin/sh
# Title: checked arithmetic on register scalars always calls Value helpers (~8x slower in a fused fold)
# Command: sh design/review-2026-10-19/repro/f-nodes-scalar-checked-arith-helper.sh
# Expected: a fused fold with `+?`/`*?` costs about what the unchecked fold does (an overflow
#   test per op), both being #[native].
# Observed (debug binary, 3M elements, two runs each):
#   unchecked  13499995500000 11345312.ns / 11565288.ns
#   checked    13499995500000 90789588.ns / 96052465.ns
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
cat > $D/u.gx <<'GX'
let xs = array::init(3000000, |i| i);
let f = |xs: Array<i64>| #[native] array::fold(xs, 0, |acc, x| acc + x * 3);
let t0 = sys::time::now(xs);
let r = f(xs);
let t1 = sys::time::now(r);
println("[r] [sys::time::diff(t1, t0)]");
sys::exit(sys::time::after_idle(duration:50.ms, r ~ 0))
GX
sed 's/acc + x \* 3/(acc +? (x *? 3)$)$/' $D/u.gx > $D/c.gx
echo unchecked; timeout -s KILL 120 $G --no-cache $D/u.gx
echo checked; timeout -s KILL 120 $G --no-cache $D/c.gx
rm -rf $D
