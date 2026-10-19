#!/bin/sh
# Widening is order-dependent for an argument whose type is an open cell.
# CLAUDE.md: a call's data-only type variable "settles to the widest
# argument whatever the order". The site's cell does, but an argument whose
# own type is still open (an unannotated parameter, a `let n = never()`) is
# fixed to whatever narrower type the cell held when that argument was
# checked, and is not widened with it.
# Command: sh c-callsite-widening-order-open-arg.sh
# Expected: both orders print [null, null, 1] / [1, null, null] (z: [i64, null]).
# Observed: order "i64:1, z, v" is refused at h(null):
#   "type mismatch i64 does not contain null"; order "v, z, i64:1" runs.
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
for order in "i64:1, z, v" "v, z, i64:1"; do
cat > $D/w.gx <<EOT
let pick = |x: 'a, y: 'a, z: 'a| -> Array<'a> [x, y, z];
let v: [i64, null] = null;
let h = |z| pick($order);
println("[h(null)]");
sys::exit(sys::time::after_idle(duration:100.ms, 0))
EOT
echo "== pick($order)"
timeout -s KILL 30 $G --no-cache $D/w.gx 2>&1 | tail -1
done
rm -rf $D
