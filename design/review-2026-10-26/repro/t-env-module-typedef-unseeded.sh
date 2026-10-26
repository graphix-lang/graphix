#!/bin/sh
# Title: a recursive typedef inside a module with an interface is never seeded,
#        so an array::map instance over it fails to elaborate ("undefined type T")
# Command: sh design/review-2026-10-26/repro/t-env-module-typedef-unseeded.sh
# Expected: --check passes and the run prints [7] (as it does with m.gxi removed,
#           or with the same three lines at the top level of a script).
# Observed: --check exits 0, the run fails:
#   in the instance of array::map([a], |x| 7) at this call site ...
#   undefined type T in m at line: 1, column: 23 in file .../m.gx
# Same failure when `type T` is declared in m.gxi instead of m.gx.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > $D/main.gx <<'GX'
mod m;
sys::exit(sys::time::after_idle(duration:100.ms, 0));
m::r
GX
echo 'val r: Array<i64>' > $D/m.gxi
cat > $D/m.gx <<'GX'
type T = Array<[null, T]>;
let a: T = [null];
let r = array::map([a], |x| 7)
GX
timeout -s KILL 60 $G --check $D/main.gx; echo "check exit $?"
timeout -s KILL 60 $G --no-cache $D/main.gx 2>&1 | tail -2
rm -f $D/m.gxi
echo "without m.gxi:"
timeout -s KILL 60 $G --no-cache $D/main.gx 2>&1 | tail -1
rm -rf $D
