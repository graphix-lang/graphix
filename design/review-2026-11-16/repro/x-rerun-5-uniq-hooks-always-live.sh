#!/bin/bash
# Title: x-rerun-4-05 regressed: uniq over an unchanged array is O(n) per fire again, because
#   core's own `impl<'a> Cast for Transparent<'a> {}` makes coretraits::hooks_live true in every
#   program, so Uniq::update never reaches structural_eq's same-allocation shortcut and compares
#   with Value's PartialEq (a full walk).
# Command: bash design/review-2026-11-16/repro/x-rerun-5-uniq-hooks-always-live.sh [graphix]
# Expected: per-cycle cost independent of n, as `once` over the same array is, and as the
#   2026-10-26 review measured uniq over an array (n=1000 4.3 ms, n=100000 4.7 ms).
# Observed (debug review binary, 1000 cycles, --no-fusion, GRAPHIX_PAR=off):
#   uniq n=1000 7.4 ms   uniq n=100000 362 ms   once n=100000 3.5 ms
#   (a map, or a tuple around the array, is linear the same way: 1059 ms / 357 ms at n=100000)
G=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
for e in "uniq(i ~ big)" "once(i ~ big)"; do for n in 1000 100000; do
cat > $d/p.gx <<GX
let k = 1010;
let big = array::init($n, |i| i);
let i = 0;
i <- select i { x if x < k => x + 1, _ => never() };
let u = count($e);
let t0 = sys::time::now(select i { 10 => 0, _ => never() });
let done = select i { x if x == k => x, _ => never() };
let t1 = sys::time::now(done);
let el = cast<f64>(sys::time::diff(t1, t0))\$ * 1000.0;
println("$e n=$n ms=[el] u=[u]");
sys::exit(el ~ 0)
GX
GRAPHIX_PAR=off timeout -s KILL 60 $G --no-cache --no-fusion $d/p.gx 2>&1 | grep 'n='
done; done
rm -rf $d
