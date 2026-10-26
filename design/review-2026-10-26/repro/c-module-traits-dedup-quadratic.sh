#!/bin/sh
# array::dedup over abstract values whose type implements Eq is quadratic in hook dispatches
# GxAbstract::hash hashes only the id once dispatch_eq_applies says an Eq impl exists,
# so every element of one abstract type lands in one bucket and the AHashSet in
# fc_dedup compares each new element against all kept ones through the Eq hook.
# Run: sh c-module-traits-dedup-quadratic.sh   (debug binary)
# Observed: 2000 elements 1.8 s, 4000 elements 7.0 s (x4 per x2: quadratic);
#           without the impl 0.1 s; array::sort of 4000 under an Ord impl 0.13 s.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
for n in 2000 4000; do
cat > $D/dd$n.gx <<GX
type Mod = Abstract<i64>;
impl Eq for Mod { let eq = |a, b| a.0 == b.0 };
let xs = array::init($n, |i| Mod(i));
let d = array::dedup(xs);
sys::exit(sys::time::after_idle(duration:10.ms, 0));
array::len(d)
GX
s=$(date +%s%N); timeout -s KILL 120 $G --no-cache $D/dd$n.gx; echo "n=$n $(( ($(date +%s%N) - s) / 1000000 )) ms"
done
rm -rf $D
