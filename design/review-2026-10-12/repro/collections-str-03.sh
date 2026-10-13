#!/bin/sh
# collections-str-03: array::group never takes queued elements after a late `true` answer.
# (collections-str-03.gx in this directory is an unrelated sprintf repro; this one is .sh.)
# Run: sh design/review-2026-10-12/repro/collections-str-03.sh [path/to/graphix]
# f answers 20ms late, so 2..6 queue while f considers 1. After f answers
# true for 2, group emits [1, 2] and pops nothing: 3..6 stay queued until v
# fires again. The extra `7` written at 300ms is what releases [3, 4]
# (one element per fire of v, lagging); [5, 6] never comes.
# Expected (both engines): g [1, 2], g [3, 4], g [5, 6] (all within ~100ms)
# Observed (both engines, --no-cache and --no-cache --no-fusion):
#   g [1, 2]
#   g [3, 4]      <- only after the 300ms write of 7
#   (no [5, 6])
# Control: the same program with a synchronous f (`|n, x| n == 2`) prints
# [1, 2], [3, 4], [5, 6].
GX=${1:-graphix}
P=$(mktemp --suffix=.gx)
cat > "$P" <<'GX'
sys::exit(sys::time::after_idle(duration:600.ms, 0));
let x = never();
x <- array::iter([1, 2, 3, 4, 5, 6]);
x <- sys::time::timer(duration:300.ms, false) ~ 7;
let g = array::group(x, |n, x| sys::time::after_idle(duration:20.ms, n == 2));
println("g [g]");
"done"
GX
timeout -s KILL 30 "$GX" --no-cache "$P"
rm -f "$P"
