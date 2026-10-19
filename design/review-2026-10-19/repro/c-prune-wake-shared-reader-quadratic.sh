#!/bin/sh
# Title: a variable read by N boundaries registers in O(N) each: startup is
#        quadratic in the number of collection slots that capture it.
# Command: sh design/review-2026-10-19/repro/c-prune-wake-shared-reader-quadratic.sh
#   (GRAPHIX defaults to the review's debug binary)
# Expected: `plaint` (each slot's callback reads the outer `t`) costs a
#   constant factor over `plain` (the callback reads only its element).
# Observed (debug build, opt-level s, --no-fusion):
#   plain  n=32000 0.93s   plaint n=32000 1.65s
#   plain  n=64000 1.95s   plaint n=64000 5.94s
#   plain  n=128000 3.51s  plaint n=128000 17.99s
# Each slot's instance body is its own prune boundary, so each registers a
# distinct Reader for `t`; GXRt::ref_var searches the id's reader list
# (SmallVec<[(Reader, u32); 1]>) linearly, and unref_var does the same.
GRAPHIX=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
for n in 32000 64000 128000; do
  printf 'let t = 1;\nlet src = array::init(%s, |i| i);\nlet r = array::map(src, |x| x + 1);\nsys::exit(sys::time::after_idle(duration:50.ms, 0));\narray::len(r)\n' $n > $d/plain.gx
  sed 's/x + 1/x + t/' $d/plain.gx > $d/plaint.gx
  for f in plain plaint; do
    s=$(date +%s.%N)
    timeout -s KILL 170 $GRAPHIX --no-cache --no-fusion $d/$f.gx > /dev/null 2>&1
    e=$(date +%s.%N)
    echo "$f n=$n $(echo "$e - $s" | bc)s"
  done
done
rm -rf $d
