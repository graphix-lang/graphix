#!/bin/bash
# Title: a node-walked collection slot costs several KB: array::init + array::map over
#   1M elements exceeds a 6 GB cap in the node-walk (found re-running the 10-26 probe
#   f-kernel-standing-bottom-reruns under --no-fusion, whose 2M-element fold was OOM-killed).
# Command: bash design/review-2026-11-16/repro/x-rerun-5-nodewalk-slot-memory.sh [graphix]
#   (the review's graphix wrapper caps each run at 6 GB and prints "[probe] OOM-KILLED")
# Expected: memory in the tens of bytes per element beyond the values themselves.
# Observed (debug review binary):
#   init only, 1M, node-walk            -> 1000000 (fits)
#   init + map, 500k, node-walk         -> 500000 in 6.8 s
#   init + map, 1M, node-walk           -> OOM-KILLED past 6 GB
#   init + fold, 500k ok / 700k OOM-KILLED, node-walk
#   init + map, 1M, fused               -> 1000000 at once
#   init + map whose callback calls str::parse (node-walked, so the map does not fuse),
#     1M, default flags                 -> OOM-KILLED past 6 GB after ~10 s
G=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
run() { # n body flags
  printf 'let arr = array::init(%s, |i| i);\n%s\nsys::exit(sys::time::after_idle(duration:50.ms, r ~ 0));\narray::len(r)\n' "$1" "$2" > $d/p.gx
  echo "$1 [$2] $3: $(timeout -s KILL 150 $G --no-cache $3 $d/p.gx 2>&1 | tail -1)"
}
run 1000000 'let r = arr;' --no-fusion
run 500000 'let r = array::map(arr, |x| x + 1);' --no-fusion
run 1000000 'let r = array::map(arr, |x| x + 1);' --no-fusion
run 1000000 'let r = array::map(arr, |x| x + 1);' ""
run 1000000 'let r = array::map(arr, |x| str::len(str::parse(cast<string>(x)$)$));' ""
rm -rf $d
