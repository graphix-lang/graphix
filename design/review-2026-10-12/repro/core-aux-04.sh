#!/bin/sh
# core-aux-04: queuefn #count through a place reference is patched once per
# change, not as a level. write_count sets a binding target with
# Rt::set_level but a place target with Rt::patch_var (an ordinary queued
# write), so four depth changes in one cycle land one per cycle.
# command: sh core-aux-04.sh [path-to-graphix]
# Expected: place=-1 binding=-1, then place=3 binding=3
#   (mod.gxi: a place is written "as to a binding: as a level").
# Observed (node-walk and JIT alike):
#   place=-1 binding=-1 / place=0 binding=3 / place=1 binding=3 /
#   place=2 binding=3 / place=3 binding=3
G=${1:-graphix}
F=$(mktemp --suffix=.gx)
cat > "$F" <<'GX'
let s = { depth: -1, other: 0 };
let d = -1;
let qp = queuefn(#count: &mut s.depth, #trigger: never(), |x: i64| -> i64 x * 10);
let qd = queuefn(#count: &mut d, #trigger: never(), |x: i64| -> i64 x * 10);
qp(1); qp(2); qp(3); qp(4);
qd(1); qd(2); qd(3); qd(4);
println("place=[s.depth] binding=[d]");
sys::exit(sys::time::after_idle(duration:100.ms, 0));
null
GX
timeout -s KILL 30 "$G" --no-cache "$F"
rm -f "$F"
