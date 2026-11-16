#!/bin/bash
# x-perf-runtime: a read in an arm that is not running marks its select every fire: a tick costs every row
# array::map over N idle slots whose callback is
#   select *r > 1000000 { true => i, false => *r + 1 }
# The `true` arm is never taken, but it reads the ticking `i`: every tick
# marks that arm's boundary and, through BoundaryRef::mark's climb, the
# slot's instance-body boundary, so MapQ cannot skip the slot
# (skip_idle_slot's can_skip) and every select runs to ride its standing
# arm. The control reads the idle `z` in that arm.
# Command: bash design/review-2026-11-16/repro/x-perf-runtime-sleeping-arm-marks.sh
# Expected: both shapes cost what the control costs (nothing in the map
#   changes: its value never fires).
# Observed (debug build, GRAPHIX_PAR=off), us per tick of i:
#   arm reads i:  N=1000 451  N=4000 1786  N=16000 10336 (--no-fusion; fused 445/1745/9812)
#   arm reads z:  N=1000 3.7  N=4000 3.7   N=16000 3.2
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
for arm in i z; do
for n in 1000 4000 16000; do
cat > $d/p.gx <<GX
let k = 510;
let i = 0;
i <- select i { x if x < k => x + 1, _ => never() };
let z = 0;
let rs = array::init($n, |j| &z);
let ts = array::map(rs, |r| select *r > 1000000 { true => $arm, false => *r + 1 });
let t0 = sys::time::now(select i { 10 => 0, _ => never() });
let done = select i { x if x == k => x, _ => never() };
let t1 = sys::time::now(done);
let el = cast<f64>(sys::time::diff(t1, t0))\$ * 1000000.0 / 500.0;
println("arm reads $arm: N=$n us_per_tick=[el] ts0=[ts[0]\$]");
sys::exit(el ~ 0)
GX
GRAPHIX_PAR=off timeout -s KILL 170 $G --no-cache --no-fusion $d/p.gx | grep us_per
done
done
rm -rf $d
