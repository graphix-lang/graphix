#!/bin/bash
# x-perf-runtime: GRAPHIX_PAR=auto makes a 1 ms-timer cycle slower and burns ~6x the CPU
# 2000 node-walked slots, one of which owns a 1 ms repeating timer; each
# timer cycle visits every slot (~0.4 us each, debug), so the slot loop
# costs ~0.9 ms, over Auto's threshold (cost.rs SlotSite, 4x a worker's
# wake latency), and Auto forks it onto the 32-worker pool every cycle.
# Command: bash design/review-2026-10-26/repro/x-perf-runtime-auto-cpu.sh
# Expected: Auto no slower than serial, and its CPU in proportion to any
#   wall-clock gain.
# Observed (debug build, --no-fusion, 32-thread host, 3 s run):
#   PAR=off   cycles=1434-1451  cpu 1.5-1.6 s
#   PAR=auto  cycles=1260-1324  cpu 8.2-13.0 s   (fewer cycles, 6-8x the CPU)
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
cat > $d/p.gx <<'GX'
let n = 2000;
let ts = array::init(n, |j| {
  let k = 0;
  k <- sys::time::timer(select j { 0 => duration:1.ms, _ => duration:1000.s }, true) ~ k + 1;
  k
});
let c = count(ts);
let e = sys::time::timer(duration:3.s, false);
println(e ~ "cycles=[c]");
sys::exit(sys::time::timer(duration:3100.ms, false) ~ 0);
0
GX
for p in off auto; do
  python3 -c "
import resource,subprocess,os
r=subprocess.run(['timeout','-s','KILL','60','$G','--no-cache','--no-fusion','$d/p.gx'],capture_output=True,text=True,env=dict(os.environ,GRAPHIX_PAR='$p'))
u=resource.getrusage(resource.RUSAGE_CHILDREN)
print('PAR=$p', [l for l in r.stdout.splitlines() if 'cycles' in l], 'cpu_s=%.2f' % (u.ru_utime+u.ru_stime))"
done
rm -rf $d
