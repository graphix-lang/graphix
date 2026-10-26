#!/bin/bash
# x-perf-runtime: a map whose one slot changes costs every slot each cycle
# array::map over [&i, &z, &z, ...]: only slot 0 reads the ticking `i`, so
# only slot 0's instance body boundary is marked, yet MapQ::update calls every
# standing slot's CallSite (args, GXLambda formals, the body's Prune check,
# TagValue clones) and then rebuilds the result.
# Command: bash design/review-2026-10-26/repro/x-perf-runtime-idle-slots.sh
# Expected: the per-cycle cost of the idle slots near the cost of rebuilding
#   the result array (the fused map rebuilds 10k elements in ~20 ns each).
# Observed (debug build, --no-fusion, GRAPHIX_PAR=off), us per cycle:
#   n=500 101   n=2000 371   n=8000 1525   (~190 ns per idle slot)
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
for n in 500 2000 8000; do
  python3 - "$n" > $d/p.gx <<'PY'
import sys
n = int(sys.argv[1])
refs = ", ".join(["&z"] * (n - 1))
print(f"""let k = 1010;
let i = 0;
i <- select i {{ x if x < k => x + 1, _ => never() }};
let z = 0;
let rs = [&i, {refs}];
let ts = array::map(rs, |r| *r + 1);
let t0 = sys::time::now(select i {{ 10 => 0, _ => never() }});
let done = select i {{ x if x == k => x, _ => never() }};
let t1 = sys::time::now(done);
let el = cast<f64>(sys::time::diff(t1, t0))$ * 1000.0;
println("n={n} us_per_cycle=[el]");
sys::exit(el ~ 0)""")
PY
  GRAPHIX_PAR=off timeout -s KILL 120 $G --no-cache --no-fusion $d/p.gx
done
rm -rf $d
