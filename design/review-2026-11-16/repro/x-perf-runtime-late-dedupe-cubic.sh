#!/bin/bash
# x-perf-runtime: GX::deliver_late dedupes late fires by a linear scan: a seq waiting after N same-cycle steps costs O(N^3)
# The same chained seq as x-perf-runtime-seq-steps-quadratic.sh, with a
# last step that waits 1 ms on a timer. The N same-cycle handoffs leave
# ~1.5 N^2 late pc fires (one per passed pc reader per handoff); the
# next cycle delivers no pc, so deliver_late takes every one, and its
# `fires.iter().any(|(_, known)| *known == r)` scans up to ~3N distinct
# readers for each.
# Command: bash design/review-2026-11-16/repro/x-perf-runtime-late-dedupe-cubic.sh
# Expected: about the no-wait seq's cost plus the 1 ms wait.
# Observed (debug build, --no-fusion, GRAPHIX_PAR=off), us per run:
#   N=50 2468  N=100 3280  N=200 19109  N=400 128884
#   (without the wait step: N=200 4128, N=400 16228)
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
for n in 100 200 400; do
python3 - "$n" > $d/p.gx <<'PY'
import sys
n = int(sys.argv[1])
steps = ";\n  ".join([f"let a{j} = a{j-1} + 1" if j else "let a0 = t + 1" for j in range(n)])
print(f"""let k = 30;
let i = 0;
let r = seq let t = i {{
  {steps};
  let w = sys::time::timer(duration:1.ms, false);
  a{n-1}
}};
i <- r ~ select i {{ x if x < k => x + 1, _ => never() }};
let t0 = sys::time::now(select i {{ 10 => 0, _ => never() }});
let done = select i {{ x if x == k => x, _ => never() }};
let t1 = sys::time::now(done);
let el = cast<f64>(sys::time::diff(t1, t0))$ * 1000000.0 / 20.0;
println("N={n} us_per_run=[el]");
sys::exit(el ~ 0)""")
PY
GRAPHIX_PAR=off timeout -s KILL 170 $G --no-cache --no-fusion $d/p.gx | grep us_per
done
rm -rf $d
