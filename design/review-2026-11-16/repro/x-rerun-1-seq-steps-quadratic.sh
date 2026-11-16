#!/bin/sh
# Title: one run of a seq of N same-cycle steps costs O(N^2) (c-prune-wake-05,
#   whose per-step input count K is now cheap; the step count N is not).
# Command: sh design/review-2026-11-16/repro/x-rerun-1-seq-steps-quadratic.sh
# Expected: the run time (second element) linear in N, as the same chain of
#   lets in a plain block is (0.19 ms at N=800, 0.52 ms at N=1600).
# Observed (debug binary, shared box; PAR off/auto and PRUNE off alike):
#   N=400 20 ms, 800 72 ms, 1600 288 ms, 3200 1190 ms (x4 per doubling).
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
for n in 400 800 1600 3200; do
  python3 - "$n" > "$D/p.gx" <<'PY'
import sys
n = int(sys.argv[1])
steps = ";\n  ".join(f"let v{i} = v{i-1} + 1" if i else "let v0 = go" for i in range(n))
print(f"""let go = 1;
let t0 = sys::time::now(go);
let r = seq go {{
  {steps};
  v{n-1}
}};
let t1 = sys::time::now(r);
sys::exit(sys::time::after_idle(duration:50.ms, t1 ~ 0));
(r, sys::time::diff(t1, t0))""")
PY
  echo "N=$n: $(timeout -s KILL 120 $G --no-cache "$D/p.gx" 2>&1 | tail -1)"
done
rm -rf "$D"
