#!/bin/bash
# x-perf-runtime: a seq run costs O(steps^2): every same-cycle step handoff scans every pc reader and every carried let
# A seq of N chained `let`s runs in ~3 cycles; the per-run cost should be
# O(N). Each same-cycle handoff in SeqMachine::update_serial delivers pc
# (ExecCtx::deliver -> RtView::store_fire), which walks all ~3N readers of
# pc and pushes a late fire for each reader in a step already passed, and
# each entered step runs TrackedFires::enter, which scans every carried
# let fire (N after a run). The unchained variant (no carried lets) is
# still quadratic, about a third of the cost.
# Command: bash design/review-2026-11-16/repro/x-perf-runtime-seq-steps-quadratic.sh
# Expected: us_per_run roughly doubling with N.
# Observed (debug build, --no-fusion, GRAPHIX_PAR=off; fusion on is the same):
#   chained   N=50 397  N=100 1202  N=200 4128  N=400 16228  N=1600 281066 us/run
#   unchained          N=100  452  N=200 1477  N=400  5216 us/run
#   (2.96 cycles per run at every N)
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
for shape in chained unchained; do
for n in 100 200 400; do
python3 - "$n" "$shape" > $d/p.gx <<'PY'
import sys
n, shape = int(sys.argv[1]), sys.argv[2]
def step(j):
    if j == 0: return "let a0 = t + 1"
    return f"let a{j} = a{j-1} + 1" if shape == "chained" else f"let a{j} = t + {j}"
steps = ";\n  ".join(step(j) for j in range(n))
print(f"""let k = 60;
let i = 0;
let r = seq let t = i {{
  {steps};
  a{n-1}
}};
i <- r ~ select i {{ x if x < k => x + 1, _ => never() }};
let t0 = sys::time::now(select i {{ 10 => 0, _ => never() }});
let done = select i {{ x if x == k => x, _ => never() }};
let t1 = sys::time::now(done);
let el = cast<f64>(sys::time::diff(t1, t0))$ * 1000000.0 / 50.0;
println("{shape} N={n} us_per_run=[el]");
sys::exit(el ~ 0)""")
PY
GRAPHIX_PAR=off timeout -s KILL 170 $G --no-cache --no-fusion $d/p.gx | grep us_per
done
done
rm -rf $d
