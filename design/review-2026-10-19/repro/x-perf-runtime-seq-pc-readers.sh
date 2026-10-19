#!/bin/bash
# x-perf-runtime: a seq run of S cycle-separated steps costs O(S^2)
# Every step's value is gated on the machine's pc (`seqpc ~! v`, see
# `graphix --expand`), so by_ref[pc] holds one Reader per step, and each pc
# delivery (one per step) walks all of them in GXRt::wake_readers: an
# `updated` entry and a boundary mark per reader, although one step runs.
# Command: bash design/review-2026-10-19/repro/x-perf-runtime-seq-pc-readers.sh
# Expected: time per step roughly constant in S.
# Observed (debug build, GRAPHIX_PAR=off): us per step
#   S=400 24  S=800 29  S=1600 39  S=3200 62   (about 20 us + 13 ns x S)
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
for s in 400 800 1600 3200; do
  python3 - "$s" > $d/p.gx <<'PY'
import sys
s = int(sys.argv[1])
steps = ";\n".join("  a <- a + 1" for _ in range(s))
print(f"""let a = 0;
let t0 = sys::time::now(0);
let r = seq {{
{steps};
  a
}};
let t1 = sys::time::now(r);
let el = cast<f64>(sys::time::diff(t1, t0))$ * 1000.0;
println("S={s} ms=[el] us_per_step=[el * 1000.0 / {s}.0]");
sys::exit(el ~ 0)""")
PY
  GRAPHIX_PAR=off timeout -s KILL 120 $G --no-cache $d/p.gx
done
rm -rf $d
