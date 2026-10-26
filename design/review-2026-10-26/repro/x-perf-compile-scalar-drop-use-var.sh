#!/bin/sh
# A fused function with a chain of scalar selects builds in quadratic time:
# the scope drop at the body's end calls emit_drop_local on every local, which
# use_vars disc and payload before emit_owned_drop returns for a scalar, so
# Cranelift's SSA builder walks each scalar local back through every select
# join (making and removing a block param at each: a value alias).
# Command: sh x-perf-compile-scalar-drop-use-var.sh
# Expected: CLIF size and build time linear in the number of selects.
# Observed (debug, HEAD e6ffc6eed):
#   CLIF aliases (`vN -> vM`, all unused): n=50 2597, n=100 10197, n=200 40397
#   (blocks 400 / 800 / 1600); time to first value n=800 0.51 s, 1600 1.31 s,
#   3200 4.94 s, with phase Clif (emission + SSA) 0.63 s at 1600 and 3.30 s at
#   3200. graphix-fuzz run: fused=1. In the n=4 dump every alias is created
#   after the last select, at the body's end.
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
gen() {
  python3 - "$1" <<'PY'
import sys
n = int(sys.argv[1])
body = ["let a0 = x"] + [f"let a{i} = select a{i-1} {{ v if v > {i} => v - 1, v => v + 2 }}" for i in range(1, n)]
print("let f = |x: i64| {\n  " + ";\n  ".join(body) + f";\n  a{n-1}\n}};\nf(1)")
PY
}
for n in 50 100 200; do
  gen $n > "$D/p.gx"
  echo "n=$n aliases: $(GRAPHIX_DUMP_CLIF=1 timeout -s KILL 120 "$GX" --no-cache --expand "$D/p.gx" 2>&1 | grep -c -- ' -> v')"
done
for n in 800 1600 3200; do
  gen $n > "$D/p.gx"
  t0=$(date +%s%N); timeout -s KILL 170 "$GX" --no-cache --expand "$D/p.gx" >/dev/null 2>&1; t1=$(date +%s%N)
  echo "n=$n build (--expand) $(( (t1 - t0) / 1000000 )) ms"
done
rm -rf "$D"
