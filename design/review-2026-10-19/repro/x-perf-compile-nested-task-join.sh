#!/bin/sh
# Title: a nested compile task's join re-merges everything its descendants
# accumulated, so a chain of static resolutions pays O(depth x instances) in joins.
# Command: GRAPHIX=/home/eric/tmp/review-2026-10-19/bin/graphix sh x-perf-compile-nested-task-join.sh
# Expected: TaskJoin time per call constant as the chain grows.
# Observed (debug build, HEAD 9445a1d21, --no-fusion):
#   n=800  TaskJoin calls=802  total 0.045 s (56 us/call)
#   n=1600 TaskJoin calls=1602 total 0.134 s (84 us/call)
#   n=3200 TaskJoin calls=3202 total 0.548 s (171 us/call)
G=${GRAPHIX:-graphix}
T=$(mktemp -d)
for n in 800 1600 3200; do
  python3 - "$n" > "$T/p.gx" <<'PY'
import sys
n = int(sys.argv[1])
print("sys::exit(sys::time::after_idle(duration:1.ms, 0));")
print("let f0 = |x| x;")
for i in range(1, n):
    print(f"let f{i} = |x| f{i-1}(x) + 1;")
print(f"f{n-1}(1)")
PY
  GRAPHIX_PROFILE=1 GRAPHIX_PAR=off timeout -s KILL 120 "$G" --no-cache --no-fusion "$T/p.gx" 2>&1 >/dev/null \
    | grep "PROFILE phase=TaskJoin" | python3 -c '
import sys
c = t = 0
for l in sys.stdin:
    kv = dict(x.split("=", 1) for x in l.split() if "=" in x)
    c += int(kv["calls"]); t += int(kv["total_ns"])
print(f"n='"$n"' TaskJoin calls={c} total={t/1e9:.3f} s ({t/1e3/max(c,1):.0f} us/call)")'
done
rm -rf "$T"
