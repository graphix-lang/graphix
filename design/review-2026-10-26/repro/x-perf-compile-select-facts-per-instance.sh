#!/bin/sh
# Every instance of a select builds LazyArmFacts at its first update, and each
# arm's shallow_discriminant re-walks the whole scrutinee union
# (PatternNode::covers_what_it_meets -> union_members, Type::shallow_discriminant
# -> flatten_union_members), expanding each named member's typedef: O(arms x
# members) per instance, paid again by every collection slot / call instance.
# Command: sh x-perf-compile-select-facts-per-instance.sh
# Expected: a select's per-instance setup independent of the arm count squared;
#   the facts depend only on the definition's patterns and scrutinee type.
# Observed (debug, HEAD e6ffc6eed, --no-cache --no-fusion, time to first value,
#   2000 slots of array::map calling f):
#   abstract members: arms=25 0.92 s, 50 2.40 s, 100 8.87 s (500 slots: 2.07 s)
#   tag members:      arms=25 0.42 s, 50 0.77 s, 100 2.19 s
#   gdb: 8 of 8 samples in LazyArmFacts::build -> shallow_discriminant.
#   A warm image start of a 1600-arm select over abstracts spends 0.89 s here.
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
for n in 25 50 100; do
  python3 - "$n" > "$D/p.gx" <<'PY'
import sys
n, m = int(sys.argv[1]), 2000
s = [f"type C{i} = Abstract<i64>" for i in range(n)]
ty = "[" + ", ".join(f"C{i}" for i in range(n)) + "]"
arms = ",\n  ".join(f"C{i} as c => c.0 + {i}" for i in range(n))
s.append(f"let f = |x: {ty}| select x {{\n  {arms}\n}}")
s.append(f"let xs: Array<{ty}> = array::init({m}, |i| C0(i))")
s.append("array::fold(array::map(xs, |x| f(x)), 0, |a, b| a + b)")
print(";\n".join(s))
PY
  python3 - "$GX" "$D/p.gx" "$n" <<'PY'
import subprocess, sys, time
t = time.perf_counter()
p = subprocess.Popen(["timeout", "-s", "KILL", "170", sys.argv[1], "--no-cache", "--no-fusion", sys.argv[2]], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
line = p.stdout.readline()
print(f"arms={sys.argv[3]}: {time.perf_counter() - t:.2f} s -> {line.decode().strip()}")
subprocess.run(["pkill", "-KILL", "-P", str(p.pid)]); p.kill(); p.wait()
PY
done
rm -rf "$D"
