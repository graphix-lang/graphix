#!/bin/sh
# A union of N abstract (or other unkeyed) types checks in O(N^3): an array
# literal of N abstract values, or a select with N type-test arms over a union
# of N abstracts.
# Command: sh x-perf-compile-abstract-union-cubic.sh
# Expected: --check time grows about linearly, as it does for the same shapes
# over variants or structs (keyed in normalize::Kept).
# Observed (debug, HEAD e6ffc6eed, --no-cache --check, seconds):
#   array literal   n=200 0.050  n=400 0.177  n=800 1.117
#   select          n=200 0.067  n=400 0.219  n=800 1.257  n=1600 9.108
#   (struct members, same select: 0.043 / 0.067 / 0.143 / 0.448)
# gdb samples: every one in Kept::merge_with -> Type::merge -> union_identical.
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
for n in 200 400 800; do
  python3 - "$n" > "$D/arr.gx" <<'PY'
import sys
n = int(sys.argv[1])
s = [f"type C{i} = Abstract<i64>" for i in range(n)]
s.append("let a = [" + ", ".join(f"C{i}({i})" for i in range(n)) + "]")
s.append("array::len(a)")
print(";\n".join(s))
PY
  python3 - "$n" > "$D/sel.gx" <<'PY'
import sys
n = int(sys.argv[1])
s = [f"type C{i} = Abstract<i64>" for i in range(n)]
ty = "[" + ", ".join(f"C{i}" for i in range(n)) + "]"
arms = ",\n  ".join(f"C{i} as c => c.0 + {i}" for i in range(n))
s.append(f"let f = |x: {ty}| select x {{\n  {arms}\n}}")
s.append(f"f(C{n//2}(1))")
print(";\n".join(s))
PY
  for k in arr sel; do
    t0=$(date +%s%N); timeout -s KILL 170 "$GX" --no-cache --check "$D/$k.gx"; t1=$(date +%s%N)
    echo "$k n=$n rc=$? $(( (t1 - t0) / 1000000 )) ms"
  done
done
rm -rf "$D"
