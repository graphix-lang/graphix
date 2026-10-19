#!/bin/sh
# x-rerun-4: t-misc-03's fix (a one-element array/list literal binds its own cell
# with no walk, TVar::bind_unshared) does not reach a map literal or a literal
# with two elements: a chain `let x_i = {1 => x_(i-1)}` or `[x_(i-1), x_(i-1)]`
# still checks in quadratic time.
# Command: sh x-rerun-4-literal-chain.sh [path/to/graphix]
# Expected: each kind near-linear in n, like `[x_(i-1)]` (2000 lets: 0.16 s).
# Observed (debug build, loaded machine): seconds for n = 1000 / 2000 / 4000
#   one (control): 0.15 / 0.19 / 0.21   two: 0.31 / 1.07 / 3.6   map: 0.59 / 2.3 / 9.0
G=${1:-graphix}
D=$(mktemp -d)
export XDG_CACHE_HOME=$D/cache
for kind in one two map; do
  for n in 1000 2000 4000; do
    python3 - "$kind" "$n" > "$D/c.gx" <<'PY'
import sys
k, n = sys.argv[1], int(sys.argv[2])
print("let x0 = 1;")
for i in range(1, n):
    p = f"x{i-1}"
    e = {"one": f"[{p}]", "two": f"[{p}, {p}]", "map": f"{{1 => {p}}}"}[k]
    print(f"let x{i} = {e};")
print(f"x{n-1}")
PY
    s=$(date +%s.%N)
    timeout -s KILL 120 "$G" --no-cache --check "$D/c.gx" > /dev/null 2>&1; rc=$?
    e=$(date +%s.%N)
    echo "$kind n=$n rc=$rc $(echo "$e - $s" | bc)s"
  done
done
rm -rf "$D"
