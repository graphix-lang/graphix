#!/bin/bash
# x-panics: Env::seed_typedef_refs_under walks each typedef's whole reach with a fresh
# `seen` set: a chain of N typedefs costs O(N^2) in SeedTypes.
# Command: bash x-panics-seed-types-quadratic.sh
# Expected: build time linear in N, as --check is (--check of N=10000: ~150 ms).
# Observed (debug binary, GRAPHIX_PROFILE=1 shows phase=SeedTypes is ~99% of Compile):
#   N=2500 446 ms, N=5000 1471 ms, N=10000 6003 ms, N=20000 24378 ms;
#   N typedefs that do not reference each other: 112 / 132 / 146 ms.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
for n in 2500 5000 10000; do
  python3 - "$n" "$D/chain_$n.gx" <<'PY'
import sys
n = int(sys.argv[1])
lines = [f'type T{i} = [`L{i}(T{i+1}), null];' for i in range(n)]
lines += [f'type T{n} = i64;', 'sys::exit(sys::time::after_idle(duration:10.ms, 0));', '0']
open(sys.argv[2], 'w').write('\n'.join(lines) + '\n')
PY
  s=$(date +%s%N)
  timeout -s KILL 100 "$G" --no-cache "$D/chain_$n.gx" >/dev/null 2>&1
  rc=$?
  e=$(date +%s%N)
  echo "N=$n rc=$rc ms=$(( (e - s) / 1000000 ))"
done
rm -rf "$D"
