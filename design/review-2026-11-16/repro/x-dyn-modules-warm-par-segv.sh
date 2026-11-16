#!/bin/sh
# A warm image start of x-dyn-modules-warm-par-segv.gx under forked
# evaluation segfaults in JIT code; the cold run before it is fine.
# usage: sh x-dyn-modules-warm-par-segv.sh [path-to-graphix]
# expected: "cold: (40, [19, 20, 21])" and "warm: (40, [19, 20, 21])"
# observed: the warm run is killed by SIGSEGV (exit 139)
G=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
here=$(cd "$(dirname "$0")" && pwd)
d=$(mktemp -d)
for run in cold warm; do
  out=$(XDG_CACHE_HOME=$d GRAPHIX_PAR=auto GRAPHIX_PAR_T=4 timeout -s KILL 60 "$G" "$here/x-dyn-modules-warm-par-segv.gx" 2>&1)
  echo "$run: exit $? : $(echo "$out" | tail -1 | cut -c1-200)"
done
rm -rf "$d"
