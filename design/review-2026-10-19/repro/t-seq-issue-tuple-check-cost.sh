#!/bin/sh
# Title: an issued call in a seq costs the check O(steps): `--check` time grows ~N^2.8
#   with the number of call steps, because each issue atom's tuple carries `pc`, whose
#   type is the set of every step label.
# cmd: sh t-seq-issue-tuple-check-cost.sh [path/to/graphix]
# expected: check time roughly linear in N, as for the same seq of constant writes
#   (`a <- i`: 500 -> 0.08 s, 1000 -> 0.12 s) or of nullary calls (500 -> 0.10 s)
# observed (debug build): `a <- f(i)` or `a <- str::len("x")` steps:
#   125 -> 0.11 s, 250 -> 0.26 s, 500 -> 1.07 s, 1000 -> 7.2 s
G=${1:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
for n in 250 500 1000; do
  python3 -c "
n=$n
body='; '.join(f'a <- str::len(\"x\")' for i in range(n))
print('let a = 0;\nlet r = seq { ' + body + '; a };\nr')" > $D/calls$n.gx
  python3 -c "
n=$n
body='; '.join(f'a <- {i}' for i in range(n))
print('let a = 0;\nlet r = seq { ' + body + '; a };\nr')" > $D/consts$n.gx
  for f in calls consts; do
    s=$(date +%s.%N); timeout -s KILL 170 $G --check $D/$f$n.gx >/dev/null 2>&1; e=$(date +%s.%N)
    echo "$f $n: $(echo "$e - $s" | bc) s"
  done
done
rm -rf $D
