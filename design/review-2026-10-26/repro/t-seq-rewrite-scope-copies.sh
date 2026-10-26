#!/bin/sh
# t-seq: the seq rewrite copies the whole name map at every select arm (and lambda,
# catch, try), and the map holds one entry per `let` the seq has bound so far, so the
# lowering is O(steps x arms x lets).
# command: sh t-seq-rewrite-scope-copies.sh
# Two seqs of N identical steps `{ let a = 1; select a { .. } }`; the second binds each
# step to its own name (`let aI = ..`), which is the only difference.
# observed (debug binary, --check, 10-arm selects):
#   N=2000 unbound 0.6 s, bound 2.3 s;  N=4000 unbound 1.3 s, bound 7.9 s
#   (with 4-arm selects the bound form takes 1.2 s / 4.1 s: the cost follows the arm count)
# expected: the bound form within a small factor of the unbound one, growing linearly.
G=/home/eric/tmp/review-2026-10-26/bin/graphix
D=$(mktemp -d)
for n in 2000 4000; do
  for mode in unbound bound; do
    python3 - "$n" "$mode" > "$D/s.gx" <<'PY'
import sys
n, mode = int(sys.argv[1]), sys.argv[2]
print('let r = seq {')
for i in range(1, n):
    v = '{ let a = 1; select a { 0 => 1, 1 => 2, 2 => 3, 3 => 4, 4 => 5, 5 => 6, 6 => 7, 7 => 8, 8 => 9, _ => 0 } }'
    print(f'  let a{i} = {v};' if mode == 'bound' else f'  {v};')
print('  1')
print('};')
print('r')
PY
    s=$(date +%s%N); timeout -s KILL 170 $G --check "$D/s.gx" >/dev/null 2>&1; e=$(date +%s%N)
    echo "N=$n $mode: $(( (e - s) / 1000000 )) ms"
  done
done
rm -rf "$D"
