#!/bin/sh
# queuefn: every wrapper call scans the whole queue (QueueState::release), so
# a backlog that grows by one call a cycle costs O(n) a call and O(n^2) in
# all.
#
# Command: sh core-aux-queuefn-backlog-quadratic.sh [path/to/graphix]
# Expected: time growing linearly in N, as the baseline (the same call made
#   directly) does.
# Observed (debug build, 11-16, wall ms incl. ~100 ms startup):
#   queuefn  N=10000: 191   N=20000: 396   N=40000: 1128
#   direct   N=10000: 124   N=20000: 154   N=40000: 205
#   the queuefn excess (67, 242, 923 ms) quadruples per doubling.
G=${1:-graphix}
D=$(mktemp -d)
for N in 10000 20000 40000; do
  cat > "$D/q.gx" <<EOF
let c = 0;
c <- select c { n if n < $N => n + 1, _ => never() };
let q = queuefn(#trigger: never(), |x: i64| -> i64 x);
let r = q(c);
sys::exit(select c { $N => 0, _ => never() });
c
EOF
  sed 's/let r = q(c);/let r = (|x: i64| -> i64 x)(c);/' "$D/q.gx" > "$D/d.gx"
  for f in q d; do
    s=$(date +%s%N)
    timeout -s KILL 170 "$G" --no-cache "$D/$f.gx" > /dev/null 2>&1
    e=$(date +%s%N)
    echo "$f N=$N $(( (e - s) / 1000000 )) ms"
  done
done
rm -rf "$D"
