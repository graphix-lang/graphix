#!/bin/sh
# Title: the JsonData/TomlData bound walk (typ/textdata.rs Walk) visits a shared
#   type DAG as a tree: json::write_str / toml::write_str check in 2^depth time
#   (variant of c-lib-02 / t-typ-mod-03, whose other walks are now linear).
# Command: sh design/review-2026-11-16/repro/x-rerun-1-textdata-dag-exponential.sh
# Expected: --check time flat in N, as for `xN == xN`, `uniq(xN)`, `{xN => 1}`,
#   `array::sort([xN])` (about 40 ms at every N on the review binary).
# Observed (debug binary, --check, shared box):
#   json::write_str(xN):          N=20 50 ms, 22 75 ms, 24 170 ms, 26 583 ms, 28 2256 ms
#   toml::write_str({a: xN}):     N=20 49 ms, 22 88 ms, 24 175 ms, 26 571 ms, 28 2205 ms
#   json::write_str over a typedef pair DAG (type R<'a,'b> = ('a,'b); mk = |x| -> R<'a,'a> (x, x)):
#                                 n=18 53 ms, 20 99 ms, 22 273 ms, 24 936 ms
#   while `xN == xN` stays at ~45 ms through N=26.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
for e in 'xN == xN' 'json::write_str(xN)' 'toml::write_str({a: xN})'; do
  for n in 20 22 24 26 28; do
    f=$d/dag.gx
    echo "let x0 = 1;" > $f
    i=1; while [ $i -le $n ]; do echo "let x$i = (x$((i-1)), x$((i-1)));" >> $f; i=$((i+1)); done
    echo "$e" | sed "s/xN/x$n/g" >> $f
    s=$(date +%s%N); timeout -s KILL 120 $G --check $f >/dev/null 2>&1; e2=$(date +%s%N)
    echo "[$e] N=$n $(( (e2 - s) / 1000000 )) ms"
  done
done
rm -rf $d
