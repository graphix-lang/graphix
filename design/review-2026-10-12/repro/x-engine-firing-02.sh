#!/bin/sh
# x-engine-firing-02: resuming outside Select/seq is not a wake: a `<-` target
# resets to its initializer and a captured fire during the pause is lost.
# (x-engine-firing-02.gx in this directory is an unrelated earlier probe.)
#
# One body, {let t = 0; t <- a ~ t + 1; t*100 + x}, paused by a bottom (d=0 at
# e3), `a` fires during the pause (e4), resumed at e5 (d=1), under four hosts:
# a select arm, an array::map slot, an array::fold slot, a dynamic call whose
# function was bottom.
# run: sh x-engine-firing-02.sh <path to graphix-fuzz>
# expected (select, per CLAUDE.md "slots pause as a deselected arm does and
#   wake, caught up"): e5 = [0:210 1:310]   (t kept at 2, a's e4 fire caught up)
# observed: select [0:i64:210 1:i64:310]; map [0:[i64:10]]; fold [0:i64:10];
#   dyn [0:i64:10]  (t republished as 0 by Bind's wake_refresh, no catch-up).
#   Both engines agree. GXDBG_LETBIND=1 shows t's let at the resume:
#   select `held=[BindId(..)] publishing=true`, map `held=[] publishing=true`.
FUZZ=${1:-graphix-fuzz}
D=$(mktemp -d)
S='// schedule-v1: cap=64 events=512; in2=i64:1; in1=i64:1; in2=i64:0; in1=i64:2; in2=i64:1; in1=i64:3'
B='{let t = i64:0; t <- a ~ t + i64:1; t * i64:100 + x}'
printf '%s\n{\nlet f = |d, a| select i64:10 / d {x => %s};\nf(in2, in1)\n}\n' "$S" "$B" > $D/sel.gx
printf '%s\n{\nlet f = |d, a| array::map([i64:10 / d], |x| %s);\nf(in2, in1)\n}\n' "$S" "$B" > $D/map.gx
printf '%s\n{\nlet f = |d, a| array::fold([i64:10 / d], i64:0, |acc, x| %s);\nf(in2, in1)\n}\n' "$S" "$B" > $D/fold.gx
printf '%s\n{\nlet g = |x, a| %s;\nlet f = |k: fn(x: i64, a: i64) -> i64, a| k(i64:10, a);\nf(select in2 {i64:1 => g, _ => never()}, in1)\n}\n' "$S" "$B" > $D/dyn.gx
for f in sel map fold dyn; do
  echo "=== $f"
  timeout -s KILL 180 "$FUZZ" run $D/$f.gx 2>&1 | grep -E '^(Interp|Jit)/InLanguage:'
done
rm -rf $D
