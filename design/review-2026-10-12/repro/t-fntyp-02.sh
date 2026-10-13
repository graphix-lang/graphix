#!/bin/bash
# t-fntyp-02: a self-referential fn bound over a union with a variable makes
# --check (and a run) recurse without bound in FnType::collect_tvars.
# (t-fntyp-02.gx in this directory is an unrelated probe; this script is the repro.)
#
# command: bash design/review-2026-10-12/repro/t-fntyp-02.sh [path/to/graphix]
# expected: every program below checks at once (rc 0).
# observed: the first (self-referential bound, called) is OOM-killed at the
#   6 GB cap after ~25 s; gdb shows site_bounds -> FnType::collect_tvars ->
#   for_each_part -> Type::normalize().collect_tvars -> for_each_part ... thousands
#   of frames deep. The three controls (no call, no self-reference, ground
#   union) check in under a second.
G=${1:-graphix}
D=$(mktemp -d)
printf "let f = 'a: fn(x: 'a) -> [string, 'b] |g: 'a| 0;\nf(|x| 0)\n" > $D/selfref.gx
printf "let f = 'a: fn(x: 'a) -> [string, 'b] |g: 'a| 0;\n0\n" > $D/nocall.gx
printf "let f = 'a: fn(x: i64) -> [string, 'b] |g: 'a| 0;\nf(|x| 0)\n" > $D/noself.gx
printf "let f = 'a: fn(x: 'a) -> [string, i64] |g: 'a| 0;\nf(|x| 0)\n" > $D/ground.gx
for p in selfref nocall noself ground; do
  s=$(date +%s)
  timeout -s KILL 60 "$G" --check $D/$p.gx > $D/$p.out 2>&1
  echo "$p rc=$? t=$(( $(date +%s) - s ))s"
done
rm -rf $D
