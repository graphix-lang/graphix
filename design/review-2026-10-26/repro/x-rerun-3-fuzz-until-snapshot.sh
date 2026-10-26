#!/bin/sh
# Title: the reactive ceremony generator's `until (b >= 3)` on its burst counter reads
#   the step's entry snapshot (no live(..)), so the run stalls forever, yet its run
#   counter is marked reliable and can be the program's tail (fuzz-gen-b-04 variant);
#   the generator never draws live(e) at all.
# Command: sh x-rerun-3-fuzz-until-snapshot.sh [graphix-fuzz]
#   x-rerun-3-fuzz-until-snapshot.prog is program 1 of `graphix-fuzz gen 60 1 --reactive`
#   whose ceremony has `until (v0 >= i64:3)` over burst counter v0; its tail is
#   `((v13 * v8) + v7)`, v13 the ceremony's run counter.
# Expected: the ceremony runs on each burst ("run .." lines), as it does with
#   `until live(v0 >= i64:3)`.
# Observed: plain: 0 "run" lines in 6 epochs (v13 stays 0, the tail never moves with
#   it); with live: 5. The check also warns "this until reads only the step's entry
#   snapshot and calls nothing".
F=${1:-/home/eric/tmp/review-2026-10-26/bin/graphix-fuzz}
D=$(mktemp -d)
HERE=$(cd "$(dirname "$0")" && pwd)
cp "$HERE/x-rerun-3-fuzz-until-snapshot.prog" "$D/plain.gx"
sed -E 's/until \((v[0-9]+ >= i64:[0-9])\)/until live(\1)/g' "$D/plain.gx" > "$D/live.gx"
for p in plain live; do
  n=$(timeout -s KILL 120 "$F" run "$D/$p.gx" 2>&1 | grep '^Interp/InLanguage:' | head -1 | tr '|' '\n' | grep -c 'run ')
  echo "$p: completed runs printed = $n"
done
rm -rf "$D"
