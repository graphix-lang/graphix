#!/bin/bash
# x-image: `graphix --warm` ("write the image cache and exit") runs the program's first cycle, effects included
#
# Run: bash x-image-warm-runs-effects.sh [path-to-graphix]
# Expected: no side_effect.txt after either --warm run: --warm compiles, writes the cache
#   entries and exits; design/program_image.md says the entry is written before any cycle
#   and --warm exists for deployment pipelines that want the first run fast.
# Observed (HEAD 9445a1d21): the program's first cycle runs under --warm: side_effect.txt
#   is written by run 1 (cold, writes the entry) every time and by run 2 (restores the
#   entry) in 9 of 10 trials; the miss is the cycle losing the race with the exit.
#   `sys::process::spawn` of `echo hi` under --warm prints "hi" the same way.
#   The cycle races the shell's exit, so a longer init is cut off part way.
G=${1:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
cat > $D/warmfx.gx <<EOF2
let t = sys::time::now(null);
sys::fs::write_all(#path: "$D/side_effect.txt", "[t]\n")
EOF2
for i in 1 2; do
  XDG_CACHE_HOME=$D/cache timeout -s KILL 30 $G -n --warm $D/warmfx.gx
  echo "--warm run $i exit $?"
  if [ -e $D/side_effect.txt ]; then echo "  side effect ran: $(cat $D/side_effect.txt)"; rm $D/side_effect.txt; else echo "  no side effect"; fi
done
rm -rf $D
