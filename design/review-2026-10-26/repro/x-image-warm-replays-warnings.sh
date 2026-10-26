#!/bin/bash
# x-image: design/program_image.md says compile-time diagnostics print on the cold run only;
#          the code replays them on a warm start (ProgramRoot::warnings, Env::replay_warnings)
#
# Run: bash x-image-warm-replays-warnings.sh [path-to-graphix]
# Doc claims: run 2 (restores the program entry) prints no warnings.
# Observed (HEAD e6ffc6eed): both runs print the unhandled-`?` and unused-import warnings,
#   which is the intended behaviour (the doc's earlier "Key" paragraph says so): the
#   slice (a) sentence is stale.
G=${1:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > $D/w.gx <<'GX'
use array::map;
let r: Result<i64, `E> = 3;
let x = r?;
sys::exit(sys::time::after_idle(duration:50.ms, 0));
x
GX
for i in 1 2; do
  XDG_CACHE_HOME=$D/cache timeout -s KILL 30 $G $D/w.gx > /dev/null 2> $D/err
  echo "run $i: $(grep -c '^WARNING' $D/err) warning(s)"
done
rm -rf $D
