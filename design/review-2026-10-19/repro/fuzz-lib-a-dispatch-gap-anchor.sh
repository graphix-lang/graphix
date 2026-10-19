#!/bin/sh
# Title: the dispatch route's gap compiles land in the first dispatch epoch's
#   trace segment, so that epoch's offsets are anchored at a gap compile and
#   vary run to run; every dispatch-route pair is held only at Final strength.
# Command: sh design/review-2026-10-19/repro/fuzz-lib-a-dispatch-gap-anchor.sh
# Expected (if the gap compiles had a segment of their own): every Dispatch run
#   shows the same offsets in every epoch.
# Observed: the first dispatch epoch's offset is 4, 5 or 6 depending on the run;
#   the second dispatch epoch's offset is 1 in all eight dispatch runs.
cd /home/eric/proj/graphix/graphix-fuzz/findings || exit 1
timeout -s KILL 170 /home/eric/tmp/review-2026-10-19/bin/graphix-fuzz run \
  callable-route-aug2026/02_dispatch_route_matrix.gx 2>&1 \
  | grep -E '^(Interp|Jit)/Dispatch' \
  | sed -E 's/abstract:[A-Za-z0-9+\/=]+, //g'
