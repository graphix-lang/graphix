#!/bin/bash
# Title: a deterministic subject for a minimizer pin (minimize/shrink have none):
#        a value carrying the twin tag is a Pair::Twin finding in every run.
# Command: bash design/review-2026-11-16/repro/fuzz-lib-c-minimize-twin-pin.sh
# Expected: minimize returns a shorter text whose check shows the same finding.
# Observed (HEAD e1be2fbf2, budget 40, sessions and forks off to keep it short):
#   minimized (41 checks):
#   { let a = 1; let b = a; let c = "TwinDiverged"; let d = b; c }
#   DIVERGENCE — twin invariant violated (...)
#     trace: Trace([0:"TwinDiverged"])
BIN=${GRAPHIX_FUZZ:-/home/eric/tmp/review-2026-11-16/bin/graphix-fuzz}
D=$(mktemp -d)
cat > $D/twin.gx <<'GX'
{
  let a = i64:1;
  let b = a + i64:2;
  let c = "TwinDiverged";
  let d = b * i64:3;
  (d, c)
}
GX
GRAPHIX_FUZZ_SESSIONS=0 GRAPHIX_FUZZ_FORK=0 timeout -s KILL 175 $BIN minimize $D/twin.gx 40 2>&1 | grep -v '^  minimize:'
rm -rf $D
