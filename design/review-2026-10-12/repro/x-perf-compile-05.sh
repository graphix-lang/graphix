#!/bin/sh
# x-perf-compile-05: the shell can drop a value produced before sys::exit
# (graphix-shell/src/lib.rs, ShellBuilder::run: the unbiased select! may take
# the exit request over an update batch already queued in from_gx).
# Command: GRAPHIX=<path to graphix> sh design/review-2026-10-12/repro/x-perf-compile-05.sh
# Program: x-perf-compile-05-exit-drops-output.gx (value 100 in cycle 0,
# sys::exit 1 ms after idle).
# Expected: printed 20/20 (every run prints 100, exit 0).
# Observed at 7324207f (debug build): printed 5/20; the other runs print
# nothing and exit 0. With a 300 ms exit delay: 6/6.
G=${GRAPHIX:-graphix}
D=$(dirname "$0")
c=0
for i in $(seq 1 20); do
  o=$(timeout -s KILL 30 "$G" --no-cache "$D/x-perf-compile-05-exit-drops-output.gx")
  [ "$o" = "100" ] && c=$((c+1))
done
echo "printed $c/20"
