#!/bin/sh
# Under a task (thread) limit, `graphix --check` of a one-line file panics in
# the fork-threshold calibration and can fail outright: the calibration builds
# a second pool of eval_threads() workers beside the evaluation pool and the
# tokio workers, and every spawn failure there is an expect/panic.
# Command: sh x-perf-compile-thread-limit-panic.sh   (32-core host)
# Expected: rc=0 with no panic at any limit a single-threaded check fits in
#   (the evaluation pool already halves its size when a spawn fails).
# Observed (debug, HEAD e6ffc6eed): TasksMax=40: "thread 'graphix-par-calibrate'
#   panicked at graphix-compiler/src/cost.rs: the calibration pool:
#   ThreadPoolBuildError ... Resource temporarily unavailable", then "the
#   evaluation pool: Resource temporarily unavailable", "Error: loading initial
#   modules / channel closed", rc=1. TasksMax=70: the calibration panic, rc=0.
#   The shared review host (pids.max 8192 over ~14 agents) produced the same
#   panics ("spawn the calibration thread") on plain runs.
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/real/graphix}
F=$(mktemp --suffix=.gx); echo 1 > "$F"
for t in 40 70; do
  echo "== TasksMax=$t"
  timeout -s KILL 30 systemd-run --user --scope --quiet --collect -p MemoryMax=6G \
    -p TasksMax=$t -- "$GX" --no-cache --check "$F"
  echo "rc=$?"
done
rm -f "$F"
