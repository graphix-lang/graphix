#!/bin/bash
# x-perf-runtime: under a thread limit the fork-threshold calibration panics, and startup can fail
# cost::calibrate builds a second rayon pool of eval_threads() workers (all
# cores) beside the runtime's evaluation pool and `.expect`s it; under a
# task limit the calibration thread panics (Auto then never forks), and the
# concurrent Control::eval_pool build can run out of threads down to one
# worker and `panic!`, failing the program's startup.
# RLIMIT_NPROC counts every thread of the user, so the margin below is
# relative to the threads running now and the outcome varies run to run.
# Command: bash design/review-2026-10-26/repro/x-perf-runtime-thread-limit.sh
# Expected: fewer workers (or none), a logged warning, the program runs.
# Observed (32-thread host, debug build), among the runs:
#   thread 'graphix-par-calibrate' panicked at graphix-compiler/src/cost.rs:126:10:
#   the calibration pool: ThreadPoolBuildError { kind: IOError(Os { code: 11, .. }) }
#   thread 'tokio-rt-worker' panicked at graphix-types/src/stack.rs:455:31:
#   the evaluation pool: Resource temporarily unavailable (os error 11)
#   Error: loading initial modules  Caused by: channel closed
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
cat > $d/p.gx <<'GX'
let i = 0;
i <- select i { x if x < 100 => x + 1, _ => never() };
println(select i { 100 => "done", _ => never() });
sys::exit(sys::time::after_idle(duration:50.ms, 0))
GX
for extra in 45 60 80 120; do
  cur=$(ps -L -u "$USER" --no-headers | wc -l)
  echo "== $extra threads over the $cur running"
  bash -c "ulimit -u $((cur + extra)); GRAPHIX_PAR=auto timeout -s KILL 30 $G --no-cache $d/p.gx" 2>&1 \
    | grep -E "panicked|pool|Error|channel|done"
done
rm -rf $d
