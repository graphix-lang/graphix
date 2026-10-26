#!/bin/bash
# Title: the JIT backend builds rayon's global pool only to read its size
# Command: bash design/review-2026-10-26/repro/x-parallel-rayon-global-pool.sh
# Expected: the thread count does not depend on RAYON_NUM_THREADS beyond the
#   backend's own scoped compile threads (which exit after a link).
# Observed (32-core box, GRAPHIX_EVAL_THREADS=4): 78 threads with
#   RAYON_NUM_THREADS unset, 43 with RAYON_NUM_THREADS=1: ~35 idle global-pool
#   workers (named after the tokio thread that built them) live for the
#   process. Under thread pressure the same call panicked instead of taking
#   backend_all's single-thread path:
#   thread 'tokio-rt-worker' panicked at rayon-core-1.13.0/src/registry.rs:171:10:
#   The global thread pool has not been initialized.: ThreadPoolBuildError {
#   kind: IOError(Os { code: 11, kind: WouldBlock, .. }) }
#   Error: loading initial modules / Caused by: channel closed
BIN=${BIN:-/home/eric/tmp/review-2026-10-26/bin/real/graphix}
D=$(mktemp -d)
cat > $D/p.gx <<'GX'
{
  let n = 0;
  n <- select n < 3 { true => n + 1, false => never() };
  let f = |xs: Array<i64>| (array::map(xs, |x| x + 1), array::map(xs, |x| x * 2), array::map(xs, |x| x - 3), array::filter(xs, |x| x > 2), array::fold(xs, 0, |a, x| a + x));
  let g = |xs: Array<i64>| (array::map(xs, |x| x + 4), array::map(xs, |x| x * 5), array::filter(xs, |x| x > 1), array::fold(xs, 1, |a, x| a * x));
  (f([n, n + 1, n + 2]), g([n, 2, 3]))
}
GX
cat > $D/t.py <<'PY'
import subprocess, time, os, sys
env = dict(os.environ); env.pop("RAYON_NUM_THREADS", None)
env.update(GRAPHIX_EVAL_THREADS="4", TOKIO_WORKER_THREADS="2")
env.update(dict(a.split("=", 1) for a in sys.argv[3:]))
p = subprocess.Popen([sys.argv[1], "--no-cache", sys.argv[2]], env=env,
                     stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
time.sleep(4)
n = len(os.listdir(f"/proc/{p.pid}/task"))
p.kill(); p.wait()
print(sys.argv[3:] or "RAYON_NUM_THREADS unset", "threads:", n)
PY
timeout -s KILL 30 python3 -I $D/t.py $BIN $D/p.gx
timeout -s KILL 30 python3 -I $D/t.py $BIN $D/p.gx RAYON_NUM_THREADS=1
rm -rf $D
