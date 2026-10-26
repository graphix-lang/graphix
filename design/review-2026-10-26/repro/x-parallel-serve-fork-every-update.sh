#!/bin/bash
# Title: a server forks its running request instances at every update,
#   unmeasured: waiting instances that read a ticking value burn ~25x the CPU
#   of a serial run for the same answers in the same wall time.
# Command: bash design/review-2026-10-26/repro/x-parallel-serve-fork-every-update.sh
#   (binds 127.0.0.1:18765; default GRAPHIX_EVAL_THREADS = cores, 32 here)
# Expected: Auto costs about what serial does when every instance's update is
#   a few nodes (the cost model would not fork such siblings).
# Observed (debug build, 32 cores):
#   PAR=off  N=300 ok=300 wall=3.15s cpu_during=0.62s
#   PAR=auto N=300 ok=300 wall=3.14s cpu_during=5.92s
#   PAR=off  N=20  ok=20  wall=3.11s cpu_during=0.13s
#   PAR=auto N=20  ok=20  wall=3.12s cpu_during=3.20s
BIN=${BIN:-/home/eric/tmp/review-2026-10-26/bin/real/graphix}
D=$(mktemp -d)
cat > $D/serve.gx <<'GX'
let tick = 0;
tick <- sys::time::timer(duration:1.ms, true) ~ tick + 1;
let handler = |req: http::Request| {
  let start = req ~ tick;
  select tick { t if t > start + 1500 => {body: "ok", headers: [], status: u16:200}, _ => never() }
};
let server = http::serve(#addr: "127.0.0.1:18765", #handler: handler)$;
http::server_addr(server)
GX
cat > $D/load.py <<'PY'
import subprocess, time, os, sys, threading, urllib.request
env = dict(os.environ); env.update(GRAPHIX_PAR=sys.argv[3])
p = subprocess.Popen([sys.argv[1], "--no-cache", sys.argv[2]], env=env,
                     stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
def cpu():
    f = open(f"/proc/{p.pid}/stat").read().split(")")[1].split()
    return (int(f[11]) + int(f[12])) / os.sysconf("SC_CLK_TCK")
time.sleep(2.0)
N = int(sys.argv[4]); ok = [0]
def go():
    try:
        urllib.request.urlopen("http://127.0.0.1:18765/", timeout=20).read(); ok[0] += 1
    except Exception:
        pass
c0 = cpu(); t0 = time.time()
ts = [threading.Thread(target=go) for _ in range(N)]
for t in ts: t.start()
for t in ts: t.join()
c1 = cpu(); t1 = time.time()
p.kill(); p.wait()
print(f"PAR={sys.argv[3]} N={N} ok={ok[0]} wall={t1-t0:.2f}s cpu_during={c1-c0:.2f}s")
PY
for n in 300 20; do for m in off auto; do
  timeout -s KILL 90 python3 -I $D/load.py $BIN $D/serve.gx $m $n
done; done
rm -rf $D
