#!/bin/sh
# Time to the first printed value of a deep call chain, node-walk, per parallel mode.
# Measured at HEAD 7324207f (debug build): off 0.29s, force 0.71s, auto 4.04s (depth 500);
# depth 1000 (same generator, f999): off 0.91s, auto 90.9s. The JIT (fused) path: 0.45s.
G=${GRAPHIX:-graphix}
D=$(dirname "$0")
for p in off force auto; do
  printf '%s: ' "$p"
  GRAPHIX_PAR=$p timeout -s KILL 120 python3 -c '
import subprocess, sys, time
s = time.time()
p = subprocess.Popen(sys.argv[1:], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
line = p.stdout.readline()
print(f"{time.time() - s:.2f}s", line.decode().strip())
p.kill()
' "$G" --no-cache --no-fusion "$D/f-jit-01-deep-chain-par-auto.gx"
done
