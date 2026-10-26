#!/bin/sh
# Fusing a function whose lets bind owned values (structs, strings) costs
# O(lets^2): every abort exit's rung chain use_vars every owned local in scope.
# Command: sh x-perf-compile-owned-chain-quadratic.sh
# Expected: time to first value grows linearly, like --no-fusion (0.12 s at 800).
# Observed (debug, HEAD e6ffc6eed, time to first value, seconds):
#   struct `with` chain n=100 0.25  n=200 0.45  n=400 1.43  n=800 6.1
#   string interpolation chain n=100 0.14  n=200 0.17  n=400 0.40  n=800 1.88
#   CLIF dump of the struct chain: 27k / 80k / 259k lines at n=25/50/100, of which
#   204k at n=100 are `vN -> vM` aliases (instructions and blocks grow linearly).
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
for n in 100 200 400 800; do
  python3 - "$n" > "$D/s.gx" <<'PY'
import sys
n = int(sys.argv[1])
ty = ", ".join(f"f{i}: i64" for i in range(20))
lit = ", ".join(f"f{i}: {i}" for i in range(20))
body = ["let s0 = s"] + [f"let s{i} = {{s{i-1} with f{i % 20}: s{i-1}.f{(i+1) % 20} + 1}}" for i in range(1, n)]
print(f"type S = {{{ty}}};\nlet f = |s: S| {{\n  " + ";\n  ".join(body) + f";\n  s{n-1}.f0\n}};\nf({{{lit}}})")
PY
  for flag in "" "--no-fusion"; do
    python3 - "$GX" "$D/s.gx" $flag <<'PY'
import subprocess, sys, time
t = time.perf_counter()
p = subprocess.Popen(["timeout", "-s", "KILL", "170", sys.argv[1], "--no-cache", *sys.argv[3:], sys.argv[2]], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
line = p.stdout.readline()
print(f"{sys.argv[2]} {' '.join(sys.argv[3:]) or 'fused'}: {time.perf_counter() - t:.2f} s -> {line.decode().strip()}")
subprocess.run(["pkill", "-KILL", "-P", str(p.pid)]); p.kill(); p.wait()
PY
  done
done
rm -rf "$D"
