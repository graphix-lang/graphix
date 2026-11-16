#!/usr/bin/env python3
# Title: discovering a region's lambda callees fingerprints each callee by
#        walking every body it reaches (body_fingerprint over
#        for_each_reachable_node), so the Callees phase is quadratic in the
#        depth of a static call chain.
# Command: python3 design/review-2026-11-16/repro/x-perf-compile-fingerprint-chain.py
# Program (D = 200, 400, 800):
#   let f0 = |x| x + 1; let f{i} = |x| f{i-1}(x) + 1; ..; f{D-1}(1)
# Expected: the Callees phase linear in D (each instance's own body walked
#   once).
# Observed (debug binary at e1be2fbf2, 2026-11-16; GRAPHIX_PROFILE of the
#   program's own root, to first output):
#   D=200 Callees 4.3 ms; D=400 15.6 ms; D=800 61.4 ms (the largest phase at
#   D=800: Link 28.8, Typecheck1 20.3)
import os, re, subprocess, tempfile

G = "/home/eric/tmp/review-2026-11-16/bin/graphix"
W = tempfile.mkdtemp(prefix="x-perf-compile-fp-", dir="/home/eric/tmp/review-2026-11-16/work")
ENV = dict(os.environ, XDG_CACHE_HOME=W, GRAPHIX_PROFILE="1")

for d in (200, 400, 800):
    s = ["let f0 = |x| x + 1;"] + [f"let f{i} = |x| f{i-1}(x) + 1;" for i in range(1, d)]
    p = os.path.join(W, f"chain_{d}.gx")
    open(p, "w").write("\n".join(s) + f"\nf{d-1}(1)\n")
    with tempfile.TemporaryFile(mode="w+") as ef:
        proc = subprocess.Popen(["timeout", "-s", "KILL", "120", G, "--no-cache", p],
                                stdout=subprocess.PIPE, stderr=ef, env=ENV, text=True)
        first = proc.stdout.readline().strip()
        subprocess.run(["pkill", "-KILL", "-f", p])
        proc.wait()
        ef.seek(0)
        err = ef.read()
    phases, root = {}, False
    for line in err.splitlines():
        if line.startswith("PROFILE root="):
            root = line.startswith("PROFILE root=Compile")
            phases = {} if root else phases
            continue
        m = re.match(r"PROFILE phase=(\w+) .*self_ns=(\d+)", line)
        if m and root:
            phases[m.group(1)] = int(m.group(2)) / 1e6
    top = sorted(phases.items(), key=lambda kv: -kv[1])[:4]
    print(f"D={d}: Callees {phases.get('Callees', 0):.1f} ms; top "
          + ", ".join(f"{k} {v:.1f}" for k, v in top) + f"; out={first}")
