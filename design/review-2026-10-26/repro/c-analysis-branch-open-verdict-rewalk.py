#!/usr/bin/env python3
# Title: a fork verdict left open by a call that never binds (a null optional
#   callback) re-runs analysis::independent over the whole subtree and every
#   reachable instance at each cycle that binds anything anywhere.
# Command: python3 -I design/review-2026-10-26/repro/c-analysis-branch-open-verdict-rewalk.py
#   (BIN defaults to the review's debug graphix; writes two programs to a temp dir)
# Expected: the null-callback program costs what the bound-callback one does
#   (same work per cycle once the verdict is known), about as GRAPHIX_PAR=off.
# Observed (debug build, 1000 cycles, each growing an array::init: a rebuild):
#   null cb,  PAR=auto: wall 6.28 s / 6.47 s
#   bound cb, PAR=auto: wall 2.94 s / 2.97 s
#   either,   PAR=off:  wall 2.88-2.95 s
#   about 3.4 ms a cycle re-walking ~12k nodes: the tuple's verdict is None
#   (dependent only through the unbound `f(n)`), Siblings::decide re-asks it
#   whenever Control::rebuilds moved, and the growth moves it every cycle.
import os, subprocess, sys, tempfile, time
BIN = os.environ.get("BIN", "/home/eric/tmp/review-2026-10-26/bin/graphix")
K, S = 300, 40
lines = ["let f0 = |x: i64| x;"]
for k in range(1, K + 1):
    body = " ".join(f"let a{j} = {'x' if j == 0 else 'a' + str(j - 1)} + {j};" for j in range(S))
    lines.append(f"let f{k} = |x: i64| {{ {body} f{k-1}(a{S-1}) }};")
tail = """
let cb: [fn(x: i64) -> i64, null] = CB;
let t = sys::time::timer(duration:1.ms, true);
let n = 0;
n <- t ~ n + 1;
let grow = array::init(n, |i| i * 2);
let r = ({ let y = n * 2; f%d(y) }, select cb { null => 0, f => f(n) }, array::len(grow));
sys::exit(select n > 1000 { true => 0, false => never() });
r
""" % K
d = tempfile.mkdtemp()
for name, cb in [("null", "null"), ("bound", "|x: i64| x")]:
    p = os.path.join(d, name + ".gx")
    open(p, "w").write("\n".join(lines) + tail.replace("CB", cb))
    for par in ["auto", "off"]:
        env = dict(os.environ, GRAPHIX_PAR=par)
        t = time.time()
        r = subprocess.run(["timeout", "-s", "KILL", "170", BIN, "--no-cache", "--no-fusion", p],
                           env=env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        print(f"{name:5} PAR={par:4} exit={r.returncode} wall={time.time() - t:.2f}s")
