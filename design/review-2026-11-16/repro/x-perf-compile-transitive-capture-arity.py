#!/usr/bin/env python3
# Title: a lambda kernel takes every capture its callees read, transitively,
#        as a parameter pair: arity grows with call depth and fusion is
#        quadratic in the depth of a call chain whose levels each read a binding.
# Command: python3 design/review-2026-11-16/repro/x-perf-compile-transitive-capture-arity.py
# Programs (D = 100, 200, 400, 800):
#   cap:   let v{i} = i; let f{i} = |x| x + v{i} + f{i-1}(x);  f{D-1}(1)
#   nocap: let f{i} = |x| f{i-1}(x) + 1;                        f{D-1}(1)
# Expected: compile time linear in D for both (D instances, each body two
#   operators and a call).
# Observed (debug binary at e1be2fbf2, 2026-11-16; GRAPHIX_PROFILE of the
#   program's own root, to first output):
#   cap   D=100   34 ms: Link 13, Clif 4, Callees 3, Freeze 6.6k calls
#   cap   D=200  100 ms: Link 48, Clif 14, Callees 10, Freeze 23k calls
#   cap   D=400  405 ms: Link 255, Clif 50, Callees 38, Freeze 87k calls
#   cap   D=800 2000 ms: Link 1530, Clif 178, Callees 149, Freeze 333k calls;
#                 526 MB peak
#   nocap D=800  195 ms: Link 31, Clif 12, Callees 65, Freeze 8k calls;
#                 135 MB peak
#   (f{i}'s kernel signature has 2*(i+1)+5 params: GRAPHIX_DUMP_CLIF)
import os, re, subprocess, tempfile

G = "/home/eric/tmp/review-2026-11-16/bin/graphix"
W = tempfile.mkdtemp(prefix="x-perf-compile-cap-", dir="/home/eric/tmp/review-2026-11-16/work")
ENV = dict(os.environ, XDG_CACHE_HOME=W, GRAPHIX_PROFILE="1")


def program(d, cap):
    if cap:
        s = ["let v0 = 0;", "let f0 = |x| x + v0;"]
        for i in range(1, d):
            s += [f"let v{i} = {i};", f"let f{i} = |x| x + v{i} + f{i-1}(x);"]
    else:
        s = ["let f0 = |x| x + 1;"] + [f"let f{i} = |x| f{i-1}(x) + 1;" for i in range(1, d)]
    p = os.path.join(W, f"{'cap' if cap else 'nocap'}_{d}.gx")
    open(p, "w").write("\n".join(s) + f"\nf{d-1}(1)\n")
    return p


def run(p):
    with tempfile.TemporaryFile(mode="w+") as ef:
        proc = subprocess.Popen(["timeout", "-s", "KILL", "170", G, "--no-cache", p],
                                stdout=subprocess.PIPE, stderr=ef, env=ENV, text=True)
        first = proc.stdout.readline().strip()
        subprocess.run(["pkill", "-KILL", "-f", p])
        proc.wait()
        ef.seek(0)
        err = ef.read()
    root, phases = None, {}
    for line in err.splitlines():
        m = re.match(r"PROFILE root=Compile .*duration_ns=(\d+)", line)
        if m:
            root, phases = int(m.group(1)), {}
            continue
        m = re.match(r"PROFILE phase=(\w+) .*calls=(\d+) self_ns=(\d+)", line)
        if m and root is not None:
            phases[m.group(1)] = (int(m.group(2)), int(m.group(3)))
    ms = lambda k: phases.get(k, (0, 0))[1] / 1e6
    return (f"{root / 1e6:.0f} ms: Link {ms('Link'):.0f}, Clif {ms('Clif'):.0f}, "
            f"Callees {ms('Callees'):.0f}, Freeze {phases.get('Freeze', (0, 0))[0]} calls; out={first}")


for d in (100, 200, 400, 800):
    print(f"cap   D={d}: {run(program(d, True))}")
print(f"nocap D=800: {run(program(800, False))}")
