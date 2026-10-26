# Title: an unguarded typedef chain still checks in quadratic time, either order (c-node-mod-03 remainder)
# Command: timeout -s KILL 170 python3 -I design/review-2026-10-26/repro/x-rerun-5-typedef-chain-quadratic.py /home/eric/tmp/review-2026-10-26/bin/graphix <scratch dir>
# Expected: --check time near linear in N, like a guarded chain ([`A(T{i+1}), `B]: 72 ms at 4000).
# Observed (debug build): forward/backward 1000: 0.20/0.19 s, 2000: 0.82/0.81 s, 4000: 4.6/4.6 s.
#   Env::deftype (and check_contractive) walks reaches_unguarded through every earlier
#   typedef the body reaches unguarded, and `seen` is a linear scan.
import os, subprocess, sys, time
graphix, d = sys.argv[1], sys.argv[2]
def run(name, lines):
    p = os.path.join(d, name + ".gx")
    open(p, "w").write("\n".join(lines) + "\n0\n")
    t = time.time()
    subprocess.run(["timeout", "-s", "KILL", "120", graphix, "--check", p], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    print(f"{name}: {time.time() - t:.2f} s")
for n in (1000, 2000, 4000):
    fwd = [f"type T{i} = [`A{i}(i64), T{i+1}];" for i in range(n)] + [f"type T{n} = `End;"]
    run(f"forward{n}", fwd)
    run(f"backward{n}", list(reversed(fwd)))
    run(f"guarded{n}", [f"type T{i} = [`A{i}(T{i+1}), `B];" for i in range(n)] + [f"type T{n} = `End;"])
