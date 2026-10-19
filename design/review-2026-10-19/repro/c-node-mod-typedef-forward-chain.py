# Title: a chain of typedefs each naming the next one unguarded (a union
# member) checks in cubic time: TypeDef::compile rechecks every pending
# contractivity question, and none is ever dropped once decided.
#
# Command:
#   timeout -s KILL 300 python3 -I design/review-2026-10-19/repro/c-node-mod-typedef-forward-chain.py \
#       /home/eric/tmp/review-2026-10-19/bin/graphix
# Expected: --check time roughly linear in N, and the same whether the chain
#   is written top-down (forward references) or bottom-up.
# Observed (debug build, loaded machine):
#   forward  N=250 0.8 s, N=500 6.7 s, N=1000 62 s   (~8x per doubling)
#   backward N=500 0.14-0.5 s; guarded forward chain ([`A(T{i+1}), `B]) N=1000 0.08 s
import os, subprocess, sys, tempfile, time

graphix = sys.argv[1]
d = tempfile.mkdtemp()

def run(name, lines):
    p = os.path.join(d, name + ".gx")
    with open(p, "w") as f:
        f.write("\n".join(lines) + "\n0\n")
    t = time.time()
    subprocess.run(["timeout", "-s", "KILL", "120", graphix, "--check", p],
                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    print(f"{name}: {time.time() - t:.2f} s")

for n in (250, 500):
    fwd = [f"type T{i} = [`A{i}(i64), T{i+1}];" for i in range(n)] + [f"type T{n} = `End;"]
    run(f"forward{n}", fwd)
    run(f"backward{n}", list(reversed(fwd)))
