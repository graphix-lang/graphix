#!/usr/bin/env bash
# f-jit-04: elaborating a static call chain costs memory and time quadratic
# in its depth. Every statically resolved call site forks the compile context
# (graphix-compiler/src/node/callsite.rs, `ctx.fork()` before bind_instance),
# CompileCtx::fork (graphix-compiler/src/lib.rs) copies resolving_lambdas
# whole, an IntMap of FnTypes as deep as the resolution in progress, and the
# forks nest along the chain, each alive until its child returns.
#
# The program: f0 = |a| a * 2, f_i = |a| f_{i-1}(a) + 1, then f_N(x). A flat
# program with as many lambdas is the control.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/f-jit-04.sh
#          (needs python3; reports each run's peak RSS)
#
# expected: peak memory linear in N, like the flat control.
# observed (HEAD c722befe, debug build, --no-fusion --no-cache):
#   chain N=250   88 MB     chain N=500  170 MB
#   chain N=1000 478 MB     chain N=1500 969 MB
#   flat  N=1000  75 MB     chain N=1000 --check 77 MB
#   GRAPHIX_PROFILE TaskFork: 172 us per fork at N=500, 292 us at N=1000.
#   Reported by the reviewer: N=2000 1729 MB, N=4000 killed at a 6 GB cap.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT

python3 - "$GRAPHIX" "$dir" <<'EOF'
import os, resource, subprocess, sys
graphix, d = sys.argv[1], sys.argv[2]

def prog(n, chain):
    s = "let x = sys::time::after_idle(duration:1.ms, 3);\n"
    s += "let f0 = |a: i64| a * 2;\n"
    for i in range(1, n + 1):
        s += f"let f{i} = |a: i64| f{i-1}(a) + 1;\n" if chain else f"let f{i} = |a: i64| a + {i};\n"
    if chain:
        s += f"let r = f{n}(x) + 0;\n"
    else:
        s += "let r = " + " + ".join(f"f{i}(x)" for i in range(0, n + 1, max(1, n // 50))) + ";\n"
    return s + "sys::exit(sys::time::after_idle(duration:100.ms, 0));\nr\n"

def run(kind, n, *flags):
    path = os.path.join(d, f"{kind}{n}.gx")
    open(path, "w").write(prog(n, kind == "chain"))
    env = dict(os.environ, XDG_CACHE_HOME=os.path.join(d, "cache"))
    # each run in a child of its own, so RUSAGE_CHILDREN is that run's peak
    r, w = os.pipe()
    pid = os.fork()
    if pid == 0:
        os.close(r)
        p = subprocess.run(["timeout", "-s", "KILL", "170", graphix, "--no-netidx",
                            "--no-cache", *flags, path], capture_output=True, text=True, env=env)
        peak = resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss // 1024
        os.write(w, f"{kind} N={n} {' '.join(flags)}: exit {p.returncode}, peak {peak} MB, "
                    f"prints {p.stdout.strip()[-20:]!r}\n".encode())
        os._exit(0)
    os.close(w)
    os.waitpid(pid, 0)
    print(os.read(r, 4096).decode(), end="", flush=True)

for n in (250, 500, 1000, 1500):
    run("chain", n, "--no-fusion")
run("flat", 1000, "--no-fusion")
run("chain", 1000, "--check")
EOF
