#!/usr/bin/env python3
# Title: FusionCtx::dead grows by one LambdaId per deleted lambda definition
#   for as long as nothing links: slot churn leaks 8 bytes a definition
#
# Command (about 3 minutes; two runs at a time at most):
#   timeout -s KILL 600 python3 design/review-2026-11-16/repro/f-mod-lowering-dead-defs-grow.py
#
# The program maps a callback holding one lambda definition (`let h = ..`)
# over an array that alternates between 1 and 1000 elements, so each pair of
# cycles builds and deletes 1000 slot instances. Deleting a slot deletes its
# Lambda node, which calls FusionCtx::forget_def; the id waits in
# FusionCtx::dead for the next FusionCtx::link, and none comes: every slot
# takes its kernels from the prototype's table (share::fuse_slot), which
# emits nothing.
#
# Expected: peak RSS does not depend on how long the program runs.
# Observed (debug binary, this host):
#   400 ticks,  fusion on : 110 MB      6400 ticks, fusion on : 145 MB
#   400 ticks,  --no-fusion: 103 MB     6400 ticks, --no-fusion: 110 MB
#   6400 ticks, fusion on, `h` defined once outside the callback: 93 MB
# i.e. about 28 MB more for 3 million more slot lifetimes: 8 bytes each, and
# the Vec's slack.
import os, resource, subprocess, sys, tempfile

G = os.environ.get("GRAPHIX", "/home/eric/tmp/review-2026-11-16/bin/graphix")
BIG = ", ".join(str(i) for i in range(1000))

def prog(limit, callback):
    return f"""let big = [{BIG}];
let hoisted = |q: i64| q + 1;
let tick = 0;
tick <- select tick {{ n if n < {limit} => tick + 1, _ => never() }};
let xs = select tick % 2 == 0 {{ true => tick ~ [0], false => tick ~ big }};
let r = array::map(xs, {callback});
sys::exit(filter(tick, |n: i64| n == {limit}) ~ 0);
array::len(r)
"""

LAMBDA = "|v| { let h = |q: i64| q + 1; (h(v), count(v)) }"
HOISTED = "|v| (hoisted(v), count(v))"

def run(limit, callback, flags):
    # a child of its own, so ru_maxrss is this run's alone
    code = (
        "import resource, subprocess, sys\n"
        "subprocess.run(sys.argv[1:], stdout=subprocess.DEVNULL)\n"
        "print(resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss // 1024)\n"
    )
    with tempfile.NamedTemporaryFile("w", suffix=".gx", delete=False) as f:
        f.write(prog(limit, callback))
    try:
        out = subprocess.run(
            [sys.executable, "-c", code, "timeout", "-s", "KILL", "175", G, "--no-cache", *flags, f.name],
            capture_output=True, text=True,
        )
        return out.stdout.strip()
    finally:
        os.unlink(f.name)

for limit, callback, flags, what in [
    (400, LAMBDA, [], "400 ticks, fusion on"),
    (400, LAMBDA, ["--no-fusion"], "400 ticks, --no-fusion"),
    (6400, LAMBDA, [], "6400 ticks, fusion on"),
    (6400, LAMBDA, ["--no-fusion"], "6400 ticks, --no-fusion"),
    (6400, HOISTED, [], "6400 ticks, fusion on, the lambda defined once outside"),
]:
    print(f"{what}: peak RSS {run(limit, callback, flags)} MB", flush=True)
