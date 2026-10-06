#!/usr/bin/env python3
# t-tvar-11: settle_terminal's dependency DFS is not stack-guarded; a long
# chain of cells in one call site's signature aborts the process.
#
# FnType::settle_terminal (graphix-types/src/typ/settle.rs:260-280) orders
# the cells a call site's signature reaches with a recursive `visit`, one
# native frame per cell along a dependency chain, outside
# crate::stack::ensure_sufficient (settle_refs, position_cells and
# reached_cells beside it are guarded). Nothing bounds the chain but the
# program.
#
# The generated program is flat (nothing the parser's nesting limit sees):
#   let f = 'a0: Array<'a1>, 'a1: Array<'a2>, .., 'aK: Concrete |x: ('a0, .., 'aK)| x;
#   f(never())
# Every 'ai is a position of f's signature, so the call's terminal settle
# orders all K+1 cells; 'a0 sorts first, so `visit` descends the whole
# chain before anything settles. 'aK: Concrete makes the first settle fail
# at once, so a run that survives the walk ends in a type error.
#
# command: timeout -s KILL 120 python3 design/review-2026-10-05/repro/t-tvar-11.py <graphix> [K ...]
#
# expected: every K ends in the type error, exit 1:
#   the type 'aK must be fully known here, a type-directed operation reads it: annotate it
# observed (HEAD c722befe; debug build, and the quick profile alike):
#   K=20000  exit 1, the type error (4.8 s)
#   K=30000  exit 134: "thread 'tokio-rt-worker' has overflowed its stack /
#            fatal runtime error: stack overflow, aborting" (17 s). The core's
#            backtrace is ~25770 frames of FnType::settle_terminal::visit
#            under drain_pending_settles <- check_and_fuse <- compile_script
#            on a 2 MiB tokio worker stack (~80 bytes a frame).
import os, subprocess, sys, tempfile

graphix = sys.argv[1] if len(sys.argv) > 1 else "graphix"
ks = [int(k) for k in sys.argv[2:]] or [20000, 30000]

def program(k):
    cons = ", ".join([f"'a{i}: Array<'a{i + 1}>" for i in range(k)] + [f"'a{k}: Concrete"])
    tup = ", ".join(f"'a{i}" for i in range(k + 1))
    return f"let f = {cons} |x: ({tup})| x;\nf(never())\n"

with tempfile.TemporaryDirectory() as d:
    env = dict(os.environ, XDG_CACHE_HOME=os.path.join(d, "cache"))
    for k in ks:
        path = os.path.join(d, f"chain{k}.gx")
        with open(path, "w") as f:
            f.write(program(k))
        r = subprocess.run([graphix, "--check", path], env=env, capture_output=True, text=True)
        tail = [l for l in (r.stdout + r.stderr).splitlines() if l.strip()][-2:]
        print(f"K={k} exit {r.returncode}")
        for l in tail:
            print(f"  {l[:160]}")
