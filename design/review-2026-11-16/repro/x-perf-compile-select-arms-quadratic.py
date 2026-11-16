#!/usr/bin/env python3
# Title: a select's check is quadratic in its arm count: Reach re-walks what
#        still reaches at every arm, and check_repeats compares every arm with
#        every earlier one.
# Command: python3 design/review-2026-11-16/repro/x-perf-compile-select-arms-quadratic.py
# Programs:
#   tags: type U = [`V0(i64), .., `V{N-1}(i64)]; let u: U = `V0(1);
#         select u { `V0(x) => x + 0, .., `V{N-1}(x) => x + N-1 }
#   lits: let x = 3; select x { 0 => 0, 1 => 2, .., N-1 => .., _ => 0 }
# Expected: Typecheck0 linear in the arms.
# Observed (debug binary at e1be2fbf2, 2026-11-16; Typecheck0 of the
#   program's own root, `--check --no-cache`):
#   tags N=100 7.5 ms, 200 27.7, 400 109.2, 800 417.6
#   lits N=200 1.2 ms, 400 4.0, 800 14.5, 1600 53.6
import os, re, subprocess, tempfile

G = "/home/eric/tmp/review-2026-11-16/bin/graphix"
W = tempfile.mkdtemp(prefix="x-perf-compile-sel-", dir="/home/eric/tmp/review-2026-11-16/work")
ENV = dict(os.environ, XDG_CACHE_HOME=W, GRAPHIX_PROFILE="1")


def typecheck0_ms(src, name):
    p = os.path.join(W, name)
    open(p, "w").write(src)
    r = subprocess.run(["timeout", "-s", "KILL", "120", G, "--check", "--no-cache", p],
                       capture_output=True, text=True, env=ENV)
    t, inroot = None, False
    for line in r.stderr.splitlines():
        if line.startswith("PROFILE root="):
            inroot = line.startswith("PROFILE root=Compile")
            continue
        m = re.match(r"PROFILE phase=Typecheck0 .*self_ns=(\d+)", line)
        if m and inroot:
            t = int(m.group(1)) / 1e6
    return f"{t:.1f}" if t is not None else f"rc={r.returncode}"


row = []
for n in (100, 200, 400, 800):
    ty = ", ".join(f"`V{i}(i64)" for i in range(n))
    arms = ",\n  ".join(f"`V{i}(x) => x + {i}" for i in range(n))
    src = f"type U = [{ty}];\nlet u: U = `V0(1);\nselect u {{\n  {arms}\n}}\n"
    row.append(f"N={n} {typecheck0_ms(src, f'tags_{n}.gx')}")
print("tags " + ", ".join(row))
row = []
for n in (200, 400, 800, 1600):
    arms = ",\n  ".join(f"{i} => {i * 2}" for i in range(n))
    src = f"let x = 3;\nselect x {{\n  {arms},\n  _ => 0\n}}\n"
    row.append(f"N={n} {typecheck0_ms(src, f'lits_{n}.gx')}")
print("lits " + ", ".join(row))
