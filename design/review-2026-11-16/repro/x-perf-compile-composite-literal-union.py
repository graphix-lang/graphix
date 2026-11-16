#!/usr/bin/env python3
# Title: a union of N same-shaped composite alternatives (an array literal of
#        structs, tuples or arrays; `any` over struct literals) checks in
#        O(N^2): Alternatives::apart scans every alternative at each collapse.
# Command: python3 design/review-2026-11-16/repro/x-perf-compile-composite-literal-union.py
# Programs: `let a = [E0, E1, ..]; array::len(a)` with E{i} one of
#   {x: i, y: "s"}, (i, "s"), [i], `A(i), i  (and `any({a: x}, ..)`)
# Expected: Typecheck0 linear in N for every element shape, as it is for
#   primitives and variants.
# Observed (debug binary at e1be2fbf2, 2026-11-16; Typecheck0 of the
#   program's own root, `--check --no-cache`):
#   struct  N=1000 3.3 ms, 2000 9.9, 4000 36.0, 8000 145.6  (4x per doubling)
#   tuple   N=1000 3.0, 2000 9.6, 4000 37.7, 8000 145.1
#   array   N=1000 3.1, 2000 10.1, 4000 36.7, 8000 150.5
#   any({a: x}, ..) N=1000 2.6, 2000 9.8, 4000 39.4
#   variant N=1000 0.7, 2000 1.4, 4000 3.5, 8000 10.2  (a Variant pair
#           collapses with an empty Welds, so it never reaches apart)
#   i64     N=1000 0.1, 2000 0.1, 4000 0.1, 8000 0.2   (element_union dedupes)
#   an annotated `let a: Array<R> = [..]` is as slow as the bare literal
import os, re, subprocess, tempfile

G = "/home/eric/tmp/review-2026-11-16/bin/graphix"
W = tempfile.mkdtemp(prefix="x-perf-compile-union-", dir="/home/eric/tmp/review-2026-11-16/work")
ENV = dict(os.environ, XDG_CACHE_HOME=W, GRAPHIX_PROFILE="1")

SHAPES = {
    "struct": lambda i: f'{{x: {i}, y: "s"}}',
    "tuple": lambda i: f'({i}, "s")',
    "array": lambda i: f"[{i}]",
    "variant": lambda i: f"`A({i})",
    "i64": lambda i: f"{i}",
}


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


for shape, el in SHAPES.items():
    row = []
    for n in (1000, 2000, 4000, 8000):
        src = "let a = [" + ", ".join(el(i) for i in range(n)) + "];\narray::len(a)\n"
        row.append(f"N={n} {typecheck0_ms(src, f'{shape}_{n}.gx')}")
    print(f"{shape:8} " + ", ".join(row))
row = []
for n in (1000, 2000, 4000):
    src = "let x = 1;\nany(" + ", ".join("{a: x}" for _ in range(n)) + ")\n"
    row.append(f"N={n} {typecheck0_ms(src, f'any_{n}.gx')}")
print("any      " + ", ".join(row))
