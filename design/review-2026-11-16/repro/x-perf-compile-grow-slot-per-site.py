#!/usr/bin/env python3
# Title: every checked lambda call site gets its own grow-stack spill slot of
#        8*(args+2) bytes, so a kernel's frame is the SUM of its checked calls'
#        arities (2 words per transitive capture), not their max.
# Command: python3 design/review-2026-11-16/repro/x-perf-compile-grow-slot-per-site.py
# Program (N=10/20/40): for i < N
#     let v{i} = i;
#     let f{i} = |x: i64| -> i64 x + v{i} + f{i-1}(x);
#     let g{i} = |t: {a: i64, b: string}| -> i64 t.a + f{i}(1);
#   last expression: g0({..}) + ... + g{N-1}({..})
# Expected: one spill slot per function, sized by the largest checked call
#   (the slot is live only across graphix_grow_stack), so the top region's
#   frame grows with the widest call, linearly in N.
# Observed (debug binary at e1be2fbf2, 2026-11-16, GRAPHIX_DUMP_CLIF=1):
#   N=10: top region 832 bytes in 16 explicit slots, among them one spill
#         slot per checked g{i} call (72, 88, 104, .., 152), widest kernel 25
#         params
#   N=20: 2784 bytes; N=40: 14624 bytes (quadratic); at N=200 the region's
#         frame is 329504 bytes, past MAX_FRAME (262144), and it de-fuses
#         (see x-perf-compile-capture-chain-defuse-cliff.py)
import os, re, subprocess, tempfile

G = "/home/eric/tmp/review-2026-11-16/bin/graphix"
W = tempfile.mkdtemp(prefix="x-perf-compile-slot-", dir="/home/eric/tmp/review-2026-11-16/work")
ENV = dict(os.environ, XDG_CACHE_HOME=W, GRAPHIX_DUMP_CLIF="1")

for n in (10, 20, 40):
    s = ["let v0 = 0;", "let f0 = |x: i64| -> i64 x + v0;",
         "let g0 = |t: {a: i64, b: string}| -> i64 t.a + f0(1);"]
    for i in range(1, n):
        s.append(f"let v{i} = {i};")
        s.append(f"let f{i} = |x: i64| -> i64 x + v{i} + f{i-1}(x);")
        s.append(f"let g{i} = |t: {{a: i64, b: string}}| -> i64 t.a + f{i}(1);")
    s.append(" + ".join(f"g{i}({{a: {i}, b: \"x\"}})" for i in range(n)))
    p = os.path.join(W, f"slots_{n}.gx")
    open(p, "w").write("\n".join(s) + "\n")
    r = subprocess.run(["timeout", "-s", "KILL", "60", G, "--no-cache", "--expand", p],
                       capture_output=True, text=True, env=ENV)
    frames, params, cur = {}, {}, None
    for line in r.stderr.splitlines():
        m = re.match(r"function (u0:\d+)\((.*?)\)", line)
        if m:
            cur = m.group(1)
            frames[cur] = []
            params[cur] = len(m.group(2).split(","))
            continue
        m = re.match(r"\s+ss\d+ = explicit_slot (\d+)", line)
        if m and cur:
            frames[cur].append(int(m.group(1)))
    top = max(frames, key=lambda f: sum(frames[f]))
    print(f"N={n}: widest kernel {max(params.values())} params; "
          f"largest frame {sum(frames[top])} bytes in {len(frames[top])} slots "
          f"(spill slots over 16 bytes: {sorted(x for x in frames[top] if x > 16)})")
