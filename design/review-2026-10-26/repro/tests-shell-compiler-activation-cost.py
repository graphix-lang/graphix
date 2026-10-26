#!/usr/bin/env python3
# Title: an interpreted activation now costs ~11.7 KB, within 5% of recursion_memory.rs's 12 KB ceiling
# Command: timeout -s KILL 170 python3 tests-shell-compiler-activation-cost.py [path/to/real/graphix]
# Expected: the per-activation cost recursion_memory.rs documents ("about 8.7KB
#   measured", measured 2026-10-13) with its margin to MAX_BYTES_PER_ACTIVATION (12288).
# Observed (debug build at e6ffc6eed, the dev profile cargo test uses):
#   1 -> 64596 KB, 5000 -> 123004 KB, 20000 -> 294756 KB peak RSS;
#   low 11964 B/activation, high 11724 B/activation.
import os, subprocess, sys, tempfile
gx = sys.argv[1] if len(sys.argv) > 1 else "graphix"
res = {}
with tempfile.TemporaryDirectory() as d:
    for depth in [1, 5000, 20000]:
        exp = depth * (depth + 1) // 2
        prog = (f"let rec f = |n: i64| -> i64 select n {{\n  0 => 0,\n  _ => n + f(n - 1)\n}};\n"
                f"sys::exit(select f({depth}) {{ {exp} => 0, _ => 1 }})\n")
        path = os.path.join(d, f"rm_{depth}.gx")
        open(path, "w").write(prog)
        p = subprocess.Popen([gx, "--no-cache", "--no-fusion", "--no-netidx", path],
                             stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        _, status, usage = os.wait4(p.pid, 0)
        res[depth] = usage.ru_maxrss
        print(depth, status, usage.ru_maxrss)
print("low", (res[5000] - res[1]) * 1024 // 4999, "high", (res[20000] - res[5000]) * 1024 // 15000)
