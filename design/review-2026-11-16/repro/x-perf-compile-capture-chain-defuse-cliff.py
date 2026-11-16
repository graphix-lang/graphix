#!/usr/bin/env python3
# Title: a fused region whose call sites carry many transitive captures passes
#        MAX_FRAME (one grow-stack spill slot per checked call site), de-fuses,
#        and the retry ladder that follows re-emits every callee once per
#        operand: compile time 4x/15x/37x and memory to OOM.
# Command: python3 design/review-2026-11-16/repro/x-perf-compile-capture-chain-defuse-cliff.py
# Program family (flatmods_N, one file): for i < N
#     let v{i} = i;
#     let f{i} = |x: i64| -> i64 x + v{i} + f{i-1}(x);
#     let g{i} = |t: {a: i64, b: string}| -> i64 t.a + f{i}(1);
#   and the last expression g0({..}) + g1({..}) + ... + g{N-1}({..}).
#   (the same chain written as N modules, each reading its own `let v` and
#   calling the module below, behaves alike: OOM-killed at N=400)
# Expected: compile time and memory linear in the instances (N^2/2), as
#   under --no-fusion: N=200 0.6 s / 220 MB, N=400 2.4-2.6 s / 821-831 MB.
# Observed (debug binary at e1be2fbf2, 2026-11-16, two runs):
#   frame: N=180 fuses whole; N=200 under #[native]: "kernel `region_..`: a
#     frame of 329504 bytes, past 262144 — de-fuse", 20 frame refusals
#   fused, to first output: N=180 0.87-0.91 s 233-236 MB; N=200 4.1-5.0 s
#     604-606 MB; N=240 13.2-14.3 s 1543-1557 MB (GRAPHIX_FUSE_SERIAL=1:
#     11.5-13.1 s 417-421 MB); N=300 33.8-36.7 s 3547-3549 MB; N=400 (module
#     form) OOM-killed at 6 GB after 73 s
#   GRAPHIX_PROFILE, module form N=200: Fusion self 2.5-2.9 s (waiting on
#     tasks); under GRAPHIX_FUSE_SERIAL=1 Callees 1047 ms over 39 region
#     attempts and 7697 functions emitted (N=180: one attempt, 361)
#   the top region's CLIF (GRAPHIX_DUMP_CLIF=1, N=10/20/40) holds one
#     explicit_slot of 8*(args+2) bytes per checked call to g{i}, args growing
#     by 2 per transitive capture: frame 832 / 2784 / 14624 bytes
#     (x-perf-compile-grow-slot-per-site.py)
import os, re, subprocess, sys, tempfile, time

G = "/home/eric/tmp/review-2026-11-16/bin/graphix"
W = tempfile.mkdtemp(prefix="x-perf-compile-cliff-", dir="/home/eric/tmp/review-2026-11-16/work")
ENV = dict(os.environ, XDG_CACHE_HOME=W)


def prog(n, native=False):
    s = ["let v0 = 0;", "let f0 = |x: i64| -> i64 x + v0;",
         "let g0 = |t: {a: i64, b: string}| -> i64 t.a + f0(1);"]
    for i in range(1, n):
        s.append(f"let v{i} = {i};")
        s.append(f"let f{i} = |x: i64| -> i64 x + v{i} + f{i-1}(x);")
        s.append(f"let g{i} = |t: {{a: i64, b: string}}| -> i64 t.a + f{i}(1);")
    last = " + ".join(f"g{i}({{a: {i}, b: \"x\"}})" for i in range(n))
    s += [f"let r = #[native] ({last});", "r"] if native else [last]
    p = os.path.join(W, f"flatmods_{n}{'_native' if native else ''}.gx")
    open(p, "w").write("\n".join(s) + "\n")
    return p


def descendants(pid):
    out = []
    kids = subprocess.run(["pgrep", "-P", str(pid)], capture_output=True, text=True).stdout.split()
    for k in kids:
        out.append(int(k))
        out += descendants(int(k))
    return out


def first_output(args, env=ENV):
    t0 = time.time()
    p = subprocess.Popen(["timeout", "-s", "KILL", "170", G, "--no-cache"] + args,
                         stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, env=env, text=True)
    line = p.stdout.readline()
    dt = time.time() - t0
    hwm = 0
    pids = descendants(p.pid)
    for pid in pids:
        try:
            for l in open(f"/proc/{pid}/status"):
                if l.startswith("VmHWM"):
                    hwm = max(hwm, int(l.split()[1]))
        except OSError:
            pass
    for pid in reversed(pids):
        try:
            os.kill(pid, 9)
        except OSError:
            pass
    p.kill()
    p.wait()
    return dt, hwm // 1024, line.strip()[:20]


for n in (180, 200):
    r = subprocess.run(["timeout", "-s", "KILL", "120", G, "--no-cache", "--expand", prog(n, True)],
                       capture_output=True, text=True, env=dict(ENV, GXDBG_NATIVE_ALL="1"))
    frames = re.findall(r"a frame of \d+ bytes", r.stderr)
    print(f"N={n} #[native]: rc={r.returncode} frame refusals={len(frames)} "
          f"{frames[0] if frames else 'no frame refusal'}")
for n in (180, 200, 240, 300):
    dt, mb, out = first_output([prog(n)])
    print(f"N={n} fused: {dt:.2f}s {mb} MB out={out}")
dt, mb, out = first_output([prog(240)], env=dict(ENV, GRAPHIX_FUSE_SERIAL="1"))
print(f"N=240 fused, GRAPHIX_FUSE_SERIAL=1: {dt:.2f}s {mb} MB out={out}")
for n in (200, 400):
    dt, mb, out = first_output(["--no-fusion", prog(n)])
    print(f"N={n} --no-fusion: {dt:.2f}s {mb} MB out={out}")
