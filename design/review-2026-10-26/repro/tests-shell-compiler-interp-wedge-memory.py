#!/usr/bin/env python3
# Title: interrupt_wedge's node-walk wedges allocate ~0.5 GB/s until signalled
# Command: timeout -s KILL 60 python3 tests-shell-compiler-interp-wedge-memory.py [path/to/real/graphix]
#   (pass the unwrapped binary; it is SIGKILLed after 8 s)
# Expected: a wedge test holds bounded memory while it waits 6 s to signal it
#   (the JIT twin stays at ~72 MB).
# Observed (debug build, --no-fusion): RSS ~500 MB at 1 s, ~2.7 GB at 6 s, ~3.2 GB
#   at 7 s for FIRST_CYCLE_WEDGE; LATER_CYCLE_WEDGE ~1.7 GB after 4 s. The four
#   interp tests in interrupt_wedge.rs run concurrently under `cargo test`, so the
#   file alone peaks near 10 GB.
import os, signal, subprocess, sys, tempfile, time
gx = sys.argv[1] if len(sys.argv) > 1 else "graphix"
prog = "{ let rec f = |v: i64| -> i64 f(v + i64:1); f(i64:0) }"
with tempfile.TemporaryDirectory() as d:
    path = os.path.join(d, "wedge.gx")
    open(path, "w").write(prog)
    p = subprocess.Popen([gx, "--no-netidx", "--no-cache", "--no-fusion", path],
                         stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    t0 = time.time()
    while time.time() - t0 < 8 and p.poll() is None:
        try:
            rss = [int(l.split()[1]) for l in open(f"/proc/{p.pid}/status") if l.startswith("VmRSS")][0]
            print(f"{time.time() - t0:4.1f}s RSS {rss // 1024} MB", flush=True)
        except Exception:
            pass
        time.sleep(1)
    p.send_signal(signal.SIGKILL)
    p.wait()
