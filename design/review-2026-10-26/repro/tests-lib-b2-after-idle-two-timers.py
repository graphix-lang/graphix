# Title: sys::after_idle_invalid_timeout races two timers armed at init
#
# The fixture (stdlib/graphix-tests/src/lib_tests/sys.rs, AFTER_IDLE_INVALID_TIMEOUT)
# writes the invalid timeout from a 10 ms after_idle and expects the 50 ms idle
# wait to fire after that write has landed. A stall of the runtime across the
# window delivers both timers in one batch: the result fires while `timeout` is
# still 0.05 (its write lands a cycle later) and the predicate reads false.
# This driver runs the fixture's program, SIGSTOPs the real graphix process for
# 150 ms at a sweep of offsets, and prints what the program printed.
#
# Command: timeout -s KILL 170 python3 -I design/review-2026-10-26/repro/tests-lib-b2-after-idle-two-timers.py
# Expected: 'true' at every offset (what the test asserts).
# Observed (2026-10-26, debug binary at e6ffc6eed): 'false' at 3-5 offsets
#   between 0.075 and 0.110 in each of two runs ('true' elsewhere).
# The causal form `timeout <- once(timeout) ~ -1.0` (set CAUSAL=1) prints
# 'true' at every offset.
import os, signal, subprocess, sys, tempfile, time

BIN = "/home/eric/tmp/review-2026-10-26/bin/graphix"
TWO_TIMERS = """let timeout = 0.05;
timeout <- sys::time::after_idle(duration:10.ms, -1.0);
let result = sys::time::after_idle(timeout, 7);
sys::exit(sys::time::after_idle(duration:300.ms, result));
result ~ (result == 7 && timeout == -1.0)
"""
CAUSAL = TWO_TIMERS.replace(
    "timeout <- sys::time::after_idle(duration:10.ms, -1.0);",
    "timeout <- once(timeout) ~ -1.0;",
)

def real_pid(prog):
    for d in os.listdir("/proc"):
        if not d.isdigit():
            continue
        try:
            argv = open(f"/proc/{d}/cmdline", "rb").read().split(b"\0")
        except OSError:
            continue
        if argv and argv[0].endswith(b"real/graphix") and prog.encode() in argv[-2:]:
            return int(d)
    return None

def run(prog, stop_at):
    p = subprocess.Popen([BIN, "--no-cache", prog], stdout=subprocess.PIPE,
                         stderr=subprocess.DEVNULL, text=True)
    time.sleep(stop_at)
    pid = real_pid(prog)
    if pid is not None:
        os.kill(pid, signal.SIGSTOP)
        time.sleep(0.15)
        os.kill(pid, signal.SIGCONT)
    try:
        out, _ = p.communicate(timeout=20)
    except subprocess.TimeoutExpired:
        p.kill()
        out, _ = p.communicate()
    return out.strip()

with tempfile.TemporaryDirectory() as d:
    prog = os.path.join(d, "after_idle_invalid.gx")
    with open(prog, "w") as f:
        f.write(CAUSAL if os.environ.get("CAUSAL") else TWO_TIMERS)
    for i in range(21):
        x = 0.03 + 0.005 * i
        print(f"{x:.3f} {run(prog, x)!r}", flush=True)
