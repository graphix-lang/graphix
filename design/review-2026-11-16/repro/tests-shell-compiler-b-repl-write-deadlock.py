# Title: common::repl writes the whole input before anything drains the
# child's stdout, so an input past the pipe buffers deadlocks, beyond HANG.
# Command: timeout -s KILL 60 python3 -I design/review-2026-11-16/repro/tests-shell-compiler-b-repl-write-deadlock.py
# This does what graphix-shell/tests/common/mod.rs `repl` does: spawn the
# REPL with piped stdin/stdout, write_all the input, and only then read.
# Expected (if the harness were safe): the write finishes, the run ends.
# Observed: "write_all finished within 20s: False" -- the REPL blocks on a
# full stdout pipe, stops reading stdin, and the write never returns; in
# the harness `within_deadline` has not started, so HANG never fires.
import os
import subprocess
import threading

GX = os.environ.get("GX", "/home/eric/tmp/review-2026-11-16/bin/graphix")
inp = "".join(f"{i} + 0\n" for i in range(20000)).encode()  # ~170 KB
p = subprocess.Popen(
    [GX, "--no-netidx", "--no-cache"],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
)
done = threading.Event()


def write():
    try:
        p.stdin.write(inp)
        p.stdin.close()
    except Exception as e:
        print("write error", e)
    done.set()


threading.Thread(target=write, daemon=True).start()
print("write_all finished within 20s:", done.wait(20))
p.kill()
p.wait()
