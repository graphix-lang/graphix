#!/usr/bin/env python3
# Title: an empty REPL line (or a trailing `;`, or a comment-only line) is an
#        output: it prints `-: _` and, at a terminal, takes the prompt away.
# Command: timeout -s KILL 60 python3 design/review-2026-11-16/repro/x-repl-empty-input.py
# Expected: Enter at an empty prompt gives the prompt back; `let x = 5` and
#           `x + 1` then run and show `-: i64` / `6`.
# Observed: `-: _` is printed and the shell enters a value display that never
#           shows anything; the next lines are not read (the terminal echoes
#           them in cooked mode) until Ctrl-C. Piped, an empty line likewise
#           prints `-: _` and replaces the value being shown, and `let b = 1;`
#           prints `-: _`.
import os, pty, re, select, signal, subprocess, time

BIN = os.environ.get("GRAPHIX", "/home/eric/tmp/review-2026-11-16/bin/graphix")
env = dict(os.environ, TERM="xterm", XDG_CACHE_HOME="/tmp/x-repl-empty-cache")

pid, fd = pty.fork()
if pid == 0:
    os.environ.update(env)
    os.execv(BIN, [BIN, "--no-cache", "--no-init"])
buf = bytearray()

def pump(t):
    end = time.time() + t
    while (rem := end - time.time()) > 0:
        if not select.select([fd], [], [], rem)[0]:
            continue
        try:
            d = os.read(fd, 65536)
        except OSError:
            return
        buf.extend(d)
        if b"\x1b[6n" in d:
            os.write(fd, b"\x1b[1;1R")

pump(3)
for line in ["", "let x = 5", "x + 1"]:
    os.write(fd, line.encode() + b"\r")
    pump(1)
os.kill(pid, signal.SIGKILL)
shown = re.sub(r"\x1b\[[0-9;?]*[@-~]", "", buf.decode(errors="replace")).replace("\r", "\n")
print("--- terminal:")
print("\n".join(l for l in shown.splitlines() if l.strip()))
print("--- piped:")
piped = subprocess.run(
    ["timeout", "-s", "KILL", "30", BIN, "--no-cache", "--no-init"],
    input="let clk = sys::time::timer(duration:100.ms, true)\ncount(clk)\n\nlet b = 1;\n",
    capture_output=True, text=True, env=env)
print(piped.stdout)
