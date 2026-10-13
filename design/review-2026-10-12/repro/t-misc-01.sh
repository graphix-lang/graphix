#!/bin/bash
# t-misc-01: kernel per-activation blocks freed outside a cycle never
# release their activation charge.
#
# Drives the REPL through a pty: enter a 50-deep fused recursion, Ctrl-C
# (clears the output, which deletes the expression: ToGX::Delete runs
# outside with_control), enter it again, ... under an activation budget of
# 120. Only one 50-deep recursion is ever live.
#
# usage: bash t-misc-01.sh /path/to/graphix            (JIT)
#        GXFLAGS=--no-fusion bash t-misc-01.sh /path/to/graphix
# expected: "-: i64 / 1275" four times in both modes.
# observed (JIT): 1275 twice, then the third evaluation aborts the runtime
#   (log: "activation budget (120) exceeded by a recursion — aborting the
#   runtime"; the shell then panics "called after complete" in
#   tokio oneshot). --no-fusion: 1275 four times.
B=${1:?graphix binary}
L='{ let x = array::iter([i64:50]); let rec f = |k: i64| -> i64 select k { i64:0 => i64:0, _ => k + f(k - i64:1) }; f(x) }'
GRAPHIX_ACTIVATION_BUDGET=120 timeout -s KILL 90 python3 -I - "$B" "$L" "$L" "$L" "$L" <<'PY' | tr -d '\r' | grep -v '^\s*$' | grep -v 'let x'
import os, pty, sys, time, select, re
B = sys.argv[1]
lines = sys.argv[2:]
pid, fd = pty.fork()
if pid == 0:
    os.execv(B, [B, "--no-cache", "-n"] + os.environ.get("GXFLAGS","").split())
out = b""
def pump(t):
    global out
    end = time.time() + t
    while time.time() < end:
        r, _, _ = select.select([fd], [], [], 0.05)
        if r:
            try:
                d = os.read(fd, 65536)
            except OSError:
                return False
            if not d:
                return False
            out += d
            n = d.count(b"\x1b[6n")
            for _ in range(n):
                os.write(fd, b"\x1b[1;1R")
    return True
pump(3)
for l in lines:
    os.write(fd, l.encode() + b"\r")
    pump(3)
    os.write(fd, b"\x03")
    pump(2)
os.write(fd, b"\x04")
pump(2)
s = re.sub(rb"\x1b(\[[0-9;?]*[A-Za-z]|[78])", b"", out).decode(errors="replace")
print(s)
try:
    os.kill(pid, 9)
except Exception:
    pass
PY
