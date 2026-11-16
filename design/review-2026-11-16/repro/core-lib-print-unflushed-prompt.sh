#!/bin/bash
# Title: print() without a trailing newline stays in stdout's line buffer:
#        a prompt is not shown until a later newline or the exit.
# Command: bash design/review-2026-11-16/repro/core-lib-print-unflushed-prompt.sh
# Expected: "name? " arrives at ~0.0s, before the input is sent at 1.5s.
# Observed: 1.50s: sending input
#           1.50s stdout: b'name? hello eric\n'
#           (the prompt arrives only with the reply's newline)
set -e
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > "$D/prompt.gx" <<'GX'
use sys::io;
print("name? ");
let line = io::Lines::lines(io::stdin(null))$;
println(line ~ "hello [line]");
sys::exit(sys::time::after_idle(duration:100.ms, line ~ 0));
never()
GX
cat > "$D/drive.py" <<'PY'
import subprocess, sys, time, os, select
p = subprocess.Popen(sys.argv[1:], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
start = time.time(); fd = p.stdout.fileno(); sent = False
while True:
    r, _, _ = select.select([fd], [], [], 0.1)
    if r:
        b = os.read(fd, 100)
        if not b: break
        print(f"{time.time()-start:.2f}s stdout: {b!r}")
    if not sent and time.time() - start > 1.5:
        print(f"{time.time()-start:.2f}s: sending input")
        p.stdin.write(b"eric\n"); p.stdin.flush(); sent = True
p.wait()
PY
timeout -s KILL 30 python3 -I "$D/drive.py" "$G" --no-cache "$D/prompt.gx"
rm -rf "$D"
