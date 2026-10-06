#!/usr/bin/env python3
# rt-06: an interrupted derivation is not re-fired. GXHandle::interrupt's
# doc (graphix-rt/src/lib.rs:654-655) and design/atomic_recursion.md:84-85
# say the aborted cycle "rides its last result and re-fires next cycle";
# nothing re-schedules an interrupted root (GXLambda::update and
# FusedKernel ride their resident, do_cycle clears `updated`), so a
# one-shot trigger's result is lost until an input fires again.
#
# command: timeout -s KILL 60 python3 design/review-2026-10-05/repro/rt-06.py <graphix>
#          (drives `<graphix> --no-cache` as a REPL under a pty, twice: with
#          a Ctrl-C ~1 s into a ~2.7 s fused tail loop, and without)
#
# expected (per the doc): the interrupted sum re-runs next cycle, so `r`
#          later prints 80000000200000000 in both runs.
# observed (HEAD c722befe, debug build):
#   interrupted: `r` prints `-: i64` and no value, 6 s after the Ctrl-C
#   control:     `r` prints `-: i64` then 80000000200000000
import os, pty, re, select, signal, struct, sys, termios, fcntl, time

G = sys.argv[1] if len(sys.argv) > 1 else "graphix"
SUM = "let rec sum = |acc: i64, n: i64| -> i64 select n { 0 => acc, n => sum(acc + n, n - 1) }"
GO = "let go = sys::time::timer(duration:300.ms, false); let r = sum(0, go ~ 400000000)"
RUNS = {
    "interrupted": [("send", SUM), ("wait", 1), ("send", GO), ("wait", 1.0),
                    ("raw", "\x03"), ("wait", 6), ("send", "r"), ("wait", 3)],
    "control": [("send", SUM), ("wait", 1), ("send", GO), ("wait", 7),
                ("send", "r"), ("wait", 3)],
}
ANSI = re.compile(rb"\x1b\[[0-9;?]*[A-Za-z]|\x1b\][^\x07]*\x07|\x1b[=>]|\r")


def run(steps):
    pid, fd = pty.fork()
    if pid == 0:
        os.environ["TERM"] = "xterm"
        os.execvp(G, [G, "--no-cache"])
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 40, 160, 0, 0))
    out = []

    def pump(secs):
        end = time.time() + secs
        while (left := end - time.time()) > 0:
            r, _, _ = select.select([fd], [], [], left)
            if fd in r:
                try:
                    b = os.read(fd, 65536)
                except OSError:
                    return
                if not b:
                    return
                # answer the line editor's cursor-position queries
                for _ in range(b.count(b"\x1b[6n")):
                    os.write(fd, b"\x1b[1;1R")
                out.append(b)

    pump(3)
    for kind, arg in steps:
        if kind == "send":
            os.write(fd, arg.encode() + b"\r")
            pump(0.3)
        elif kind == "raw":
            os.write(fd, arg.encode())
            pump(0.3)
        else:
            pump(arg)
    try:
        os.killpg(pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    os.waitpid(pid, 0)
    text = ANSI.sub(b"", b"".join(out)).decode(errors="replace")
    return [l for l in text.splitlines() if l.strip()]


for name, steps in RUNS.items():
    print(f"== {name}")
    for line in run(steps)[-3:]:
        print(line)
