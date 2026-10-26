#!/usr/bin/env python3
# Title: a layout child whose constraint changes is rebuilt whole, and an
#        event its input handler passed on (`Continue) is lost with it.
# Command: timeout -s KILL 90 python3 -I design/review-2026-10-26/repro/tui-core-layout-rebuild-loses-event.py
# Expected: both runs end "(2, 4)": keys a, w, a, w; the outer handler
#           answers `Continue to all of them, so the inner one counts 4.
# Observed (HEAD e6ffc6eed): constant constraint -> (2, 4);
#           constraint `Length(n + 3) -> (2, 2): each `w` changes n, the
#           Child value changes, reconcile compiles a fresh Slot, and the
#           old inner handler's queued call is cancelled with its Callable.
import os, pty, re, select, time, tempfile, fcntl, struct, termios

GX = os.environ.get("GX", "/home/eric/tmp/review-2026-10-26/bin/graphix")
PROG = r'''use tui::{
  input_handler::{Event, input_handler, on_press},
  layout::{child, layout},
  text::text
};

let n = 0;
let seen = 0;

let outer = |e: Event| on_press(e, |k| select k.code {
  q@ `Char("q") => {
    tui::exit(q ~ null);
    `Stop
  },
  c@ `Char("w") => {
    n <- c ~ n + 1;
    `Continue
  },
  _ => `Continue
});

let inner = |e: Event| on_press(e, |k| {
  seen <- k ~ seen + 1;
  `Stop
});

let children = [child(
  #constraint: CONSTRAINT,
  input_handler(#handle: &outer, &input_handler(#handle: &inner, &text(&"n=[n] seen=[seen]")))
)];

dbg((n, seen));
layout(&children)
'''

def run(path):
    pid, fd = pty.fork()
    if pid == 0:
        os.environ["TERM"] = "xterm-256color"
        os.execv(GX, [GX, "--no-cache", path])
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 10, 40, 0, 0))
    out, t0 = bytearray(), time.time()
    keys = [(1.2 * (i + 1), k) for i, k in enumerate([b"a", b"w", b"a", b"w", b"q"])]
    while time.time() - t0 < 12:
        if keys and time.time() - t0 >= keys[0][0]:
            os.write(fd, keys.pop(0)[1])
        if select.select([fd], [], [], 0.05)[0]:
            try:
                b = os.read(fd, 65536)
            except OSError:
                break
            if not b:
                break
            out += b
    try:
        os.kill(pid, 9)
    except ProcessLookupError:
        pass
    os.waitpid(pid, 0)
    return re.findall(r"dbg\(\(n, seen\)\): (\(\d+, \d+\))", out.decode("utf-8", "replace"))[-1]

with tempfile.TemporaryDirectory() as d:
    for constraint in ["`Length(3)", "`Length(n + 3)"]:
        path = os.path.join(d, "lost.gx")
        open(path, "w").write(PROG.replace("CONSTRAINT", constraint))
        print(constraint, "->", run(path))
