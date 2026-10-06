#!/usr/bin/env python3
# tui-core-01: input_handler pairs any handler emission with the in-flight
# event: keys are lost or leaked.
#
# InputHandlerW::handle_update (stdlib/graphix-package-tui/src/input_handler.rs
# 448-460) takes every update of the handler callable's expr as the reply to
# queued.front(). A select also emits when a consulted guard's input fires
# (CLAUDE.md, organic firing), so `kk@ `Up if sel > 0 => { sel <- (kk ~ sel)
# - 1; `Stop }` answers Stop again one cycle after every Up, when `sel`
# lands, and that verdict is applied to the next queued key.
#
# The script runs three programs in a pseudo-terminal (24x100) and prints
# the first screen line after each phase:
#   canonical: outer = the handler above with `_ => `Continue`; inner
#              counts 'x'. Phases: 'x' alone, 10 times; Up and 'x' in one
#              write (type-ahead, a paste), 20 times; Up, 100 ms, 'x', 10
#              times.
#   noguard:   the same outer handler without `if sel > 0` (control).
#   mirror:    outer answers Stop to 'a' and Continue to everything else
#              through a guard on a 2 ms timer; inner counts the 'a' keys
#              that leak and the 'b' keys. Phase: "ba" in one write, 60
#              times.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/tui-core-01.py <graphix> [--no-fusion]
#
# expected: XS counts every 'x' (the outer handler answers Continue to
#           'x'); LEAKED stays 0 (the outer handler answers Stop to 'a').
# observed (HEAD c722befe, debug build, --no-fusion):
#   canonical startup                    SEL=1000 XS=0 END
#   canonical after x alone x10          SEL=1000 XS=10 END
#   canonical after Up+x burst x20       SEL=980 XS=10 END     <- 0 of 20 'x' arrive
#   canonical after Up,100ms,x x10       SEL=970 XS=20 END     <- 10 of 10
#   noguard   after Up+x burst x20       SEL=980 XS=20 END     <- control: 20 of 20
#   mirror    after "ba" burst x60       LEAKED=7 BS=60 END    <- 7 Stopped 'a' leak
# With fusion on, the same run printed XS=12 after the burst (2 of 20) and
# LEAKED=37 BS=57 for the mirror (3 'b' lost as well). The counts move with
# machine load; every Up is handled in every run (SEL drops by one per Up).
import os, pty, re, select, shutil, signal, struct, sys, tempfile, time, fcntl, termios

HEAD = '''use tui::{input_handler::{Event, input_handler, on_press}, paragraph::paragraph};
'''
CANONICAL = HEAD + '''let sel = 1000;
let xs = 0;
let outer = |e: Event| on_press(e, |k| select k.code {
  kk@ `Up if sel > 0 => { sel <- (kk ~ sel) - 1; `Stop },
  _ => `Continue
});
let inner = |e: Event| on_press(e, |k| select k.code {
  c@ `Char("x") => { xs <- c ~ xs + 1; `Stop },
  _ => `Stop
});
input_handler(#handle: &outer, &input_handler(#handle: &inner, &paragraph(&"SEL=[sel] XS=[xs] END")))
'''
NOGUARD = CANONICAL.replace(" if sel > 0", "")
MIRROR = HEAD + '''let flip = false;
flip <- sys::time::timer(duration:2.ms, true) ~ !flip;
let leaked = 0;
let bs = 0;
let outer = |e: Event| on_press(e, |k| select k.code {
  `Char("a") => `Stop,
  _ if flip => `Continue,
  _ => `Continue
});
let inner = |e: Event| on_press(e, |k| select k.code {
  c@ `Char("a") => { leaked <- c ~ leaked + 1; `Stop },
  c@ `Char("b") => { bs <- c ~ bs + 1; `Stop },
  _ => `Stop
});
input_handler(#handle: &outer, &input_handler(#handle: &inner, &paragraph(&"LEAKED=[leaked] BS=[bs] END")))
'''
UP = b"\x1b[A"
graphix = sys.argv[1] if len(sys.argv) > 1 else "graphix"
extra = sys.argv[2:]
ROWS, COLS = 24, 100
tmp = tempfile.mkdtemp(prefix="tui-core-01-")


def run(name, program, phases):
    prog = os.path.join(tmp, name + ".gx")
    open(prog, "w").write(program)
    pid, fd = pty.fork()
    if pid == 0:
        fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", ROWS, COLS, 0, 0))
        e = os.open(prog + ".stderr", os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o644)
        os.dup2(e, 2)
        os.environ["TERM"] = "xterm-256color"
        os.execvp(graphix, [graphix, "--no-cache"] + extra + [prog])
    buf = bytearray()
    grid = [[" "] * COLS for _ in range(ROWS)]
    cur = [0, 0]
    fed = [0]

    def pump(secs, until=None):
        end = time.time() + secs
        while time.time() < end:
            r, _, _ = select.select([fd], [], [], 0.01)
            if r:
                try:
                    d = os.read(fd, 65536)
                except OSError:
                    return
                if not d:
                    return
                buf.extend(d)
                if b"\x1b[6n" in d:
                    os.write(fd, b"\x1b[1;1R")
                if until is not None and until in buf:
                    return

    # just enough of a screen for ratatui's cell diffs: CSI H moves and text
    def line():
        s = bytes(buf[fed[0]:]).decode("utf8", "replace")
        fed[0] = len(buf)
        i = 0
        while i < len(s):
            c = s[i]
            if c == "\x1b":
                m = re.match(r"\x1b\[([0-9;?]*)([A-Za-z])", s[i:])
                if m:
                    if m.group(2) == "H":
                        ps = [int(x) if x else 1 for x in m.group(1).split(";")] if m.group(1) else [1, 1]
                        ps += [1] * (2 - len(ps))
                        cur[0], cur[1] = ps[0] - 1, ps[1] - 1
                    i += len(m.group(0))
                else:
                    i += 1
                continue
            if c == "\r":
                cur[1] = 0
            elif c == "\n":
                cur[0] = min(cur[0] + 1, ROWS - 1)
            elif c >= " ":
                if 0 <= cur[0] < ROWS and 0 <= cur[1] < COLS:
                    grid[cur[0]][cur[1]] = c
                cur[1] += 1
            i += 1
        return "".join(grid[0]).rstrip()

    try:
        pump(120, b"END")
        pump(1.5)
        print("%-9s %-26s %s" % (name, "startup", line()), flush=True)
        for label, keys, count, gap in phases:
            for _ in range(count):
                for k in keys:
                    if isinstance(k, bytes):
                        os.write(fd, k)
                    else:
                        pump(k)
                pump(gap)
            pump(1.5)
            print("%-9s %-26s %s" % (name, "after %s x%d" % (label, count), line()), flush=True)
        os.write(fd, b"\x03")
        pump(2.0)
    finally:
        try:
            os.killpg(pid, signal.SIGKILL)
        except Exception:
            pass
        try:
            os.waitpid(pid, 0)
        except Exception:
            pass


# a phase's keys: bytes are written, a number between them is a pause (s)
run("canonical", CANONICAL, [
    ("x alone", [b"x"], 10, 0.15),
    ("Up+x burst", [UP + b"x"], 20, 0.2),
    ("Up,100ms,x", [UP, 0.1, b"x"], 10, 0.2),
])
run("noguard", NOGUARD, [("Up+x burst", [UP + b"x"], 20, 0.2)])
run("mirror", MIRROR, [('"ba" burst', [b"ba"], 60, 0.1)])
shutil.rmtree(tmp, ignore_errors=True)
