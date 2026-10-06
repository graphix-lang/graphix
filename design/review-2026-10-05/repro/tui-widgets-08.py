#!/usr/bin/env python3
# tui-widgets-08: list: the requested selection (and scroll) is lost when the
# items are empty or shorter, and never re-applied.
#
# ListW writes `selected`/`scroll` into its ListState only at compile and
# when those refs update (stdlib/graphix-package-tui/src/list.rs:86-91,
# 121-126); draw ignores them (list.rs:138-139). ratatui's List render
# rewrites the state: an empty list selects None and resets the offset to 0,
# an out-of-range selection is clamped to the last item
# (ratatui-widgets-0.3.0/src/list/rendering.rs:42-50). The program's
# `selected` is unchanged, so the widget keeps showing the stale state.
#
# The script runs each program below in a pseudo-terminal and prints the
# screen at the given times (seconds after the first output byte).
#
# command: timeout -s KILL 120 python3 design/review-2026-10-05/repro/tui-widgets-08.py <graphix>
#
# expected: control and async show ">>Alpha"; shrink shows ">>r8" before and
#           after the shrink-and-regrow (">>r2" while only three rows exist);
#           scroll starts at r5 in both runs.
# observed (HEAD c722befe, debug build):
#   control  @1.5: ">>Alpha" on row 0
#   async    @2.0: "Alpha", "Beta", "Gamma" with no ">>" and no highlight column
#   shrink   @1.5: ">>r8" | @3.0: ">>r2" (3 rows) | @5.5: ">>r2" while selected is 8
#   scroll   @2.0: rows r0..r9 (offset 0), where `#scroll: &5` with the items
#                  present from the start shows r5..r9
import os, pty, re, select, signal, struct, sys, time, fcntl, termios, tempfile, shutil

HEAD = 'use tui::{Line, line, list::list};\n'
TEN = ('let ten: Array<Line> = [line("r0"), line("r1"), line("r2"), line("r3"), '
       'line("r4"), line("r5"), line("r6"), line("r7"), line("r8"), line("r9")];\n')
PROGRAMS = [
    ("control", [1.5], HEAD + '''let sel = 0;
let items: Array<Line> = [line("Alpha"), line("Beta"), line("Gamma")];
list(#highlight_symbol: &">>", #selected: &sel, &items)
'''),
    ("async", [2.0], HEAD + '''let sel = 0;
let items: Array<Line> = [];
items <- sys::time::after_idle(duration:500.ms, [line("Alpha"), line("Beta"), line("Gamma")]);
list(#highlight_symbol: &">>", #selected: &sel, &items)
'''),
    ("shrink", [1.5, 3.0, 5.5], HEAD + TEN + '''let items = ten;
items <- sys::time::after_idle(duration:2.s, [line("r0"), line("r1"), line("r2")]);
items <- sys::time::after_idle(duration:4.s, ten);
list(#highlight_symbol: &">>", #selected: &8, &items)
'''),
    ("scroll", [2.0], HEAD + TEN + '''let items: Array<Line> = [];
items <- sys::time::after_idle(duration:500.ms, ten);
list(#scroll: &5, &items)
'''),
]
ROWS, COLS = 24, 80
graphix = sys.argv[1] if len(sys.argv) > 1 else "graphix"
tmp = tempfile.mkdtemp(prefix="tui-widgets-08-")
os.environ["XDG_CACHE_HOME"] = tmp


def run(name, times, src):
    prog = os.path.join(tmp, name + ".gx")
    open(prog, "w").write(src)
    errf = prog + ".stderr"
    pid, fd = pty.fork()
    if pid == 0:
        fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", ROWS, COLS, 0, 0))
        os.dup2(os.open(errf, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o644), 2)
        os.environ["TERM"] = "xterm-256color"
        os.execvp(graphix, [graphix, "--no-cache", prog])
    grid = [[" "] * COLS for _ in range(ROWS)]
    cur = [0, 0]

    def feed(d):
        s = d.decode("utf8", "replace")
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

    def show(t):
        rows = ["".join(r).rstrip() for r in grid]
        n = max([i for i, r in enumerate(rows) if r] + [-1]) + 1
        print("%-8s @%.1fs: %r" % (name, t, rows[:n]))

    start, ti = None, 0
    try:
        deadline = time.time() + 60
        while ti < len(times) and time.time() < deadline:
            if start is not None and time.time() - start >= times[ti]:
                show(times[ti])
                ti += 1
                continue
            r, _, _ = select.select([fd], [], [], 0.02)
            if r:
                try:
                    d = os.read(fd, 65536)
                except OSError:
                    break
                if not d:
                    break
                start = start or time.time()
                if b"\x1b[6n" in d:
                    os.write(fd, b"\x1b[1;1R")
                feed(d)
    finally:
        for k in (lambda: os.killpg(pid, signal.SIGKILL), lambda: os.kill(pid, signal.SIGKILL),
                  lambda: os.waitpid(pid, 0)):
            try:
                k()
            except Exception:
                pass
    err = open(errf, errors="replace").read().strip()
    if err:
        print("  stderr: " + err[:400])


try:
    for name, times, src in PROGRAMS:
        run(name, times, src)
finally:
    shutil.rmtree(tmp, ignore_errors=True)
