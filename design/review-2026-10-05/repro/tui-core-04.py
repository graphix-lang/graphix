#!/usr/bin/env python3
# tui-core-04: a handler call that never replies (a raise or a bottom in the
# reply) stops the input_handler for good, and every later event is queued
# and kept.
#
# InputHandlerW (stdlib/graphix-package-tui/src/input_handler.rs:399-465)
# sends one event at a time: maybe_send_queued calls the handler only while
# `pending` is false and sets it; only an update of the handler's call site
# (the reply, 448-451) or `#enabled` going false (439-443) clears it. A
# handler whose reply is bottom produces no update (graphix-rt/src/gx.rs:369-
# 373 reports FIRED productions only), so `pending` stays true and
# handle_event (420-424) pushes every later event onto `queued`, which nothing
# drains. The handler type admits a raise (`throws 'e`, input_handler.gxi:134),
# and an unhandled `?` bottoms; a `$` over a null bottoms without a word.
#
# The script runs each program below in a pseudo-terminal, presses a, a, x,
# m, a, b, then Ctrl-C, and prints the screen after each key and the
# program's stderr trace: what the terminal delivered (tui::event) and what
# the handler was called with.
#   RAISE:  the x arm's reply is error(c ~ `Boom)?
#   BOTTOM: the x arm's reply is (c ~ mode)$ with mode still null
# With --mem it then presses x in RAISE and sends 4 bursts of 20000 'a' (4000
# a second), and the same bursts to a program with no input_handler, printing
# the graphix process's VmRSS after each burst.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/tui-core-04.py <graphix> [--mem]
#          (put --no-fusion after <graphix> for the node-walk: same result)
#
# expected: the handler is called for all six keys and the count ends at 5
#   (every key but x counts): an unanswered key costs that key only. With
#   --mem, RSS after the x levels off like the baseline's.
# observed (HEAD c722befe, debug build, JIT and --no-fusion alike):
#   RAISE:  count 1, 2, then 2 forever; stderr shows "terminal delivered" for
#           all six keys but "handler called" only for a, a, x, and
#           'unhandled error ... "Boom"' (on the TUI screen when stderr is the
#           terminal).
#   BOTTOM: the same freeze at 2, and nothing is logged.
#   --mem:  RAISE after x 62 -> 84 -> 105 -> 126 -> 147 MB (about 1 KB per
#           queued event, linear); no input_handler 58 -> 59 -> 59 -> 59 -> 59 MB.
#   Only Ctrl-C (taken in Rust, tui/src/lib.rs:854) still works.
import fcntl, os, pty, re, select, shutil, signal, struct, sys, tempfile, termios, time

TRACE = '''println(#dest: `Stderr, "terminal delivered [select tui::event { `Key(k) => k.code, _ => `Other }]");
println(#dest: `Stderr, "    count [n]");
'''

RAISE = '''use tui::{paragraph::paragraph, input_handler::{Event, input_handler, on_press}};
let n = 0;
''' + TRACE + '''let handle = |e: Event| {
  println(#dest: `Stderr, "  handler called with [select e { `Key(k) => k.code, _ => `Other }]");
  on_press(e, |k| select k.code {
    c@ `Char("x") => error(c ~ `Boom)?,
    c@ `Char(_) => { n <- c ~ n + 1; `Stop },
    _ => `Continue
  })
};
input_handler(#handle: &handle, &paragraph(&"count: [n]"))
'''

BOTTOM = '''use tui::{paragraph::paragraph, input_handler::{Event, input_handler, on_press}};
let n = 0;
let mode: [`Stop, `Continue, null] = null;
''' + TRACE + '''let handle = |e: Event| {
  println(#dest: `Stderr, "  handler called with [select e { `Key(k) => k.code, _ => `Other }]");
  on_press(e, |k| select k.code {
    c@ `Char("x") => (c ~ mode)$,
    c@ `Char("m") => { mode <- c ~ `Stop; n <- c ~ n + 1; `Stop },
    c@ `Char(_) => { n <- c ~ n + 1; `Stop },
    _ => `Continue
  })
};
input_handler(#handle: &handle, &paragraph(&"count: [n]"))
'''

NO_HANDLER = '''use tui::paragraph::paragraph;
paragraph(&"count: 0")
'''

args = [a for a in sys.argv[1:] if a != "--mem"]
mem = "--mem" in sys.argv[1:]
graphix = args[0] if args else "graphix"
extra = args[1:]
ROWS, COLS = 24, 80
tmp = tempfile.mkdtemp(prefix="tui-core-04-")


class Run:
    def __init__(self, name, program):
        self.prog = os.path.join(tmp, name + ".gx")
        self.errf = os.path.join(tmp, name + ".stderr")
        open(self.prog, "w").write(program)
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", ROWS, COLS, 0, 0))
            e = os.open(self.errf, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o644)
            os.dup2(e, 2)
            os.environ["TERM"] = "xterm-256color"
            os.execvp(graphix, [graphix, "--no-cache"] + extra + [self.prog])
        self.buf = bytearray()
        self.fed = 0
        self.grid = [[" "] * COLS for _ in range(ROWS)]
        self.cur = [0, 0]

    def pump(self, secs, until=None):
        end = time.time() + secs
        while time.time() < end:
            r, _, _ = select.select([self.fd], [], [], 0.005)
            if r:
                try:
                    d = os.read(self.fd, 1 << 20)
                except OSError:
                    return
                if not d:
                    return
                self.buf.extend(d)
                if b"\x1b[6n" in d:
                    os.write(self.fd, b"\x1b[1;1R")
                if until is not None and until in self.buf:
                    return

    # just enough of a terminal for ratatui's cell diffs: CSI row;col H,
    # CSI 2J, printable characters
    def screen(self):
        s = bytes(self.buf[self.fed:]).decode("utf8", "replace")
        self.fed = len(self.buf)
        i = 0
        while i < len(s):
            c = s[i]
            if c == "\x1b":
                m = re.match(r"\x1b\[([0-9;?]*)([A-Za-z])", s[i:])
                if m:
                    if m.group(2) == "H":
                        ps = [int(x) if x else 1 for x in m.group(1).split(";")] if m.group(1) else [1, 1]
                        ps += [1] * (2 - len(ps))
                        self.cur[0], self.cur[1] = ps[0] - 1, ps[1] - 1
                    elif m.group(2) == "J" and m.group(1) in ("2", ""):
                        self.grid = [[" "] * COLS for _ in range(ROWS)]
                    i += len(m.group(0))
                else:
                    i += 1
                continue
            if c == "\r":
                self.cur[1] = 0
            elif c == "\n":
                self.cur[0] = min(self.cur[0] + 1, ROWS - 1)
            elif c >= " ":
                if 0 <= self.cur[0] < ROWS and 0 <= self.cur[1] < COLS:
                    self.grid[self.cur[0]][self.cur[1]] = c
                self.cur[1] += 1
            i += 1
        return "".join(self.grid[0]).rstrip()

    def rss_mb(self):
        kids = {}
        for p in os.listdir("/proc"):
            if p.isdigit():
                try:
                    s = open("/proc/%s/stat" % p).read()
                except OSError:
                    continue
                kids.setdefault(int(s[s.rindex(")") + 2:].split()[1]), []).append(int(p))
        best, todo = 0, [self.pid]
        while todo:
            for c in kids.get(todo.pop(), []):
                todo.append(c)
                try:
                    for l in open("/proc/%d/status" % c):
                        if l.startswith("VmRSS"):
                            best = max(best, int(l.split()[1]) // 1024)
                except OSError:
                    pass
        return best

    def stop(self):
        try:
            os.write(self.fd, b"\x03")
            self.pump(2.0)
        finally:
            try:
                os.killpg(self.pid, signal.SIGKILL)
            except Exception:
                pass
            try:
                os.waitpid(self.pid, 0)
            except Exception:
                pass


def keys(name, program):
    print("== %s" % name)
    r = Run(name, program)
    try:
        r.pump(150, b"count:")
        r.pump(1.0)
        print("  startup: screen %r" % r.screen())
        for k in "aaxmab":
            os.write(r.fd, k.encode())
            r.pump(0.7)
            print("  after key %r: screen %r" % (k, r.screen()))
    finally:
        r.stop()
    print("  stderr:")
    for line in open(r.errf, errors="replace"):
        if "WARNING" not in line:
            print("    " + line.rstrip()[:160])


def bursts(name, program, prefix):
    r = Run(name, program)
    try:
        r.pump(150, b"count:")
        r.pump(2.0)
        for k in prefix:
            os.write(r.fd, k.encode())
            r.pump(0.7)
        out = ["%d" % r.rss_mb()]
        for _ in range(4):
            for _ in range(100):
                os.write(r.fd, b"a" * 200)
                r.pump(0.05)
            r.pump(3.0)
            out.append("%d" % r.rss_mb())
        print("== %s, screen %r, VmRSS MB per burst of 20000 keys: %s" % (name, r.screen(), " -> ".join(out)))
    finally:
        r.stop()


try:
    keys("RAISE", RAISE)
    keys("BOTTOM", BOTTOM)
    if mem:
        bursts("RAISE after x", RAISE, "ax")
        bursts("no input_handler", NO_HANDLER, "a")
finally:
    shutil.rmtree(tmp, ignore_errors=True)
