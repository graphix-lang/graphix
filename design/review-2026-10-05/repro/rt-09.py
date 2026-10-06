#!/usr/bin/env python3
# rt-09: is_output_kind (graphix-rt/src/gx.rs:151-162) omits ExprKind::Trait
# and ExprKind::Impl, so a REPL trait or impl declaration is compiled as an
# output expression: the shell prints `-: _`, moves the CompExp into its
# output and waits for Ctrl-C, swallowing every typed line; the Ctrl-C that
# ends the wait drops the CompExp, which deletes the declaration.
#
# command: timeout -s KILL 60 python3 design/review-2026-10-05/repro/rt-09.py <graphix>
#          (drives `<graphix> --no-cache` as a REPL under a pty)
#
# expected: `trait Shw {..}` is a declaration like `let qq = 1`: no `-: _`,
#           the next line `40 + 2` prints 42, and a later `impl Shw for i64`
#           finds the trait.
# observed (HEAD c722befe, debug build):
#   〉trait Shw { val shw: fn(self) -> string }
#   -: _
#   40 + 2                       <- echoed by the tty, never evaluated
#   ^C〉impl Shw for i64 { let shw = |x| "i[x]" }
#   error: ... no trait `Shw` in scope
#   (control: after `let qq = 1`, `40 + 2` prints 42 at once; and with
#   `trait Shw {..}; let zz = 1` then `impl Shw for i64 {..}; let zz2 = 2`,
#   lines that end in a let and so stay declarations, `Shw::shw(5)` prints
#   "i5")
import os, pty, re, select, signal, struct, sys, termios, fcntl, time

G = sys.argv[1] if len(sys.argv) > 1 else "graphix"
STEPS = [
    ("send", "trait Shw { val shw: fn(self) -> string }"), ("wait", 2),
    ("send", "40 + 2"), ("wait", 2),
    ("raw", "\x03"), ("wait", 1),
    ("send", 'impl Shw for i64 { let shw = |x| "i[x]" }'), ("wait", 2),
]

pid, fd = pty.fork()
if pid == 0:
    os.environ["TERM"] = "xterm"
    os.execvp(G, [G, "--no-cache"])
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 40, 160, 0, 0))
ANSI = re.compile(rb"\x1b\[[0-9;?]*[A-Za-z]|\x1b\][^\x07]*\x07|\x1b[=>]|\r")
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
for kind, arg in STEPS:
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
print(ANSI.sub(b"", b"".join(out)).decode(errors="replace"))
