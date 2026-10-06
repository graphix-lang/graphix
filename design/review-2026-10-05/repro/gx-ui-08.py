#!/usr/bin/env python3
# gx-ui-08: input_handler declares `throws 'e`, but an error the handler
# raises never reaches a catch around input_handler(..).
#
# tui::input_handler::input_handler (stdlib/graphix-package-tui/src/graphix/
# input_handler.gx:1-5, input_handler.gxi:132-136) takes
# `#handle: &fn(e: Event) -> .. throws 'e` and returns `Tui throws 'e`, so
# the checker sends the handler's errors to the catch at the
# input_handler call. A catch there must accept them
# (`catch(e: Error<ErrChain<`Other(string)>>)` is refused: "does not
# contain Error<ErrChain<`Boom(string)>>"). With no catch, the checker warns
# "raised from function call input_handler will not be caught". At run
# time the TUI calls the handler through a Callable whose call site is
# compiled under Scope::root() (graphix-rt/src/gx.rs:944). An instance takes
# its call site's handlers (graphix-compiler/src/node/lambda.rs:1218), so
# a `?` in the handler finds no handler and is logged "unhandled error",
# even with a file-level catch.
#
# The script runs the program below in a pseudo-terminal and presses
# y, x, z, x, then Ctrl-C. On `x` the TUI calls the handler, which raises
# `Boom. On `z` the handler writes `replay`, and the program calls
# h(replay) itself under the same catch: the same raise, from a Graphix call.
#
# command: timeout -s KILL 120 python3 design/review-2026-10-05/repro/gx-ui-08.py <graphix>
#          (put --no-fusion after <graphix> for the node-walk; it gives the same result)
#
# expected: CAUGHT counts the x presses (the checker says their errors reach it).
# observed (HEAD c722befe, debug build, JIT and --no-fusion alike):
#          startup: screen line 1 = 'CAUGHT=0 KEYS=0'
#    after key 'y': screen line 1 = 'CAUGHT=0 KEYS=1'
#    after key 'x': screen line 1 = 'CAUGHT=0 KEYS=1'
#    after key 'z': screen line 1 = 'CAUGHT=1 KEYS=1'
#    after key 'x': screen line 1 = 'CAUGHT=1 KEYS=1'
#   stderr: 'unhandled error in file ... at line: 6, column: 19 ["Boom", "x pressed"]'
#   once per x press. When the TUI compiles the handler's call, it also
#   prints "WARNING: ... error raised by ? will not be caught".
import os, pty, re, select, shutil, signal, struct, sys, tempfile, time, fcntl, termios

PROGRAM = r'''use tui::{input_handler::{Event, input_handler, on_press}, paragraph::paragraph};
let caught = 0;
let keys = 0;
let replay: Event = never();
let h = |e: Event| on_press(e, |k| select k.code {
  `Char("x") => { error(`Boom("x pressed"))?; `Stop },
  `Char("z") => { replay <- k ~ `Key({k with code: `Char("x")}); `Stop },
  kk => { keys <- kk ~ keys + 1; `Continue }
});
let msg = "CAUGHT=[caught] KEYS=[keys]";
{
  catch(e) caught <- e ~ caught + 1;
  h(replay);
  input_handler(#handle: &h, &paragraph(&msg))
}
'''

graphix = sys.argv[1] if len(sys.argv) > 1 else "graphix"
extra = sys.argv[2:]
tmp = tempfile.mkdtemp(prefix="gx-ui-08-")
prog = os.path.join(tmp, "prog.gx")
errf = os.path.join(tmp, "stderr.txt")
open(prog, "w").write(PROGRAM)
ROWS, COLS = 24, 100

pid, fd = pty.fork()
if pid == 0:
    fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", ROWS, COLS, 0, 0))
    e = os.open(errf, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o644)
    os.dup2(e, 2)
    os.environ["TERM"] = "xterm-256color"
    os.execvp(graphix, [graphix, "--no-cache"] + extra + [prog])

buf = bytearray()


def pump(secs, until=None):
    end = time.time() + secs
    while time.time() < end:
        r, _, _ = select.select([fd], [], [], 0.1)
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


marks = []
try:
    pump(90, b"CAUGHT=")
    pump(1.5)
    for key in [b"y", b"x", b"z", b"x"]:
        marks.append((len(buf), key.decode()))
        os.write(fd, key)
        pump(2.5)
    marks.append((len(buf), "^C"))
    os.write(fd, b"\x03")
    pump(3.0)
finally:
    try:
        os.killpg(pid, signal.SIGKILL)
    except Exception:
        pass
    try:
        os.waitpid(pid, 0)
    except Exception:
        pass

# A screen model just good enough for ratatui's cell diffs: cursor moves
# (CSI row;col H), printable characters, everything else ignored.
grid = [[" "] * COLS for _ in range(ROWS)]
cur = [0, 0]


def feed(chunk):
    s = chunk.decode("utf8", "replace")
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


raw = bytes(buf)
cuts = [0] + [m[0] for m in marks] + [len(raw)]
names = ["startup"] + ["after key %r" % m[1] for m in marks]
for i, name in enumerate(names):
    feed(raw[cuts[i]:cuts[i + 1]])
    print("%16s: screen line 1 = %r" % (name, "".join(grid[0]).rstrip()))
print("stderr:")
for line in open(errf, errors="replace"):
    print("  " + line.rstrip()[:200])
shutil.rmtree(tmp, ignore_errors=True)
