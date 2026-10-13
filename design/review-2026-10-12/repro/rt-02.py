#!/usr/bin/env python3
# rt-02 (review 2026-10-12): GX::compile (graphix-rt/src/gx.rs:833-876)
# installs the statements before a failing one and advances self.scope
# past them, then returns Err and drops their CompExps, whose Drop sends
# ToGX::Delete: the nodes are deleted, the scope is not rolled back.
#
# command: GRAPHIX=/path/to/graphix python3 rt-02.py
#
# expected: either the earlier statements stay installed (`caught ..`,
#   `x + 1` prints 42), or nothing of the input is left (the `?` warns
#   unhandled, `x` is not defined).
# observed (HEAD 7324207f, debug build):
#   >>> catch(e) println("caught [e]"); let w = nosuch   error: nosuch not defined
#   >>> error(`Boom)?     error: BUG: catch BindId(..) has no inferred bind
#   (every later `?` in the session is refused the same way)
#   >>> let x = 41; let y = nosuch   error: nosuch not defined
#   >>> x + 1             error: x not defined   (not "installed")
# control: the same statements entered one per line give `x + 1` = 42
#   and a `?` that reaches the catch (no BUG refusal).
import fcntl, os, pty, re, select, signal, struct, sys, tempfile, termios, time

GRAPHIX = os.environ.get("GRAPHIX", "graphix")
ARGS = ["--no-cache", "--no-netidx", "--no-init"] + sys.argv[1:]
WAIT = 4.0
LINES = [
    'catch(e) println("caught [e]"); let w = nosuch',
    "error(`Boom)?",
    "^C",
    "let x = 41; let y = nosuch",
    "x + 1",
    "^C",
]

os.environ["XDG_CACHE_HOME"] = tempfile.mkdtemp()
pid, fd = pty.fork()
if pid == 0:
    os.execvp(GRAPHIX, [GRAPHIX] + ARGS)
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 50, 200, 0, 0))
raw = b""


def pump(seconds):
    global raw
    end = time.time() + seconds
    while time.time() < end:
        r, _, _ = select.select([fd], [], [], 0.05)
        if fd not in r:
            continue
        try:
            data = os.read(fd, 65536)
        except OSError:
            return
        if not data:
            return
        raw += data
        # the line editor asks the terminal where the cursor is
        for _ in range(data.count(b"\x1b[6n")):
            os.write(fd, b"\x1b[1;1R")


pump(WAIT + 2)
for line in LINES:
    raw += ("\n>>> " + line + "\n").encode()
    os.write(fd, b"\x03" if line == "^C" else line.encode() + b"\r")
    pump(WAIT)
os.write(fd, b"\x04")
pump(1.5)
try:
    os.kill(pid, signal.SIGKILL)
except ProcessLookupError:
    pass
os.waitpid(pid, 0)

text = raw.decode("utf-8", "replace")
text = re.sub(r"\x1b\[[0-9;?]*[ -/]*[@-~]", "", text)
text = re.sub(r"\x1b[()][0-9A-Za-z]|\x1b[=>78]", "", text).replace("\r", "")
for l in text.split("\n"):
    if l.strip() and l.strip() not in ("〉", "^C〉"):
        print(l)
