#!/usr/bin/env python3
# c-lib-04: a failing statement in multi-statement REPL input orphans the
# statements before it.
#
# GX::compile (graphix-rt/src/gx.rs:701-721) compiles each `;`-separated
# statement with `compile_stmt(..)?` into a local Vec and installs the
# nodes only after the loop. When statement k fails, statements 0..k-1
# have passed check_and_fuse (their runtime refs registered, their names
# bound in the env, their lambdas in lambda_defs/bind_to_lambda, a
# top-level catch's scope in self.scope) and are then dropped without
# `delete`. A name from the failed input stays bound with its type but
# never has a value; a lambda from it stays callable through static
# resolution; a catch from it stays the handler of every later input,
# with no node to run it; its refs stay in the runtime's by_ref.
#
# The REPL needs a terminal, so this drives it through a pseudo-terminal.
#
# command: GRAPHIX=/path/to/graphix python3 design/review-2026-10-05/repro/c-lib-04.py
#
# expected: the failed input leaves nothing behind, so `x + 1` and `f(1)`
# are refused (`x not defined`, `f not defined`), as `y` is, and
# `error(`Boom)?` warns `unhandled error ..` as it does in a fresh REPL;
# or, were partial success intended, `x + 1` prints 42, `f` prints its
# value and `error(`Boom)?` prints `caught ..`.
#
# observed (HEAD c722befe, debug build; the same with --no-fusion):
#   >>> let x = 41; let y = nosuch     error: ... nosuch not defined
#   >>> x + 1                          -: i64            (no value, ever)
#   >>> y                              error: ... y not defined
#   >>> let f = |a| a + 1; let z = nosuch   error: ... nosuch not defined
#   >>> f(1)                           -: i64  2         (static call works)
#   >>> f                              -: fn(a: '_..) -> '_..   (no value)
#   >>> catch(e) println("caught [e]"); let w = nosuch   error: ..
#   >>> error(`Boom)?                  -: '_..   (no warning, no `caught`:
#                                      the error is lost, here and in every
#                                      later input)
# With GRAPHIX_DBG_VARS=1, `let a = 1` then `let b = a + 1; let y = nosuch`
# prints `REF_VAR <a> by <b's statement>` and no UNREF_VAR ever follows.
import fcntl, os, pty, re, select, signal, struct, sys, tempfile, termios, time

GRAPHIX = os.environ.get("GRAPHIX", "graphix")
ARGS = ["--no-cache", "--no-netidx", "--no-init"] + sys.argv[1:]
WAIT = 4.0
LINES = [
    "let x = 41; let y = nosuch",
    "x + 1",
    "^C",
    "y",
    "let f = |a| a + 1; let z = nosuch",
    "f(1)",
    "^C",
    "f",
    "^C",
    'catch(e) println("caught [e]"); let w = nosuch',
    "error(`Boom)?",
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
