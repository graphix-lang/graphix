# Drive a TUI program in a PTY of a chosen size and print the raw bytes it
# writes (escape sequences included), then kill it.
#
#   timeout -s KILL 30 python3 -I tests-tui-pty-drive.py COLS ROWS prog.gx [SECONDS]
#
# Used by the tests-tui-*.gx repros: compare the bytes drawn at two sizes.
import os, pty, sys, time, struct, fcntl, termios, select, signal

GRAPHIX = "/home/eric/tmp/review-2026-11-16/bin/graphix"
cols, rows, prog = int(sys.argv[1]), int(sys.argv[2]), sys.argv[3]
secs = float(sys.argv[4]) if len(sys.argv) > 4 else 5.0
env = dict(os.environ, TERM="xterm-256color")
pid, fd = pty.fork()
if pid == 0:
    os.execve(GRAPHIX, ["graphix", "--no-cache", prog], env)
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
out = b""
start = time.time()
while time.time() - start < secs:
    r, _, _ = select.select([fd], [], [], 0.1)
    if r:
        try:
            d = os.read(fd, 65536)
        except OSError:
            break
        if not d:
            break
        out += d
try:
    os.kill(pid, signal.SIGKILL)
except Exception:
    pass
os.waitpid(pid, 0)
sys.stdout.write(repr(out.decode("utf8", "replace")) + "\n")
