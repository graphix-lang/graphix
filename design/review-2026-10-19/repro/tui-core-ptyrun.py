#!/usr/bin/env python3
"""Run a command under a PTY, send keys at times, report exit, termios, tail.

usage: ptyrun.py TOTAL_SECS 'KEYSCRIPT' cmd args...
KEYSCRIPT: semicolon separated  t=bytes  e.g. '1.5=q;3=\\x03'  (python escapes)
"""
import os, pty, sys, time, select, termios, signal, codecs

total = float(sys.argv[1])
script = []
if sys.argv[2]:
    for item in sys.argv[2].split(';'):
        t, b = item.split('=', 1)
        script.append((float(t), codecs.decode(b, 'unicode_escape').encode('latin1')))
cmd = sys.argv[3:]

master, slave = pty.openpty()
import fcntl, struct
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 10, 40, 0, 0))
pid = os.fork()
if pid == 0:
    os.setsid()
    fcntl.ioctl(slave, termios.TIOCSCTTY, 0)
    os.dup2(slave, 0); os.dup2(slave, 1); os.dup2(slave, 2)
    os.close(master); os.close(slave)
    os.execvp(cmd[0], cmd)
start = time.time()
out = b''
status = None
while True:
    now = time.time() - start
    while script and script[0][0] <= now:
        os.write(master, script.pop(0)[1])
    r, _, _ = select.select([master], [], [], 0.05)
    if r:
        try:
            d = os.read(master, 65536)
            out += d
        except OSError:
            pass
    p, st = os.waitpid(pid, os.WNOHANG)
    if p:
        status = st
        # drain
        try:
            while True:
                r, _, _ = select.select([master], [], [], 0.2)
                if not r: break
                d = os.read(master, 65536)
                if not d: break
                out += d
        except OSError:
            pass
        break
    if now > total:
        break
attrs = termios.tcgetattr(slave)
lflag = attrs[3]
if status is None:
    print("STILL RUNNING after", total, "s; killing")
    os.kill(pid, signal.SIGKILL)
    os.waitpid(pid, 0)
else:
    if os.WIFEXITED(status):
        print("EXIT", os.WEXITSTATUS(status))
    else:
        print("SIGNAL", os.WTERMSIG(status))
print("ICANON", bool(lflag & termios.ICANON), "ECHO", bool(lflag & termios.ECHO))
i = out.rfind(b'\x1b[?1049l')
print("left alt screen:", i >= 0, "entered:", out.count(b'\x1b[?1049h'), "left:", out.count(b'\x1b[?1049l'))
print("TAIL after last leave:", repr(out[i:][-1500:] if i >= 0 else out[-1500:]))
