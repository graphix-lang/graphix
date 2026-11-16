#!/usr/bin/env python3
"""Run a command on a fresh PTY (40x10 unless --size), type keys after
delays, and report: exit status, whether the PTY was left in raw mode,
whether the alternate screen was left, and the tail of the output.

usage: ptyrun.py [--size COLSxROWS] [--wait SECS] [--keys 'DELAY:BYTES' ...] -- cmd args...
BYTES uses python escapes, e.g. '1.0:q' or '2.0:\\x03'.
"""
import os, pty, sys, time, select, termios, signal, struct, fcntl

def main():
    args = sys.argv[1:]
    size = (40, 10)
    wait = 10.0
    keys = []
    hangup = None
    resizes = []
    while args and args[0] != '--':
        a = args.pop(0)
        if a == '--size':
            c, r = args.pop(0).split('x'); size = (int(c), int(r))
        elif a == '--resize':
            t, cr = args.pop(0).split(':'); c, r = cr.split('x')
            resizes.append((float(t), int(c), int(r)))
        elif a == '--hangup':
            hangup = float(args.pop(0))
        elif a == '--wait':
            wait = float(args.pop(0))
        elif a == '--keys':
            d, b = args.pop(0).split(':', 1)
            keys.append((float(d), b.encode().decode('unicode_escape').encode('latin-1')))
    args.pop(0)
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', size[1], size[0], 0, 0))
    pid = os.fork()
    if pid == 0:
        os.setsid()
        fcntl.ioctl(slave, termios.TIOCSCTTY, 0)
        os.dup2(slave, 0); os.dup2(slave, 1); os.dup2(slave, 2)
        os.close(master)
        if slave > 2: os.close(slave)
        os.execvp(args[0], args)
    start = time.time()
    out = b''
    status = None
    keys.sort()
    events = []
    while True:
        now = time.time() - start
        while keys and keys[0][0] <= now:
            os.write(master, keys.pop(0)[1])
        while resizes and resizes[0][0] <= now:
            _, c, r = resizes.pop(0)
            fcntl.ioctl(master, termios.TIOCSWINSZ, struct.pack('HHHH', r, c, 0, 0))
            os.killpg(pid, signal.SIGWINCH)
            events.append((round(now, 2), f'resize {c}x{r}'))
        if hangup is not None and now > hangup and master >= 0:
            os.close(master); master = -1
            events.append((round(now, 2), 'hangup'))
        r, _, _ = select.select([master] if master >= 0 else [], [], [], 0.05)
        if r:
            try:
                data = os.read(master, 65536)
            except OSError:
                data = b''
            for tag in (b'[?1049h', b'[?1049l'):
                for _ in range(data.count(tag)):
                    events.append((round(now, 2), tag.decode()))
            out += data
            for _ in range(data.count(b'\x1b[6n')):
                os.write(master, b'\x1b[1;1R')
        p, st = os.waitpid(pid, os.WNOHANG)
        if p:
            status = st
            break
        if now > wait:
            os.kill(pid, signal.SIGKILL)
            _, status = os.waitpid(pid, 0)
            break
    # drain
    while master >= 0:
        r, _, _ = select.select([master], [], [], 0.1)
        if not r: break
        try:
            d = os.read(master, 65536)
        except OSError:
            break
        if not d: break
        out += d
    try:
        attrs = termios.tcgetattr(slave)
        raw = (attrs[3] & termios.ICANON) == 0
    except termios.error:
        raw = 'unknown (pty closed)'
    if os.WIFEXITED(status):
        st = f'exit {os.WEXITSTATUS(status)}'
    else:
        st = f'signal {os.WTERMSIG(status)}'
    print(f'STATUS: {st}')
    print(f'PTY LEFT RAW: {raw}')
    print(f'ENTERED ALT SCREEN: {out.count(b"[?1049h")}  LEFT ALT SCREEN: {out.count(b"[?1049l")}')
    print('ALT SCREEN TIMELINE:', events)
    print('OUTPUT TAIL:')
    print(repr(out[-int(os.environ.get('TAIL', '1500')):]))

main()
