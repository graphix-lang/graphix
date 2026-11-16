#!/usr/bin/env python3
# book-ui-b: a pty driver for TUI programs (no real terminal needed).
# usage: book-ui-b-tui-drive.py GRAPHIX prog.gx COLS ROWS START_DELAY 'key|key|...' [graphix args..]
#   each key is a string with \e-style escapes ('\x1b[A' is Up, '\r' Enter, '\x03' Ctrl-C),
#   or 'sleep:N'. Prints the screen (a minimal emulator: cursor moves and text) after each key.
#   STDERR_TO=file sends the program's stderr to that file instead of the pty.
import os, pty, sys, time, select, re, signal, struct, fcntl, termios

gx, prog, cols, rows, start = sys.argv[1], sys.argv[2], int(sys.argv[3]), int(sys.argv[4]), float(sys.argv[5])
keys = sys.argv[6].split("|") if len(sys.argv) > 6 and sys.argv[6] else []
extra = sys.argv[7:]
screen = [[" "] * cols for _ in range(rows)]
cur = [0, 0]

def feed(data):
    text = data.decode("utf-8", errors="ignore")
    j = 0
    while j < len(text):
        c = text[j]
        if c == "\x1b":
            m = re.match(r"\x1b\[([0-9;?]*)([A-Za-z@])", text[j:])
            if m:
                args, cmd = m.group(1), m.group(2)
                if cmd == "H":
                    p = [int(x) if x else 1 for x in args.split(";")] if args else [1, 1]
                    while len(p) < 2: p.append(1)
                    cur[0], cur[1] = p[0] - 1, p[1] - 1
                elif cmd == "J" and args in ("2", ""):
                    for r in range(rows): screen[r] = [" "] * cols
                elif cmd == "C":
                    cur[1] += int(args or 1)
                j += len(m.group(0)); continue
            j += 2; continue
        if c == "\r": cur[1] = 0
        elif c == "\n": cur[0] += 1
        elif ord(c) >= 32:
            if 0 <= cur[0] < rows and 0 <= cur[1] < cols: screen[cur[0]][cur[1]] = c
            cur[1] += 1
        j += 1

def show(label):
    print(f"--- {label}")
    for r in screen:
        l = "".join(r).rstrip()
        if l: print(l)

pid, fd = pty.fork()
if pid == 0:
    if os.environ.get("STDERR_TO"):
        e = os.open(os.environ["STDERR_TO"], os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o644)
        os.dup2(e, 2)
    os.execv(gx, [gx, "--no-cache"] + extra + [prog])
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))

def pump(secs):
    end = time.time() + secs
    while time.time() < end:
        r, _, _ = select.select([fd], [], [], max(0, end - time.time()))
        if r:
            try: d = os.read(fd, 65536)
            except OSError: return False
            if not d: return False
            feed(d)
    return True

pump(start); show("start")
for k in keys:
    if k.startswith("sleep:"): pump(float(k[6:])); continue
    try: os.write(fd, k.encode().decode("unicode_escape").encode())
    except OSError: break
    alive = pump(0.6); show(repr(k))
    if not alive: break
pump(1.0)
wpid, status = os.waitpid(pid, os.WNOHANG)
if wpid == 0:
    os.kill(pid, signal.SIGKILL); os.waitpid(pid, 0); print("--- killed (still running)")
else:
    print(f"--- exited status={os.waitstatus_to_exitcode(status)}")
