#!/usr/bin/env python3
# Title: stderr written while the TUI re-takes the terminal after tui::suspend
#        lands on the alternate screen, uncaptured.
# Command: timeout -s KILL 170 python3 -I design/review-2026-10-26/repro/tui-core-stderr-leak-at-resume.py
# Expected: every run prints "leaked 0": while the display holds the terminal,
#           stderr is held and replayed after the display ends.
# Observed (HEAD e6ffc6eed, debug build): 2 of 5 runs show
#           "\x1b[?1049h...line: 8, column: 5 dbg(r): false" -- the dbg line
#           written raw into the alternate screen right after the re-entry,
#           before Live::take reaches StderrHeld::hold; ratatui's diff render
#           never erases it.
import os, pty, select, time, tempfile, fcntl, struct, termios

GX = os.environ.get("GX", "/home/eric/tmp/review-2026-10-26/bin/graphix")
PROG = r'''use tui::{
  input_handler::{Event, input_handler, on_press},
  paragraph::paragraph
};

let susp = false;
let r = tui::suspend(susp);
dbg(r);

let handle = |e: Event| on_press(e, |k| select k.code {
  q@ `Char("q") => {
    tui::exit(q ~ null);
    `Stop
  },
  s@ `Char("s") => {
    susp <- s ~ true;
    susp <- sys::time::after_idle(duration:1.s, s) ~ false;
    `Stop
  },
  _ => `Continue
});

input_handler(#handle: &handle, &paragraph(&"r=[r]"))
'''

def run(path):
    pid, fd = pty.fork()
    if pid == 0:
        os.environ["TERM"] = "xterm-256color"
        os.execv(GX, [GX, "--no-cache", path])
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 10, 40, 0, 0))
    out, t0, keys = bytearray(), time.time(), [(2.0, b"s"), (5.0, b"q")]
    while time.time() - t0 < 12:
        if keys and time.time() - t0 >= keys[0][0]:
            os.write(fd, keys.pop(0)[1])
        if select.select([fd], [], [], 0.05)[0]:
            try:
                d = os.read(fd, 65536)
            except OSError:
                break
            if not d:
                break
            out += d
    try:
        os.kill(pid, 9)
    except ProcessLookupError:
        pass
    os.waitpid(pid, 0)
    s = out.decode("utf-8", "replace")
    second = s.find("\x1b[?1049h", s.find("\x1b[?1049h") + 1)
    leave = s.find("\x1b[?1049l", second)
    return s[second:leave].count("dbg(r)")

with tempfile.TemporaryDirectory() as d:
    path = os.path.join(d, "susp.gx")
    open(path, "w").write(PROG)
    for i in range(5):
        print("run", i, "leaked", run(path))
