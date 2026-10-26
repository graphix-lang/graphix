#!/usr/bin/env python3
# Title: a TUI program's stdout writes (println, print, log's default dest)
#        land raw in the alternate screen; only stderr is held.
# Command: timeout -s KILL 60 python3 -I design/review-2026-10-26/repro/tui-core-stdout-into-alt-screen.py
# Expected: the text is held while the display has the terminal (as stderr is,
#           stderr::StderrHeld) and shown after the display ends, or the
#           write is refused/redirected -- never drawn over the frame.
# Observed (HEAD e6ffc6eed): "inside alt screen: True" -- the bytes
#           "PRINTED-TO-STDOUT\n" are written between ratatui frames inside
#           \x1b[?1049h..\x1b[?1049l; ratatui's diff render never erases them.
import os, pty, select, time, tempfile, fcntl, struct, termios

GX = os.environ.get("GX", "/home/eric/tmp/review-2026-10-26/bin/graphix")
PROG = r'''use tui::{
  input_handler::{Event, input_handler, on_press},
  paragraph::paragraph
};

let handle = |e: Event| on_press(e, |k| select k.code {
  q@ `Char("q") => {
    tui::exit(q ~ null);
    `Stop
  },
  p@ `Char("p") => {
    println(p ~ "PRINTED-TO-STDOUT");
    `Stop
  },
  _ => `Continue
});

input_handler(#handle: &handle, &paragraph(&"hello"))
'''

with tempfile.TemporaryDirectory() as d:
    path = os.path.join(d, "out.gx")
    open(path, "w").write(PROG)
    pid, fd = pty.fork()
    if pid == 0:
        os.environ["TERM"] = "xterm-256color"
        os.execv(GX, [GX, "--no-cache", path])
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 10, 40, 0, 0))
    out, t0, keys = bytearray(), time.time(), [(1.5, b"p"), (3.0, b"q")]
    while time.time() - t0 < 10:
        if keys and time.time() - t0 >= keys[0][0]:
            os.write(fd, keys.pop(0)[1])
        if select.select([fd], [], [], 0.05)[0]:
            try:
                b = os.read(fd, 65536)
            except OSError:
                break
            if not b:
                break
            out += b
    try:
        os.kill(pid, 9)
    except ProcessLookupError:
        pass
    os.waitpid(pid, 0)
    s = out.decode("utf-8", "replace")
    alt = s[s.find("\x1b[?1049h"):s.find("\x1b[?1049l")]
    print("inside alt screen:", "PRINTED-TO-STDOUT" in alt)
