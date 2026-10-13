#!/bin/sh
# tui-core-04: the TUI's process-wide panic hook gives the terminal back on
# every panic, caught ones included.
#
# A user Ord that is no total order panics array::sort's Rust sort; the
# runtime catches it (sort_values) and goes on. The hook installed by
# Live::take has already left the alternate screen and cooked the terminal,
# and the display keeps drawing onto the main screen.
#
# Run: sh design/review-2026-10-12/repro/tui-core-04.sh <graphix binary>
# Expected: ICANON=0 ECHO=0 at t=2.0s (the TUI still holds the terminal), one
#   "leave alt screen" sequence at exit, "sorted 32" drawn on the alternate screen.
# Observed (HEAD 7324207f, debug build):
#   t=0.7s ICANON=0 ECHO=0
#   t=2.0s ICANON=1 ECHO=1
#   leave alt screen ?1049l at [140, 261]   (the first is the hook, mid-run)
#   the raw output shows "sorted 32" drawn after that first \e[?1049l,
#   i.e. over the shell's main screen, in cooked mode.
set -e
B=${1:?graphix binary}
D=$(mktemp -d)
cat > "$D/tuipanic.gx" <<'GX'
use tui::paragraph::paragraph;
type T = Abstract<i64>;
impl Ord for T {
    let cmp = |x, y| select ((x.0 % 3) - (y.0 % 3) + 3) % 3 {
        0 => select (x.0, y.0) {
            (a, b) if a < b => `Less,
            (a, b) if a > b => `Greater,
            _ => `Equal
        },
        1 => `Less,
        _ => `Greater
    }
};
let go = sys::time::timer(duration:1.s, false)$;
let xs = array::init(32, |i| T((i * 7919) % 32));
let s = array::sort(go ~ xs);
let text = "before";
text <- s ~ "sorted [array::len(s)]";
sys::exit(sys::time::timer(duration:3.s, false)$ ~ 0);
paragraph(&text)
GX
cat > "$D/drive.py" <<'PY'
import os, sys, time, termios, subprocess, select, struct, fcntl
binary, prog = sys.argv[1], sys.argv[2]
m, s = os.openpty()
fcntl.ioctl(s, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))
p = subprocess.Popen([binary, "--no-cache", prog], stdin=s, stdout=s, stderr=s,
                     start_new_session=True,
                     preexec_fn=lambda: fcntl.ioctl(0, termios.TIOCSCTTY, 0))
out = bytearray(); marks = []
def state():
    a = termios.tcgetattr(s)
    return "ICANON=%d ECHO=%d" % (bool(a[3] & termios.ICANON), bool(a[3] & termios.ECHO))
t0 = time.time(); probes = [0.7, 2.0]
while time.time() - t0 < 6:
    r, _, _ = select.select([m], [], [], 0.05)
    if r:
        try: d = os.read(m, 65536)
        except OSError: break
        if not d: break
        out += d
    if probes and time.time() - t0 > probes[0]:
        marks.append((probes.pop(0), state(), len(out)))
    if p.poll() is not None and not r: break
p.wait()
print("exit", p.returncode)
for t, st, n in marks: print("t=%.1fs %s bytes_so_far=%d" % (t, st, n))
def find(seq):
    i = 0; res = []
    while True:
        i = out.find(seq, i)
        if i < 0: return res
        res.append(i); i += 1
print("enter alt screen ?1049h at", find(b"\x1b[?1049h"))
print("leave alt screen ?1049l at", find(b"\x1b[?1049l"))
print("'panicked' text at", find(b"panicked"))
print("'sorted 32' drawn at", find(b"sorted 32"))
print("total bytes", len(out)); print(repr(bytes(out)))
PY
timeout -s KILL 30 python3 -I "$D/drive.py" "$B" "$D/tuipanic.gx"
rm -rf "$D"
