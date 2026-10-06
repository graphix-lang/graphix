#!/usr/bin/env python3
# tui-widgets-05: bar_chart: bar values and bar counts overflow ratatui's
# arithmetic; the 1024 cap does not prevent it.
#
# BarW::build passes any non-negative value (barchart.rs:67) and draw passes
# every bar (barchart.rs:331-342) to ratatui-widgets 0.3.0, whose
# BarChart::group_ticks computes `value * height * 8` in u64 (its
# barchart.rs:456) and `n * bar_width + (n - 1) * bar_gap` in u16 (its
# barchart.rs:436). The script runs each program below for 10 s in a 24x80
# pseudo-terminal and prints its panic line and what it drew.
#
# command: timeout -s KILL 120 python3 design/review-2026-10-05/repro/tui-widgets-05.py <graphix>
#   (<graphix> is a built binary, e.g. ~/tmp/target/debug/graphix)
#
# expected: no panic; value draws columns [24, 0, 12, ..] (the 1e17 bar fills
#   the 24 rows, the 5e16 bar half); width and many draw the bars that fit, as
#   ok (32768 bars) does.
# observed (HEAD c722befe, debug build; the process keeps running, nothing drawn):
#   value: PANIC .../ratatui-widgets-0.3.0/src/barchart.rs:456:36: attempt to multiply with overflow
#   width: PANIC .../ratatui-widgets-0.3.0/src/barchart.rs:436:35: attempt to multiply with overflow
#   many:  PANIC .../ratatui-widgets-0.3.0/src/barchart.rs:436:35: attempt to add with overflow
#   ok:    no panic; columns drawn: 36; bottom row: '  1 2 3 4 5 '
# observed (quick build of 10-04 with the same tui code, overflow checks off):
#   value: no panic; cells drawn in columns 1..6: [1, 0, 12, 0, 0, 0]; bottom row: '▇ █'
#   width: PANIC .../ratatui-core-0.1.0/src/buffer/buffer.rs:250:13: index outside of
#          buffer: the area is Rect { x: 0, y: 0, width: 80, height: 24 } but index is (80, 23)
#   many:  the same panic as width
#   ok:    as in the debug build
import fcntl, os, re, select, shlex, shutil, signal, struct, sys, tempfile, termios, time

ROWS, COLS = 24, 80
HEAD = 'use tui::barchart::{bar, bar_chart, bar_group};\n'
PROGS = {
    'value': 'bar_chart(&[bar_group([bar(&100000000000000000), bar(&50000000000000000)])])',
    'width': 'bar_chart(#bar_width: &1024, #bar_gap: &0, '
             '&[bar_group(array::init(64, |i| bar(&(i + 1))))])',
    'many': 'bar_chart(&[bar_group(array::init(32769, |i| bar(&(i % 10))))])',
    'ok': 'bar_chart(&[bar_group(array::init(32768, |i| bar(&(i % 10))))])',
}


def run(graphix, path, errpath, secs=10.0):
    master, slave = os.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', ROWS, COLS, 0, 0))
    errfd = os.open(errpath, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o644)
    pid = os.fork()
    if pid == 0:
        os.setsid()
        fcntl.ioctl(slave, termios.TIOCSCTTY, 0)
        os.dup2(slave, 0)
        os.dup2(slave, 1)
        os.dup2(errfd, 2)
        os.close(master)
        argv = graphix + ['--no-cache', path]
        os.execvp(argv[0], argv)
    os.close(slave)
    os.close(errfd)
    out = bytearray()
    deadline = time.time() + secs
    while time.time() < deadline:
        r, _, _ = select.select([master], [], [], 0.1)
        if r:
            try:
                out += os.read(master, 65536)
            except OSError:
                break
    os.kill(pid, signal.SIGKILL)
    os.waitpid(pid, 0)
    os.close(master)
    return out.decode('utf-8', 'replace')


def screen(s):
    grid = [[' '] * COLS for _ in range(ROWS)]
    r = c = i = 0
    tok = re.compile(r'\x1b\[([0-9;?]*)([A-Za-z])')
    while i < len(s):
        m = tok.match(s, i)
        if m:
            if m.group(2) == 'H':
                parts = (m.group(1) or '1;1').split(';')
                r, c = int(parts[0]) - 1, int(parts[1]) - 1
            i = m.end()
            continue
        if s[i] >= ' ' and 0 <= r < ROWS and 0 <= c < COLS:
            grid[r][c] = s[i]
            c += 1
        i += 1
    return [''.join(row).rstrip() for row in grid]


graphix = shlex.split(sys.argv[1])
tmp = tempfile.mkdtemp()
for name, body in PROGS.items():
    path = os.path.join(tmp, name + '.gx')
    with open(path, 'w') as f:
        f.write(HEAD + '\n' + body + '\n')
    errpath = os.path.join(tmp, name + '.err')
    rows = screen(run(graphix, path, errpath))
    err = open(errpath).read()
    m = re.search(r'panicked at (\S+)\n(.*)', err)
    print('%s: %s' % (name, ('PANIC %s %s' % (m.group(1), m.group(2))) if m else 'no panic'))
    heights = [sum(1 for row in rows if len(row) > x and row[x] != ' ') for x in range(COLS)]
    print('  cells drawn in columns 1..6: %s; columns drawn: %d; bottom row: %r'
          % (heights[:6], sum(1 for h in heights if h), rows[-1][:12]))
shutil.rmtree(tmp)
