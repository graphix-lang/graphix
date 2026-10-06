#!/usr/bin/env python3
# tui-widgets.r2-11: chart/canvas: a NaN point draws a spurious line to the top-left corner
#
# DatasetW::set_data (stdlib/graphix-package-tui/src/chart.rs:142-148) and the canvas
# shapes (canvas.rs:21-36, 74-77) pass non-finite coordinates to ratatui-widgets 0.3.0.
# Its Painter::get_point (canvas.rs:449-463) lets NaN through the bounds test (every
# comparison is false) and `NaN as usize` is 0, so a NaN x lands in column 0 and a NaN
# y in row 0; line-clipping 0.3.7's Region::from_point counts a NaN point as inside, so
# a segment to it is not clipped. The script runs each program below in a 8x20
# pseudo-terminal (each exits itself after 1 s idle) and prints the screen it drew.
#
# command: timeout -s KILL 120 python3 design/review-2026-10-05/repro/tui-widgets.r2-11.py <graphix>
#   (<graphix> is a built binary, e.g. ~/tmp/target/debug/graphix)
#
# expected: line_nan draws what control draws (the NaN sample is a gap or is skipped);
#   scatter_nan draws two dots near the middle and nothing at the corner; bar_nan draws
#   no bar at x = 0.5; canvas_nan draws nothing (its only shape ends at NaN).
# observed (HEAD c722befe, debug build; the 8x20 screen):
#   control                 line_nan                canvas_nan
#   |                    |  |██                  |  |██                  |
#   |                    |  |  ███               |  |  ███               |
#   |                    |  |     ███            |  |     ███            |
#   |                    |  |        ██          |  |        ██          |
#   |                    |  |          ███       |  |          ███       |
#   |                    |  |             ███    |  |             ███    |
#   |               ███  |  |               ███  |  |                ██  |
#   |                    |  |                    |  |                    |
#   scatter_nan: a dot at row 1, column 1 beside the two dots in rows 4-5;
#   bar_nan: the x = 0.5 bar fills rows 1-8 (the 0.2 and 0.3 bars are 2 and 3 rows).
#   An infinite point, (1.0 / 0.0, 1.0 / 0.0), draws a diagonal to the top-right corner.
import fcntl, os, re, select, shlex, shutil, signal, struct, sys, tempfile, termios, time

ROWS, COLS = 8, 20
EXIT = 'sys::exit(sys::time::after_idle(duration:1.s, 0));\n'
CHART = ('use tui::chart::{axis, chart, dataset};\nlet nan = 0.0 / 0.0;\n'
         'let pts: Array<(f64, f64)> = %s;\n' + EXIT +
         'chart(#x_axis: &axis({min: 0.0, max: 1.0}), #y_axis: &axis({min: 0.0, max: 1.0}),\n'
         '  &[dataset(#graph_type: &`%s, #marker: &`Block, &pts)])\n')
PROGS = {
    'control': CHART % ('[(0.8, 0.1), (0.9, 0.1)]', 'Line'),
    'line_nan': CHART % ('[(0.8, 0.1), (0.9, 0.1), (nan, nan)]', 'Line'),
    'scatter_nan': CHART % ('[(0.5, 0.5), (nan, nan), (0.6, 0.6)]', 'Scatter'),
    'bar_nan': CHART % ('[(0.2, 0.2), (0.5, nan), (0.8, 0.3)]', 'Bar'),
    'canvas_nan': 'use tui::canvas::canvas;\nlet nan = 0.0 / 0.0;\n'
                  'let l = `Line({color: `Red, x1: 0.9, y1: 0.1, x2: nan, y2: nan});\n' + EXIT +
                  'canvas(#marker: &`Block, #x_bounds: &{min: 0.0, max: 1.0},\n'
                  '  #y_bounds: &{min: 0.0, max: 1.0}, &[&l])\n',
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
        elif os.waitpid(pid, os.WNOHANG) != (0, 0):
            pid = None
            break
    if pid is not None:
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
            if m.group(2) == 'H' and not m.group(1).startswith('?'):
                parts = (m.group(1) or '1;1').split(';')
                r, c = int(parts[0]) - 1, int(parts[1]) - 1
            i = m.end()
            continue
        if s[i] >= ' ' and 0 <= r < ROWS and 0 <= c < COLS:
            grid[r][c] = s[i]
            c += 1
        i += 1
    return [''.join(row) for row in grid]


graphix = shlex.split(sys.argv[1])
tmp = tempfile.mkdtemp()
for name, body in PROGS.items():
    path = os.path.join(tmp, name + '.gx')
    with open(path, 'w') as f:
        f.write(body)
    errpath = os.path.join(tmp, name + '.err')
    rows = screen(run(graphix, path, errpath))
    err = open(errpath).read().strip()
    print(name + (' (stderr: %s)' % err[:200] if err else ''))
    for row in rows:
        print('  |' + row + '|')
shutil.rmtree(tmp)
