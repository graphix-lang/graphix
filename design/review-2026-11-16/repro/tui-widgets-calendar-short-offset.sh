#!/bin/bash
# Title: a calendar shorter than its month draws no weeks (or too few) unless
# its rect starts at the top of the screen.
# Command: bash design/review-2026-11-16/repro/tui-widgets-calendar-short-offset.sh
# Expected: in every screen, the calendar's weeks right under "above":
#   "           1  2  3  4" / "  5  6  7  8  9 10 11" / " 12 13 14 15 16 17 18"
# Observed (debug binary): the 5-row calendar at row 4 (tall path) draws its
# weeks; the 3-row one at row 4 (CalendarW::draw's short path) draws nothing
# between "above" and "below", and the 3-row one at row 1 draws two of its
# three week rows. ratatui's Monthly::render_monthly stops at `buf.area.height > y`
# (absolute y against the buffer's height), and the short path's buffer starts
# at the rect's y, so a rect whose y is at least its height gets no week rows.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
for c in "4 3" "1 3" "4 5"; do
set -- $c; a=$1; h=$2
cat > $D/cal$a$h.gx <<GX
use tui::{layout::{child, layout}, text::text, calendar::{calendar, date}};

layout(&[
  child(#constraint: \`Length($a), text(&"above")),
  child(#constraint: \`Length($h), calendar(&date(2024, 5, 15))),
  child(#constraint: \`Fill(1), text(&"below"))
])
GX
done
cat > $D/vt.py <<'PY'
import re, sys
rows, cols = int(sys.argv[2]), int(sys.argv[3])
data = open(sys.argv[1], 'rb').read().decode('utf-8', 'replace')
scr = [[' '] * cols for _ in range(rows)]
r = c = i = 0
csi = re.compile(r'\x1b\[([?0-9;]*)([A-Za-z@])')
while i < len(data):
    ch = data[i]
    if ch == '\x1b':
        m = csi.match(data, i)
        if m:
            a, cmd = m.group(1), m.group(2)
            n = [int(x) if x.isdigit() else 0 for x in a.lstrip('?').split(';')] if a else []
            if cmd in 'Hf':
                r = (n[0] if n and n[0] else 1) - 1
                c = (n[1] if len(n) > 1 and n[1] else 1) - 1
            elif cmd == 'J' or (cmd == 'h' and a.startswith('?1049')):
                scr = [[' '] * cols for _ in range(rows)]
            elif cmd == 'K':
                for x in range(c, cols): scr[r][x] = ' '
            elif cmd == 'C':
                c += n[0] if n and n[0] else 1
            i = m.end(); continue
        i += 2; continue
    if ch == '\r': c = 0
    elif ch == '\n': r = min(r + 1, rows - 1)
    elif ch >= ' ':
        if 0 <= r < rows and 0 <= c < cols: scr[r][c] = ch
        c += 1
    i += 1
for line in scr[:11]: print('|' + ''.join(line).rstrip())
PY
for c in "4 3" "1 3" "4 5"; do
  set -- $c; a=$1; h=$2
  echo "== calendar given $h rows at row $a"
  timeout -s KILL 30 script -qc "stty rows 12 cols 30; timeout -s KILL 6 $G --no-cache $D/cal$a$h.gx" $D/out$a$h >/dev/null 2>&1
  python3 -I $D/vt.py $D/out$a$h 12 30
done
rm -rf $D
