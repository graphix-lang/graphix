#!/bin/sh
# Title: a calendar shorter than its month, on a terminal of more than 65535 cells,
# panics in ratatui's Buffer::merge (u16 index arithmetic); a release build wraps
# and moves frame cells to wrong positions instead.
# Command: sh design/review-2026-10-26/repro/tui-widgets-calendar-merge-overflow.sh
# Expected: the calendar's first 3 rows above "below", no panic.
# Observed (debug binary, 300x300 pty):
#   thread 'tokio-rt-worker' panicked at ratatui-core-0.1.2/src/buffer/buffer.rs:444:22:
#   attempt to multiply with overflow ... Error: the display failed
# At 20x60 the same program draws normally.
# Second part: a calendar in a blue-background block. Expected the blue
# background (SGR 48;5;4) at every height. Observed: present at 12 rows, absent
# at 4 rows (the short path's Buffer::empty cells overwrite the block's style):
#   rows=4: lines with a blue SGR: 0     rows=12: 1
D=$(mktemp -d)
cat > $D/cal.gx <<'GX'
use tui::{calendar::{calendar, date}, layout::{child, layout}, text::text};

layout(&[
    child(#constraint: `Length(3), calendar(&date(2024, 5, 15))),
    child(#constraint: `Fill(1), text(&"below"))
])
GX
timeout -s KILL 10 script -qec "stty rows 300 cols 300; timeout -s KILL 4 ${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix} --no-cache $D/cal.gx" /dev/null 2>&1 | grep -a -A1 panicked
cat > $D/bg.gx <<'GX'
use tui::{style, block::block, calendar::{calendar, date}};

block(#style: &style(#bg: `Blue), &calendar(&date(2024, 5, 15)))
GX
for r in 4 12; do
  n=$(timeout -s KILL 10 script -qec "stty rows $r cols 40; timeout -s KILL 3 ${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix} --no-cache $D/bg.gx" /dev/null 2>&1 | grep -ac '48;5;4m')
  echo "rows=$r: lines with a blue SGR: $n"
done
rm -rf $D
