#!/bin/bash
# Title: a table row of 65536 cells with no `widths` crashes the display
# (ratatui divides the width by `col_count.max(1) as u16`, which is 0), and a
# few thousand columns stall each layout for seconds.
# Command: bash design/review-2026-11-16/repro/tui-widgets-table-many-columns.sh
# Expected: the table draws (its columns 0 wide, as ratatui gives a table with
# more columns than cells), "DRAWN" shows above it, no panic.
# Observed (debug binary, 40x10 pty):
#   2^16 cells: panicked at ratatui-widgets-0.3.2/src/table.rs:1049:37:
#     attempt to divide by zero ... the display failed
#   2^12 cells: the first frame takes 3 to 15 s; 2^14 cells: none within 15 s
#   (2^10 cells: under 3 s)
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
gen() {
  { echo 'use tui::{line, layout::{child, layout}, table::{cell, row, table}, text::text};'
    echo; echo 'let c = cell(line("x"));'; echo 'let a = [c];'
    for i in $(seq 1 $1); do echo 'let a = array::concat(a, a);'; done
    echo 'let r = row(a);'
    echo 'layout(&[child(#constraint: `Length(1), text(&"DRAWN")), child(#constraint: `Fill(1), table(&[&r]))])'
  } > $D/cols$1.gx
}
gen 16
echo "== 2^16 cells"
timeout -s KILL 40 script -qec "stty rows 10 cols 40; timeout -s KILL 15 $G --no-cache $D/cols16.gx" /dev/null 2>&1 | grep -a -A1 panicked | head -2
for n in 10 12 14; do
  gen $n
  for t in 3 15; do
    r=$(timeout -s KILL 40 script -qec "stty rows 10 cols 40; timeout -s KILL $t $G --no-cache $D/cols$n.gx" /dev/null 2>&1 | grep -a -c DRAWN)
    echo "2^$n cells, ${t}s: drawn=$r"
  done
done
rm -rf $D
