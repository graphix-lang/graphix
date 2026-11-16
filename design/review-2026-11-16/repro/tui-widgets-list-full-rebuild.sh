#!/bin/bash
# Title: a list builds a ratatui item for every element at every frame, though
# only a screenful shows: a long list makes each redraw cost O(items).
# Command: bash design/review-2026-11-16/repro/tui-widgets-list-full-rebuild.sh
# Method: a 1 ms timer moves the selection; the display's bounded channel holds
# the runtime back while a frame draws, so the ticks counted in 6 s fall as a
# frame costs more. The control holds the same 2^18-element array but lists one
# line.
# Expected: about the same tick count for all three (a 20-row window).
# Observed (debug binary, 40x20 pty):
#   2 items: ~2900 ticks; 2^18 items: ~340 ticks; control (2^18 array, 1 item
#   listed): ~2900 ticks. A table of 2^15 three-cell rows: ~880 ticks.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
gen() { # gen NAME N WIDGET
  { echo 'use tui::{line, list::list, table::{cell, row, table}};'; echo
    echo 'let c = line("item");'
    echo 'let r = row([cell(line("a")), cell(line("b")), cell(line("c"))]);'
    echo 'let a = [c];'; echo 'let rs = [&r];'
    for i in $(seq 1 $2); do echo 'let a = array::concat(a, a);'; echo 'let rs = array::concat(rs, rs);'; done
    cat <<GX
let clock = sys::time::timer(duration:1.ms, true);
let k = 0;
k <- clock ~ k + 1;
let stop = sys::time::timer(duration:6.s, false);
let w = sys::fs::write_all(#path: "$D/$1.txt", stop ~ "[k]");
sys::exit(w ~ 0);
$3
GX
  } > $D/$1.gx
}
gen small 1 'list(#selected: &(k % 1000), &a)'
gen big 18 'list(#selected: &(k % 1000), &a)'
gen control 18 'list(#selected: &(k % 1000), &[line("[array::len(a)]")])'
gen table 15 'table(#selected: &(k % 1000), &rs)'
for p in small big control table; do
  timeout -s KILL 60 script -qec "stty rows 20 cols 40; timeout -s KILL 40 $G --no-cache $D/$p.gx" /dev/null >/dev/null 2>&1
  echo "$p: $(cat $D/$p.txt 2>/dev/null) ticks"
done
rm -rf $D
