#!/bin/bash
# Title: a wrapped paragraph scrolled to row y wraps every line above y at
# every frame: a long text followed at its tail costs O(lines) per redraw.
# Command: bash design/review-2026-11-16/repro/tui-widgets-paragraph-wrap-scroll-cost.sh
# Method: a 1 ms timer moves the scroll within the last 20 rows; the display's
# bounded channel holds the runtime back while a frame draws, so the ticks
# counted in 6 s fall as a frame costs more.
# Expected: about the same tick count for each (a 20-row window).
# Observed (debug binary, 40x20 pty): 2 lines wrapped ~2900 ticks; 2^18 lines
# wrapped ~90 ticks (~70 ms a frame); 2^18 lines unwrapped ~2900 ticks.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
gen() { # gen NAME N WRAP
  { echo 'use tui::{line, paragraph::paragraph};'; echo; echo 'let c = line("a line of log text");'; echo 'let a = [c];'
    for i in $(seq 1 $2); do echo 'let a = array::concat(a, a);'; done
    cat <<GX
let clock = sys::time::timer(duration:1.ms, true);
let k = 0;
k <- clock ~ k + 1;
let stop = sys::time::timer(duration:6.s, false);
let w = sys::fs::write_all(#path: "$D/$1.txt", stop ~ "[k]");
sys::exit(w ~ 0);
let n = array::len(a);
paragraph(#wrap: &$3, #scroll: &{x: 0, y: n - 20 + (k % 10)}, &a)
GX
  } > $D/$1.gx
}
gen small_wrapped 1 true
gen big_wrapped 18 true
gen big_unwrapped 18 false
for p in small_wrapped big_wrapped big_unwrapped; do
  timeout -s KILL 60 script -qec "stty rows 20 cols 40; timeout -s KILL 40 $G --no-cache $D/$p.gx" /dev/null >/dev/null 2>&1
  echo "$p: $(cat $D/$p.txt 2>/dev/null) ticks"
done
rm -rf $D
