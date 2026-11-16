#!/bin/bash
# Title: a widget property read through a place reference (&s.items) is
# converted again at every write of `s`, though the value is unchanged.
# Command: bash design/review-2026-11-16/repro/tui-widgets-place-ref-reconvert.sh
# Method: a 1 ms timer writes `s.sel`; the display's bounded channel holds the
# runtime back while the display works, so the ticks counted in 6 s fall as
# each update costs more. All three hold a 2^16-element array in `s`.
# Expected: `place` about as fast as `plain` (the list's items never change).
# Observed (debug binary, 40x20 pty): plain ~1350 ticks (the per-frame list
# build of the list-full-rebuild finding), place ~450 ticks, control (reads
# &s.items, draws one line) ~2900 ticks: the extra cost is ListW converting
# the re-delivered 65536-line array (props! -> TRef::update) at every tick.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
gen() { # gen NAME WIDGET
  { echo 'use tui::{line, list::list};'; echo; echo 'let c = line("item");'; echo 'let a = [c];'
    for i in $(seq 1 16); do echo 'let a = array::concat(a, a);'; done
    cat <<GX
let clock = sys::time::timer(duration:1.ms, true);
let s = {items: a, sel: 0};
s <- clock ~ {s with sel: s.sel + 1};
let stop = sys::time::timer(duration:6.s, false);
let w = sys::fs::write_all(#path: "$D/$1.txt", stop ~ "[s.sel]");
sys::exit(w ~ 0);
let items = &s.items;
$2
GX
  } > $D/$1.gx
}
gen plain 'list(#selected: &(s.sel % 1000), &a)'
gen place 'list(#selected: &(s.sel % 1000), items)'
gen control 'list(#selected: &(s.sel % 1000), &[line("[array::len(*items)]")])'
for p in plain place control; do
  timeout -s KILL 60 script -qec "stty rows 20 cols 40; timeout -s KILL 40 $G --no-cache $D/$p.gx" /dev/null >/dev/null 2>&1
  echo "$p: $(cat $D/$p.txt 2>/dev/null) ticks"
done
rm -rf $D
