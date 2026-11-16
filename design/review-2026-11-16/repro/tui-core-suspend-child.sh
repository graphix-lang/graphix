#!/bin/bash
# Title: (test-gap template) tui::suspend through a running display, which no test drives
# Command: bash design/review-2026-11-16/repro/tui-core-suspend-child.sh
# A script suspends its display, runs `sh -c 'stty -a'` with inherited stdio, and resumes.
# Expected and observed (HEAD e1be2fbf2, works today): the child prints "icanon" (the display
#   gave the terminal back cooked) and CHILD_RAN; the display re-enters the alternate screen and
#   draws "done true released false"; ALT SCREEN TIMELINE shows leave ~1 s, enter ~1.1 s, leave 4 s.
here=$(dirname "$0")
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
cat > "$d/suspend_child.gx" <<'GX'
use tui::text::text;
let suspended = false;
let released = tui::suspend(suspended);
let done = seq sys::time::timer(duration:1.s, false) {
  suspended <- true;
  until live(released$);
  let child = sys::process::spawn(sys::process::options(#args: ["-c", "stty -a | tr ' ' '\\n' | grep -x -- '-\\?icanon'; echo CHILD_RAN"], "sh"))?;
  let status = sys::process::wait(child.proc)?;
  suspended <- false;
  status.success
};
sys::exit(sys::time::timer(duration:4.s, false) ~ 0);
text(&"done [done] released [released]")
GX
TAIL=1500 timeout -s KILL 30 python3 "$here/tui-core-ptyrun.py" --size 60x5 --wait 8 \
  -- "$G" --no-cache "$d/suspend_child.gx"
