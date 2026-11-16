#!/bin/bash
# Title: a tui::suspend asked while no display runs suspends the NEXT, unrelated display (REPL)
# Command: bash design/review-2026-11-16/repro/tui-core-stale-suspend.sh
# Line 1 shows display "one" and keeps `let s = tui::suspend(..)` (rising at 4 s) as a separate
# statement; Ctrl-C at 2.5 s leaves display "one", handing the suspend receiver back to
# TuiControl (headless stays false: no output is chosen at Ctrl-C). At ~5 s the suspend request
# is parked in the channel. Line 2 at 6 s starts display "two", which takes the receiver, finds
# the old request and suspends at once; the resume signal belongs to line 1's site, which lives on.
# Expected: display "two" stays up until the Ctrl-C at 11 s (control run, suspend at 30 s):
#   ALT SCREEN TIMELINE: [(1.03, '[?1049h'), (2.59, '[?1049l'), (6.04, '[?1049h'), (11.05, '[?1049l')]
# Observed: display "two" leaves the screen at once and the terminal is nobody's until Ctrl-C;
#   a line typed meanwhile ("40 + 2") is only echoed by the cooked terminal, never evaluated:
#   ALT SCREEN TIMELINE: [(1.04, '[?1049h'), (2.59, '[?1049l'), (6.05, '[?1049h'), (6.1, '[?1049l')]
here=$(dirname "$0")
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
export XDG_CACHE_HOME=$(mktemp -d)
for delay in 4 30; do
  echo "=== suspend timer ${delay}s"
  TAIL=0 timeout -s KILL 40 python3 "$here/tui-core-ptyrun.py" --size 60x10 --wait 16 \
    --keys "1:let s = tui::suspend(sys::time::timer(duration:${delay}.s, false) ~ true); tui::text::text(&\"one\")\r" \
    --keys '2.5:\x03' --keys '6:tui::text::text(&"two")\r' --keys '11:\x03' --keys '13:\x04' \
    -- "$G" --no-cache | grep TIMELINE
done
