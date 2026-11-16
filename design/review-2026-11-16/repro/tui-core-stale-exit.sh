#!/bin/bash
# Title: a tui::exit that fires while no display runs ends the NEXT, unrelated display at once (REPL)
# Command: bash design/review-2026-11-16/repro/tui-core-stale-exit.sh
# The REPL runs on a fresh PTY (tui-core-ptyrun.py). Line 1 shows display "one" and keeps a
# `tui::exit` on a 4 s timer as a separate statement; Ctrl-C at 2.5 s leaves display "one"
# (Ended::Left, the session goes on); the timer fires at ~5 s with no display up, which
# TuiControl::request_exit records as StopState::PendingExit. Line 2 at 6 s shows display "two".
# Expected: display "two" stays up until the Ctrl-C at 11 s (as in the control run, exit timer 30 s):
#   ALT SCREEN TIMELINE: [(1.05, '[?1049h'), (2.55, '[?1049l'), (6.01, '[?1049h'), (11.07, '[?1049l')]
# Observed: display "two" ends ~50 ms after it starts (install_stop finds PendingExit):
#   ALT SCREEN TIMELINE: [(1.05, '[?1049h'), (2.6, '[?1049l'), (6.01, '[?1049h'), (6.06, '[?1049l')]
# (A second exit after a display that ended by Ended::Requested does the same.)
here=$(dirname "$0")
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
export XDG_CACHE_HOME=$(mktemp -d)
for delay in 4 30; do
  echo "=== exit timer ${delay}s"
  TAIL=0 timeout -s KILL 40 python3 "$here/tui-core-ptyrun.py" --size 60x10 --wait 16 \
    --keys "1:tui::exit(sys::time::timer(duration:${delay}.s, false)); tui::text::text(&\"one\")\r" \
    --keys '2.5:\x03' --keys '6:tui::text::text(&"two")\r' --keys '11:\x03' --keys '13:\x04' \
    -- "$G" --no-cache | grep TIMELINE
done
