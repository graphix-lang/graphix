#!/bin/bash
# Title: paragraph_log_viewer starts with a blank entry and never shows the newest lines
# Command: timeout -s KILL 120 design/review-2026-10-19/repro/examples-tui-log-viewer-tail.sh
# Expected: a "live log viewer" whose newest entry is on screen, and no empty entry.
# Observed (5-row terminal, after ~15 s, ~14 entries published): line 1 is the empty
#   string the initial `log_source = ""` published, then entries 1..4; entries 5..14
#   are below the bottom edge and stay there (the paragraph draws from the top).
D=$(dirname "$0"); E=$(cd "$D/../../.." && pwd)/book/src/examples/tui
"$D/examples-tui-tmux-driver.sh" $E/paragraph_log_viewer.gx 5 50 "sleep:4;cap;sleep:11;cap"
