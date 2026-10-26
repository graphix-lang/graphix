#!/bin/sh
# Title: a table row whose reference has no value yet is dropped from the
# table (TableW::draw filter_maps it out), so every later row moves up one
# index and `selected` highlights the wrong row.
# Command: sh design/review-2026-10-26/repro/tui-widgets-table-absent-row.sh
# Expected (before r0 arrives at 3 s): r0's slot kept (an empty row), and
#   ">> row one" highlighted, since selected is 1 and r1 is row 1.
# Observed at 2 s:            at 5 s:
#      row one                   row zero
#   >> row two                >> row one
#                                row two
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > $D/p.gx <<'GX'
use tui::{line, table::{cell, row, table}};

let late = sys::time::timer(duration:3.s, false);
let r0 = row([cell(line(late ~ "row zero"))]);
let r1 = row([cell(line("row one"))]);
let r2 = row([cell(line("row two"))]);
table(#highlight_symbol: &">> ", #selected: &1, &[&r0, &r1, &r2])
GX
S=tsel$$
tmux -L $S new-session -d -x 30 -y 5 "timeout --foreground -s KILL 12 $G --no-cache $D/p.gx; sleep 20"
sleep 2; echo "at 2 s:"; tmux -L $S capture-pane -p
sleep 3; echo "at 5 s:"; tmux -L $S capture-pane -p
tmux -L $S kill-server
rm -rf $D
