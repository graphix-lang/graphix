#!/bin/bash
# examples-tui: input_line_edit.gx drops a bracketed paste
# Command: bash design/review-2026-10-26/repro/examples-tui-line-edit-paste.sh
# Drives book/src/examples/tui/input_line_edit.gx in a private 80x24 tmux
# server: types "ab", pastes "XYZ" (tmux paste-buffer -p, a bracketed paste),
# then presses Enter.
# Expected: the editor shows "abXYZ" and Enter saves "saved: abXYZ" (the book's
#   Text Entry section says line_edit::handle applies typing AND paste).
# Observed: the paste vanishes: the editor shows "ab", Enter saves "saved: ab".
#   The handler wraps everything in on_press, which answers `Continue to the
#   `Paste event, so line_edit::handle never sees it. The same paste into
#   input_form.gx (whose form.gx routes `Paste to the editor) lands.
G=/home/eric/tmp/review-2026-10-26/bin/graphix
EX=$(cd "$(dirname "$0")/../../.." && pwd)/book/src/examples/tui/input_line_edit.gx
S=rev-ex-tui-paste-$$
tmux -L $S new-session -d -x 80 -y 24 -s s "timeout --foreground -s KILL 60 $G --no-cache $EX; sleep 100"
trap 'tmux -L $S kill-server' EXIT
sleep 12
tmux -L $S send-keys -t s a b; sleep 0.5
tmux -L $S set-buffer XYZ; tmux -L $S paste-buffer -p -t s; sleep 1
tmux -L $S capture-pane -p -t s | sed -n 2,3p
tmux -L $S send-keys -t s Enter; sleep 1
tmux -L $S capture-pane -p -t s | sed -n 2,3p
