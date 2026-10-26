#!/bin/bash
# examples-tui: layout_nested_focus.gx's hard-coded max_scroll = 150 does not
# match the wrapped paragraph at any width
# Command: bash design/review-2026-10-26/repro/examples-tui-nested-focus-scroll-length.sh
# Drives book/src/examples/tui/layout_nested_focus.gx in tmux: Tab (focus block 2),
# then Down to the scrollbar's end, at 80 and at 200 columns.
# Expected: the last position shows the end of the text ("50 I've got ... head!").
# Observed: at 80 columns the 150th position still ends in sentence 49 (the end is
#   unreachable); at 200 columns the text runs out near position 57 and the pane
#   scrolls on through ~90 blank positions while the scrollbar claims 150.
G=/home/eric/tmp/review-2026-10-26/bin/graphix
EX=$(cd "$(dirname "$0")/../../.." && pwd)/book/src/examples/tui/layout_nested_focus.gx
for W in 80 200; do
  S=rev-ex-tui-nf-$$-$W
  tmux -L $S new-session -d -x $W -y 24 -s s "timeout --foreground -s KILL 90 $G --no-cache $EX; sleep 100"
  sleep 14
  tmux -L $S send-keys -t s Tab; sleep 0.3
  for i in $(seq 150); do tmux -L $S send-keys -t s Down; sleep 0.03; done
  sleep 4
  echo "=== width $W after 150 Downs"
  tmux -L $S capture-pane -p -t s | awk '/Content!/{f=1} f' | head -10
  tmux -L $S kill-server
done
