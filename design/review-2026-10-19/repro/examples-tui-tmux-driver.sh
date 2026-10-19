#!/bin/bash
# Shared driver for the examples-tui repros: runs a TUI example headless in a
# private tmux server at a fixed size, sends keys, prints screen captures.
# usage: examples-tui-tmux-driver.sh file.gx rows cols "step;step;..."
#   step = sleep:N | key:K (tmux send-keys name) | cap
# GX overrides the graphix binary (default: the review's debug build).
F=$1; R=$2; C=$3; STEPS=$4
GX=${GX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
S=rvtui$$
tmux -L $S new-session -d -x $C -y $R "timeout --foreground -s KILL 60 $GX --no-cache $F 2>/dev/null; sleep 30"
IFS=';' read -ra A <<< "$STEPS"
for s in "${A[@]}"; do
  case $s in
    sleep:*) sleep ${s#sleep:};;
    key:*) tmux -L $S send-keys ${s#key:};;
    cap) echo "----- capture"; tmux -L $S capture-pane -p;;
  esac
done
tmux -L $S kill-server; sleep 0.5; pkill -9 -f "graphix --no-cache $F"
exit 0
