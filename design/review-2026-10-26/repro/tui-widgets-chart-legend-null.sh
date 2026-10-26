#!/bin/sh
# Title: a chart's `#legend_position: &null` draws the legend at the top right
# (ratatui's default) instead of hiding it; ChartW::draw calls legend_position
# only for a non-null value, so nothing in Graphix can hide the legend of a
# chart whose datasets are named.
# Command: sh design/review-2026-10-26/repro/tui-widgets-chart-legend-null.sh
# Expected: no legend box (null = no legend, as ratatui's legend_position(None)).
# Observed:
#                                      ┌───┐
#                                      │cpu│
#                                      └───┘
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > $D/p.gx <<'GX'
use tui::{line, chart::{axis, chart, dataset}};

chart(
    #legend_position: &null,
    #x_axis: &axis({min: 0.0, max: 10.0}),
    #y_axis: &axis({min: 0.0, max: 10.0}),
    &[dataset(#name: &line("cpu"), &[(1.0, 1.0)])]
)
GX
S=leg$$
tmux -L $S new-session -d -x 40 -y 16 "timeout --foreground -s KILL 12 $G --no-cache $D/p.gx; sleep 20"
sleep 5; tmux -L $S capture-pane -p | head -4
tmux -L $S kill-server
rm -rf $D
