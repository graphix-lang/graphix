#!/bin/sh
# Title: a canvas line (or a chart Line dataset segment) with huge but finite
# coordinates is drawn along the wrong path: line-clipping's cohen_sutherland
# computes (x_min - x1) * dy / dx, whose product overflows to inf, then inf/inf
# is NaN, which every region test calls "inside"; ratatui then places NaN at
# column 0. ShapeV::finite and DatasetW's breaks only check x2 - x1 and y2 - y1.
# Command: sh design/review-2026-10-26/repro/tui-widgets-canvas-line-clip.sh
# Expected: both screens identical: the segment from (-1e300, 1e300) to (5, 5)
#   (and the one from (-1000, 1010)) enters the [0,10]x[0,10] window at its
#   top-left corner, so a diagonal from the top-left to the centre.
# Observed: the first screen (huge coordinates) draws a diagonal from the
#   BOTTOM-left corner up to the centre; the second (moderate coordinates)
#   draws the correct one from the top-left corner down to the centre.
# Third case: the line y = 5 from x = -1e308 to x = 1e308 crosses the whole
#   window; expected a full row of blocks on row 5, observed an empty screen
#   (ShapeV::finite refuses it since x2 - x1 overflows; a chart Line dataset
#   breaks there and draws nothing between the two points).
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
for c in "-1e300, y1: 1e300" "-1000.0, y1: 1010.0"; do
cat > $D/p.gx <<GX
use tui::canvas::canvas;

let l = \`Line({color: \`Red, x1: $c, x2: 5.0, y2: 5.0});
canvas(
    #marker: &\`Block,
    #x_bounds: &{min: 0.0, max: 10.0},
    #y_bounds: &{min: 0.0, max: 10.0},
    &[&l]
)
GX
S=clip$$
tmux -L $S new-session -d -x 11 -y 11 "timeout --foreground -s KILL 10 $G --no-cache $D/p.gx; sleep 20"
sleep 5
echo "x1: $c"
tmux -L $S capture-pane -p
tmux -L $S kill-server
done
cat > $D/h.gx <<'GX'
use tui::canvas::canvas;

let l = `Line({color: `Red, x1: -1e308, y1: 5.0, x2: 1e308, y2: 5.0});
canvas(
    #marker: &`Block,
    #x_bounds: &{min: 0.0, max: 10.0},
    #y_bounds: &{min: 0.0, max: 10.0},
    &[&l]
)
GX
S=clip$$
tmux -L $S new-session -d -x 11 -y 11 "timeout --foreground -s KILL 10 $G --no-cache $D/h.gx; sleep 20"
sleep 5
echo "x1: -1e308, x2: 1e308, y = 5 (rows end in |):"
tmux -L $S capture-pane -p | sed 's/$/|/'
tmux -L $S kill-server
rm -rf $D
