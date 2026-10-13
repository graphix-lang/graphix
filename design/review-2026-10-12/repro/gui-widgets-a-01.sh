#!/bin/sh
# gui-widgets-a-01: finite canvas inputs still produce NaN/inf geometry and
# panic the GUI process in draw.
#
# Runs the canvas tests of gui-widgets-a.rs (headless wgpu renderer, the
# same widget compile/draw path GuiHandler uses):
#   canvas_arc_to_reversal_draws: `Path [MoveTo(0,0), ArcTo(a:(100,0),
#     b:(0,0.00001), radius:10), Close] -- iced's Builder::arc_to divides
#     radius by tan(acos(1.0)/2) = 0, so the arc start is NaN
#   canvas_huge_circle_draws: `Circle center.x 3e38, radius 3e38 -> x = inf
#   canvas_huge_line_draws: control, passes
# Expected: all three draw (a bad shape skipped). Observed at 7324207f (dev):
#   both panic at lyon_path-1.0.19 path.rs:811 `assertion failed:
#   p.x.is_finite()`; in release the NaN reaches iced_wgpu geometry.rs
#   `.expect("Tessellate path.")` (FillTessellator: PositionIsNaN).
set -e
cd "$(dirname "$0")/../../.."
cp design/review-2026-10-12/repro/gui-widgets-a.rs \
   stdlib/graphix-package-gui/tests/review_gui_widgets_a_01.rs
trap 'rm -f stdlib/graphix-package-gui/tests/review_gui_widgets_a_01.rs' EXIT
env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 cargo test \
  -p graphix-package-gui --test review_gui_widgets_a_01 -- --nocapture canvas
