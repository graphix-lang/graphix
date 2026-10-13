#!/bin/sh
# gui-widgets-a-03: two context menus can be open at once; one click or one
# shortcut fires an action in each.
# The probes are the tests second_context_menu_closes_the_first and
# one_shortcut_one_action in gui-widgets-a.rs (this directory): rows A and B,
# each menu::context_menu([action(#shortcut: ctrl+d, Delete), action(Copy)]);
# right-click A, right-click B, then a left click where the menus overlap,
# or Ctrl+D.
# Command (from the repo root):
#   sh design/review-2026-10-12/repro/gui-widgets-a-03.sh
# Expected: each test publishes at most one call.
# Observed at 7324207f:
#   one left click published 2 call(s): copy_a = 1, del_b = 1
#   one Ctrl+D published 2 call(s): del_a = 1, del_b = 1
set -e
cp design/review-2026-10-12/repro/gui-widgets-a.rs \
   stdlib/graphix-package-gui/tests/review_gui_widgets_a.rs
trap 'rm -f stdlib/graphix-package-gui/tests/review_gui_widgets_a.rs' EXIT
env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 \
  cargo test -p graphix-package-gui --test review_gui_widgets_a -- --nocapture \
  --exact second_context_menu_closes_the_first one_shortcut_one_action
