#!/bin/sh
# gui-widgets-a-05: OwnedContextMenu does not forward operate, so focus and
# scroll operations never reach its child.
#
# The probe is the test operations_reach_context_menu_child in
# design/review-2026-10-12/repro/gui-widgets-a.rs: it builds
#   column[text_input("outside"), text("plain"),
#          menu::context_menu([action("Copy")],
#                             column[text_input("inside"), text("wrapped")])]
# headlessly and runs an iced Operation over the UserInterface that counts
# focusables and records texts.
#
# Command (copy gui-widgets-a.rs to
# stdlib/graphix-package-gui/tests/review_gui_widgets_a.rs first):
#   timeout -s KILL 2400 cargo test -p graphix-package-gui \
#     --test review_gui_widgets_a -- operations_reach_context_menu_child --nocapture
#
# Expected: focusables 2, texts include "wrapped".
# Observed at 7324207f: "focusables 1 texts [\"plain\"]", assertion
#   `left == right` failed: left: 1, right: 2.
set -e
cp "$(dirname "$0")/gui-widgets-a.rs" stdlib/graphix-package-gui/tests/review_gui_widgets_a.rs
trap 'rm -f stdlib/graphix-package-gui/tests/review_gui_widgets_a.rs' EXIT
env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 cargo test -p graphix-package-gui \
  --test review_gui_widgets_a -- operations_reach_context_menu_child --nocapture
