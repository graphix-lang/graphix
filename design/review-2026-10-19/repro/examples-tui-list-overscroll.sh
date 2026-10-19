#!/bin/bash
# Title: list_interactive / scroll_list scroll by a hard-coded `visible`, not the viewport
# Command: timeout -s KILL 120 design/review-2026-10-19/repro/examples-tui-list-overscroll.sh
# Expected: list_interactive in a 9-row terminal (7 interior rows, 7 items) shows
#   Apple..Grape after End, nothing scrolled off; scroll_list's scrollbar thumb moves
#   with the list, and in a 14-row terminal Item 1 stays visible while Item 11 is selected.
# Observed: after End, list_interactive shows Cherry..Grape over two blank rows (Apple
#   and Banana hidden though everything fits). scroll_list at 6 rows: the list has
#   scrolled to Item 6 (ratatui follows the selection) while the thumb stays at the top
#   (scroll_pos is still 1); at 14 rows it scrolls to Item 2 though Item 11 fits.
D=$(dirname "$0"); E=$(cd "$D/../../.." && pwd)/book/src/examples/tui
echo "### list_interactive, 9 rows, End"
"$D/examples-tui-tmux-driver.sh" $E/list_interactive.gx 9 30 "sleep:5;key:End;sleep:0.6;cap"
echo "### scroll_list, 6 rows, 10 x Down"
"$D/examples-tui-tmux-driver.sh" $E/scroll_list.gx 6 30 "sleep:5;key:Down;key:Down;key:Down;key:Down;key:Down;key:Down;key:Down;key:Down;key:Down;key:Down;sleep:0.6;cap"
echo "### scroll_list, 14 rows, 10 x Down"
"$D/examples-tui-tmux-driver.sh" $E/scroll_list.gx 14 30 "sleep:5;key:Down;key:Down;key:Down;key:Down;key:Down;key:Down;key:Down;key:Down;key:Down;key:Down;sleep:0.6;cap"
