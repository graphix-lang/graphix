#!/usr/bin/env bash
# gx-ui-06: tui table: setting #selected/#selected_cell/#selected_column back
# to null never deselects. TableW::draw (stdlib/graphix-package-tui/src/
# table.rs:317-325) copies the three refs into its persistent TableState only
# when they hold a value, so once a ref goes null the old row (column, cell)
# stays highlighted. ListW applies with_selected(None) and clears.
#
# The program selects row 1 (">>" symbol), then writes null to `sel` after
# 1 s; the header prints `sel`, so the frame drawn after the write is visible.
# It runs in a private tmux server (needs tmux), and the script prints the
# screen before the write and after it.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/gx-ui-06.sh
# expected after: the "sel=null" header and no ">>" on any row (the same
#   program over `list` drops the ">>").
# observed after: the "sel=null" header and ">>cc" still drawn:
#     sel=null                      hdr
#     aa                            bb
#   >>cc                            dd
# #selected_column (with #column_highlight_style) and #selected_cell (with
# #cell_highlight_style) behave the same: the reversed column or cell stays
# after the ref reads null.
set -u
G=${GRAPHIX:-graphix}
D=$(mktemp -d)
S=gxui06.$$
trap 'tmux -L $S kill-server 2>/dev/null; rm -rf "$D"' EXIT
cat > "$D/p.gx" <<'EOF'
use tui::{line, table::{cell, row, table}};
let sel: [i64, null] = 1;
sel <- sys::time::after_idle(duration:1.s, null);
let hdr = row([cell(line("sel=[sel]")), cell(line("hdr"))]);
let r1 = row([cell(line("aa")), cell(line("bb"))]);
let r2 = row([cell(line("cc")), cell(line("dd"))]);
sys::exit(sys::time::after_idle(duration:8.s, 0));
table(#header: &hdr, #highlight_symbol: &">>", #selected: &sel, &[&r1, &r2])
EOF
echo 'set -g remain-on-exit on' > "$D/tmux.conf"
tmux -L $S -f "$D/tmux.conf" new-session -d -x 60 -y 4 "$G --no-cache $D/p.gx"
screen() { tmux -L $S capture-pane -p; }
wait_for() {
    for _ in $(seq 300); do
        screen | grep -q "$1" && return 0
        sleep 0.1
    done
    echo "timed out waiting for $1"; screen; exit 2
}
wait_for 'sel=1'
echo '--- before (sel = 1)'; screen
wait_for 'sel=null'
sleep 0.5
echo '--- after (sel = null)'; screen
if screen | grep -q '>>cc'; then
    echo 'BUG: row 1 is still highlighted after sel became null'; exit 1
fi
echo 'OK: no row highlighted'
