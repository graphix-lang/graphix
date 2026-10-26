#!/bin/sh
# Title: a paragraph cannot scroll past line 65535: scroll.y is read as
# validate::Offset (clamped to u16::MAX for ratatui's Paragraph::scroll),
# so a 70000-line text scrolled to its tail shows line 65535 instead; the
# clamp only logs a warning.
# Command: sh design/review-2026-10-26/repro/tui-widgets-paragraph-scroll-cap.sh
# Expected: line 69995 .. line 69999
# Observed: line 65535 .. line 65539
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > $D/p.gx <<'GX'
use tui::paragraph::paragraph;

let text = str::join(#sep: "\n", array::init(70000, |i| "line [i]"));
paragraph(#wrap: &false, #scroll: &{x: 0, y: 69995}, &text)
GX
S=para$$
tmux -L $S new-session -d -x 30 -y 5 "timeout --foreground -s KILL 15 $G --no-cache $D/p.gx; sleep 20"
sleep 8; tmux -L $S capture-pane -p
tmux -L $S kill-server
rm -rf $D
