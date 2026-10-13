#!/bin/sh
# tests-tui-01: tui::scrollbar passes unclamped Index counts into ratatui's
# Scrollbar::part_lengths, whose rounding_divide overflows.
# Run: sh design/review-2026-10-12/repro/tests-tui-01.sh /path/to/debug/graphix
# Expected: both programs draw a scrollbar beside "body" and exit 0.
# Observed (debug build): each prints
#   panicked at .../ratatui-widgets-0.3.2/src/scrollbar.rs:562:13:
#   attempt to add with overflow ... Error: the display failed
# and exits 1. A release build wraps instead and draws the thumb in the
# wrong place.
G=${1:-graphix}
D=$(mktemp -d)
cat > "$D/pos.gx" <<'GX'
use tui::{scrollbar::scrollbar, paragraph::paragraph};
sys::exit(sys::time::after_idle(duration:500.ms, 0));
scrollbar(#position: &9223372036854775806, #content_length: &9223372036854775807, &paragraph(&"body"))
GX
cat > "$D/vp.gx" <<'GX'
use tui::{scrollbar::scrollbar, paragraph::paragraph};
sys::exit(sys::time::after_idle(duration:500.ms, 0));
scrollbar(#position: &0, #content_length: &10, #viewport_length: &9223372036854775807, &paragraph(&"body"))
GX
for f in pos vp; do
  echo "== $f"
  timeout -s KILL 60 script -qec "stty rows 10 cols 40; $G --no-cache $D/$f.gx" /dev/null 2>&1 \
    | grep -a -o "panicked at[^']*\|attempt to [a-z ]*\|Error: [a-z ]*" | sort -u
done
rm -rf "$D"
