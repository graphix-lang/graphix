#!/bin/sh
# Title: tui block #size reports the inner area, not "the rendered size of the block"
# Command: sh design/review-2026-11-16/repro/book-ui-a-block-size-is-inner.sh
# Expected (book/src/ui/tui/block.md, "size (output) - Rendered size of the
#   block"): a bordered block filling a 40x10 terminal reports 40x10.
# Observed: it reports 38x8, the area inside the borders (block.rs draw reports
#   block.inner(rect)); the tui test a_zero_area_is_reported_on_the_first_draw
#   pins the same.
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
cat > "$d/p.gx" <<'EOF'
use tui::{Size, block::block, text::text};
let size: Size = never();
block(#border: &`All, #size: &mut size, &text(&"size [size.width]x[size.height]"))
EOF
TERM=xterm timeout -s KILL 4 script -qfc "stty cols 40 rows 10; $GX --no-cache $d/p.gx" /dev/null 2>&1 \
  | head -c 200000 | grep -ao '[0-9]*x[0-9]*' | sort -u
rm -rf "$d"
