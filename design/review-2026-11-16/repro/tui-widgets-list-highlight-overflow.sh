#!/bin/bash
# Title: a list's highlight symbol is handed to ratatui uncut: one 65535 columns
# wide in a list not at column 0 overflows ratatui's u16 x (a panic under
# overflow checks, a wrapped x in release).
# Command: bash design/review-2026-11-16/repro/tui-widgets-list-highlight-overflow.sh
# Expected: the list drawn in its block, the symbol cut to the list's width.
# Observed (debug binary, 40x10 pty):
#   panicked at ratatui-widgets-0.3.2/src/list/rendering.rs:97:24:
#   attempt to add with overflow   (row_area.x + highlight_symbol_width)
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
python3 -I - "$D" <<'PY'
import sys
s = 'x' * 65535
open(f'{sys.argv[1]}/hl.gx', 'w').write(f'''use tui::{{line, block::block, list::list}};

block(#border: &`All, &list(#selected: &0, #highlight_symbol: &"{s}", &[line("one"), line("two")]))
''')
PY
timeout -s KILL 40 script -qec "stty rows 10 cols 40; timeout -s KILL 10 $G --no-cache $D/hl.gx" /dev/null 2>&1 | grep -a -A1 panicked | head -2
rm -rf $D
