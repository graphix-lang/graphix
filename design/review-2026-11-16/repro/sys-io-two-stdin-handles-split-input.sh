#!/bin/bash
# sys-io: two sys::io::stdin handles split the input between them and break a line in two
# Command: bash sys-io-two-stdin-handles-split-input.sh [path/to/graphix]
# Each `sys::io::stdin(trigger)` wraps a fresh tokio::io::stdin() in its own
# Halves, with its own read-ahead buffers, so two line readers over two
# handles each take whole chunks of fd 0.
# Expected: the 20000 input lines, each whole: na + nb == 20000, as with one
#   handle read twice (`let s = sys::io::stdin(null)`, lines(s) twice):
#   (12773, 7227).
# Observed: (12774, 7227) or (7227, 12774): 20001 lines, the chunk boundary
#   between the handles cut one line in two, and which reader gets what
#   varies run to run.
GX=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
DIR=$(mktemp -d)
trap 'rm -rf "$DIR"' EXIT
cat > "$DIR/p.gx" <<'GX'
use sys::io::Lines;
let a = Lines::lines(sys::io::stdin(null))$;
let b = Lines::lines(sys::io::stdin(null))$;
let both = (count(a), count(b));
sys::exit(sys::time::after_idle(duration:700.ms, both) ~ 0);
sys::time::after_idle(duration:500.ms, both)
GX
for i in 1 2 3; do seq 1 20000 | timeout -s KILL 30 "$GX" --no-cache "$DIR/p.gx"; done
