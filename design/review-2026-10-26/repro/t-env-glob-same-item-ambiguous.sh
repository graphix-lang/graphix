#!/bin/sh
# Title: two globs that reach the SAME binding are refused as ambiguous
# Command: sh design/review-2026-10-26/repro/t-env-glob-same-item-ambiguous.sh
# Expected: 1 (both `use super::*` and `use package::*` reach the one `x`
#           at the script's top level; Rust's glob rule, which
#           design/module_system.md cites, accepts a name two globs both
#           resolve to one item).
# Observed: refused:
#   `x` is ambiguous: both `#do4611686018427395233` and `` provide it; import one explicitly
# (the message also names a lowered block scope and an empty root path).
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > $D/main.gx <<'GX'
let x = 1;
let r = {
  mod b;
  b::y
};
sys::exit(sys::time::after_idle(duration:100.ms, 0));
r
GX
cat > $D/b.gx <<'GX'
use super::*;
use package::*;
let y = x
GX
timeout -s KILL 60 $G --no-cache $D/main.gx 2>&1 | tail -1
rm -rf $D
