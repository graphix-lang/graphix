#!/bin/sh
# Title: a standing read through a place that does not exist logs a warning,
# with no source position, at every cycle its reader is walked
#
# Command: sh design/review-2026-11-16/repro/c-bind-missing-place-read-logs-every-cycle.sh
#
# `r` is `&a[i]`; `i` moves past the end once, at c = 1. `*r` sits in a
# statement that `c` walks every cycle. Nothing `*r` reads fires after c = 1.
#
# Expected: one report of the failed read, at the move, naming where `*r` or
# `&a[i]` is written, under graphix_compiler::FAILURE_TARGET (CLAUDE.md: "A
# running program's failures are logged under FAILURE_TARGET"), as the
# dropped write through the same reference is reported.
# Observed: 10 lines of
#   WARN [graphix_compiler::node::bind] read through a reference: index 9 out of range for length 3
# one from ByRef::update at the move and one from Deref::update per cycle.
BIN=${BIN:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > "$D/p.gx" <<'GX'
let c = array::iter([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
let a = [10, 20, 30];
let i = 0;
i <- select c { 0 => 9, _ => never() };
let r = &a[i];
let out = (c, *r);
sys::exit(sys::time::after_idle(duration:200.ms, 0));
out
GX
RUST_LOG=warn timeout -s KILL 20 "$BIN" --no-cache --log-dir "$D/log" "$D/p.gx"
sort "$D/log/graphix.log" | uniq -c
rm -rf "$D"
