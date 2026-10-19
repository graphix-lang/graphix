#!/bin/sh
# Title: a `///` doc written after an interface item on the same line is
# accepted and becomes the NEXT item's doc; a trailing `//` is refused.
# Command: sh design/review-2026-10-19/repro/t-parser-b-trailing-doc.sh
# Expected: m.gxi is refused like t2.gxi ("a comment stands on a line of its
# own, above what it describes").
# Observed: m.gxi formats to `val x: i64;` then `/// trailing doc` above
# `val y: i64` (the doc moved to y); t2.gxi is refused.
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
H=$(dirname "$0")/t-parser-b-trailing-doc
echo "== m.gxi (trailing ///)"; timeout -s KILL 30 $G fmt --stdout $H/m.gxi; echo "rc=$?"
D=$(mktemp -d); printf 'val x: i64; // trailing\nval y: i64\n' > $D/t2.gxi
echo "== t2.gxi (trailing //)"; timeout -s KILL 30 $G fmt --stdout $D/t2.gxi 2>&1 | grep note; rm -rf $D
