#!/bin/sh
# fuzz-lib-b-01: `// expect: reject` makes every marked pin a parse error,
# so the 81 `reject` rows of graphix-fuzz/outcome.manifest guard nothing.
#
# build.rs::strip_header keeps the marker as the program's first line and
# Subject::parse prepends `use super::*; let result = ` on that same line,
# where a comment is a parse error. A well-typed program carrying the
# marker therefore records verdict `reject`.
#
# Command: sh fuzz-lib-b-01.sh <path-to-graphix-fuzz>
# Expected: both programs AGREE (trace) -- the marker is oracle data.
# Observed: marked -> AGREE (reject); `graphix-fuzz run` shows
#   Parse error at line 1, column 28 ... `use super::*; let result = // expect: reject`
#   note: a comment stands on a line of its own, above what it describes
# Same for real pins: the embedded (header-stripped) form of
# tvar-lock-jul2026/01_constraint_eq_self_deadlock.gx parse-fails; without
# the marker and its trailing comments it runs to Trace([[2], [<fn>]]).
FUZZ=${1:-graphix-fuzz}
D=$(mktemp -d)
printf '// expect: reject\ni64:1 + i64:2\n' > "$D/marked.gx"
printf 'i64:1 + i64:2\n' > "$D/plain.gx"
echo "marked:"; timeout -s KILL 180 "$FUZZ" check "$D/marked.gx"
echo "plain:";  timeout -s KILL 180 "$FUZZ" check "$D/plain.gx"
timeout -s KILL 180 "$FUZZ" run "$D/marked.gx" 2>&1 | head -1 | cut -c1-200
rm -rf "$D"
