#!/bin/sh
# Title: an interface file refuses comments below its last item (a commented-out last
#        `val`) and refuses a file of only comments; an empty .gxi is accepted
# Command: sh design/review-2026-10-26/repro/t-parser-a-gxi-trailing-comment.sh
# Expected: both programs check (CLAUDE.md: comments are legal "below a list's last
#           item or a file's"), as the same comments in a .gx do
# Observed: exit 1, "Parse error ... Unexpected end of input" after the trailing
#           comment, and the same for the comment-only interface
B=${B:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
printf 'mod m;\nm::x\n' > $D/main.gx
printf 'let x = 1;\nlet y = 2\n' > $D/m.gx
printf 'val x: i64;\n// val y: i64\n' > $D/m.gxi
timeout -s KILL 30 $B --check $D/main.gx > $D/out 2>&1; echo "trailing exit $?"; grep -A1 'Parse error' $D/out
printf 'mod m;\n1\n' > $D/main.gx
printf '// nothing yet\n' > $D/m.gxi
timeout -s KILL 30 $B --check $D/main.gx > $D/out 2>&1; echo "comment-only exit $?"; grep -A1 'Parse error' $D/out
printf '' > $D/m.gxi
timeout -s KILL 30 $B --check $D/main.gx > $D/out 2>&1; echo "empty exit $?"
rm -rf $D
