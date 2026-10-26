#!/bin/sh
# Title: a comment ending a line, or below a list, inside a string splice is accepted,
#        and the formatter then refuses the program as its own bug
# Command: sh design/review-2026-10-26/repro/t-parser-a-splice-eol-comment.sh
# Expected: --check refuses both programs as it refuses an own-line comment in a splice
#           ("a comment cannot stand inside a string's splice")
# Observed: --check accepts both (exit 0); `graphix fmt --stdout` prints
#           "formatter bug: the formatted text lost or moved a comment or an attribute"
B=${B:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
printf 'let s = "[{ let x = 1; // hi\n x }]";\ns\n' > $D/eol.gx
printf 'let s = "[{ let x = 1;\n x\n // below\n }]";\ns\n' > $D/below.gx
for f in $D/eol.gx $D/below.gx; do
  timeout -s KILL 30 $B --check $f; echo "check exit $?"
  timeout -s KILL 30 $B fmt --stdout $f; echo "fmt exit $?"
done
rm -rf $D
# Also: an attribute in a splice is accepted and the formatter then breaks the splice
# across lines (observed: `"a [{ ` / `#[pure]` / `let y = x; y }] b"`), against
# "a splice prints on its string's line".
D=$(mktemp -d)
printf 'let x = 1;\nlet s = "a [{ #[pure] let y = x; y }] b";\ns\n' > $D/attr.gx
timeout -s KILL 30 $B fmt --stdout $D/attr.gx; echo "fmt exit $?"
rm -rf $D
