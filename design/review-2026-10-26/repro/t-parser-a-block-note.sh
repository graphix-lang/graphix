#!/bin/sh
# Title: an unclosed or stray-closed block whose first statement is a bare name gets the
#        struct-literal probe's note "the list is not closed before this `;`"
# Command: sh design/review-2026-10-26/repro/t-parser-a-block-note.sh
# Expected: no note naming a list (a block is not a list): for `{ a;` at end of input the
#           unclosed-bracket note "this `{` is not closed", as `{ a; b` gets
# Observed: note: at line: 1, column: 4: the list is not closed before this `;`
B=${B:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
for t in '{ a;' '{ a;)' 'f({ a;' '{ a; b'; do
  printf '%s' "$t" > $D/e.gx; echo "== $t"; timeout -s KILL 30 $B --check $D/e.gx 2>&1 | grep note
done
rm -rf $D
