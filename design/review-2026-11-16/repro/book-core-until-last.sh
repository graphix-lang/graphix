#!/bin/sh
# Title: seq.md says `until` cannot end "a body whose value is used"; it cannot end any seq body
# Command: sh design/review-2026-11-16/repro/book-core-until-last.sh
# Expected (seq.md "until cond;"): a seq whose value nothing uses may end in `until live(ready)`.
# Observed: refused, "until has no value: the last statement of a seq, or of a try or with body
#   whose value is used, must be an expression".
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d /home/eric/tmp/review-2026-11-16/work/book-core/until.XXXX)
printf 'let ready = true;\nlet go = 1;\nseq go { let a = 1; until live(ready) };\n2\n' > "$D/p.gx"
timeout -s KILL 10 $G --no-cache --check "$D/p.gx"; echo "exit $?"
