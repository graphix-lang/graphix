#!/bin/sh
# t-print: a value ending in `.field`/`[i]` after a call does not follow its head, while
# the same call ending in `?` does (opens_with_bracket has no arm for the postfix accessors).
# command: sh t-print-postfix-head.sh
# expected (CLAUDE.md Formatter layout: a value opening with a bracket whose first line
#   fits follows its head):  let y = some_function(\n    aaaa,\n ...\n).field;
# observed:  let y =\n    some_function(\n        aaaa, ...\n    ).field;
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
printf 'let y = some_function(aaaaaaaaaaaa, bbbbbbbbbbbbbbb, cccccccccc)?;\nlet z = some_function(aaaaaaaaaaaa, bbbbbbbbbbbbbbb, cccccccccc).field;\nz\n' > $d/a.gx
$G fmt --stdout --width 40 $d/a.gx
rm -rf $d
