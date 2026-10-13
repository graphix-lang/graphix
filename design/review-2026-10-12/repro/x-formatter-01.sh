#!/bin/sh
# x-formatter-01: CRLF path rewrites quoted multi-line string literals: fmt silently changes program meaning
# usage: sh design/review-2026-10-12/repro/x-formatter-01.sh <graphix binary>
# Runs x-formatter-01.gx (every line CRLF, a quoted string "a<CR><LF>b")
# before and after `graphix fmt --stdout`.
# expected: before: 4 / after: 4 (or fmt refuses the file)
# observed: before: 4 / after: 3 -- fmt exits 0 having printed the literal as "a\nb"
G=${1:-graphix}
D=$(dirname "$0")
T=$(mktemp --suffix=.gx)
"$G" fmt --stdout "$D/x-formatter-01.gx" > "$T"; echo "fmt exit: $?"
echo "before: $(timeout -s KILL 30 "$G" --no-cache "$D/x-formatter-01.gx" 2>&1 | tail -1)"
echo "after:  $(timeout -s KILL 30 "$G" --no-cache "$T" 2>&1 | tail -1)"
rm -f "$T"
