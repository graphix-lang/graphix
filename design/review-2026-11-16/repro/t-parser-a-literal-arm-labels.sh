#!/bin/sh
# t-parser-a: the literal parser's diagnostic arms say things where they do
# not apply. After an in-range integer the report says "Unexpected integer
# literal" (int_range_note's label) and "Expected .." (the slice-bound arm's
# lookahead), though `..` means something only inside `a[..]`; an
# out-of-range one adds "expected int" (arrayexp::idx's refusal).
#
# Run: sh design/review-2026-11-16/repro/t-parser-a-literal-arm-labels.sh
# Expected: `5ms` says "Unexpected `m`" as `5.0ms` does, nothing about an
#           integer literal or `..`; the out-of-range literal is told by its
#           note alone.
# Observed (HEAD e1be2fbf2): "Unexpected `m` / Unexpected integer literal /
#           Expected .."; "Unexpected integer literal / expected int /
#           invalid signed integer".
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
probe() {
  printf '%s\n' "$1" > "$d/p.gx"
  echo "== $1"
  timeout -s KILL 10 "$G" --check --no-cache "$d/p.gx" 2>&1 | sed -n '5,10p'
}
probe 'let x = 5.0ms;'
probe 'let x = 5ms;'
probe 'let x = 3x + 1;'
probe 'let x = 99999999999999999999;'
rm -rf "$d"
