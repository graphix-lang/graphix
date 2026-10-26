#!/bin/sh
# Title: a type laid out flat overruns the width by what follows it: a let
#   annotation's ` =` and a hugged type's closing brackets and `;`
#   (x-formatter-03 variant, in the type printer)
# Command: sh x-rerun-3-fmt-type-hug-width.sh [graphix]
# Expected: every line within the default width of 90 (the type breaks as it does
#   one character longer).
# Observed: "92|let zz: Array<{ a..58..a: i64, b: i64 }> =" and
#   "92|type T = Array<`A({ a..62..a: i64 })>;"
G=${1:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
A58=$(printf 'a%.0s' $(seq 1 58)); A62=$(printf 'a%.0s' $(seq 1 62))
printf 'let zz: Array<{%s: i64, b: i64}> = [];\nzz\n' "$A58" > "$D/a.gx"
printf 'type T = Array<`A({%s: i64})>;\n1\n' "$A62" > "$D/b.gx"
for f in a b; do timeout -s KILL 10 "$G" fmt --stdout "$D/$f.gx" | awk '{printf "%3d|%s\n", length($0), $0}'; done
rm -rf "$D"
