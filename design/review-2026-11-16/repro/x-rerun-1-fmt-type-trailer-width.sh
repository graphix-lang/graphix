#!/bin/sh
# Title: a type laid out flat still overruns the width by what follows it in
#   two more places: a select arm's written type test (` as q =>`) and a
#   labeled default's annotation (` =`) (variant of x-rerun-3-07, fixed for a
#   let annotation and a hugged type's closers).
# Command: sh design/review-2026-11-16/repro/x-rerun-1-fmt-type-trailer-width.sh [graphix]
# Expected: every line within the default width of 90 (the type breaks, as it
#   does a few characters longer).
# Observed (k = field-name length):
#   k=63..69: " 91..97|    Array<{ a..k..a: i64 }> as q =>"   (k=70 breaks the type)
#   k=59:     " 91|    #d: Map<string, { a..59..a: i64 }> ="    (k=60 breaks it)
G=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
for k in 63 66 69 70; do
  A=$(printf 'a%.0s' $(seq 1 $k))
  printf 'let v: [i64, Array<{%s: i64}>] = 1;\nselect v { Array<{%s: i64}> as q => 1, _ => 2 }\n' "$A" "$A" > "$D/s.gx"
  timeout -s KILL 10 "$G" fmt --stdout "$D/s.gx" | awk -v k=$k 'length($0) > 90 {printf "select k=%d %3d|%s\n", k, length($0), $0}'
done
A=$(printf 'a%.0s' $(seq 1 59))
printf 'let h = |#d: Map<string, {%s: i64}> = {}| 1;\nh()\n' "$A" > "$D/d.gx"
timeout -s KILL 10 "$G" fmt --stdout "$D/d.gx" | awk 'length($0) > 90 {printf "default %3d|%s\n", length($0), $0}'
rm -rf "$D"
