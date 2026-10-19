#!/bin/sh
# Title: a struct literal's error past its first field is reported early, without its reason
# Command: sh tests-parser-struct-literal-diagnostics.sh
# Expected: `{ a: 2, b c }` reported at `c` (column 19) with the note
#   "a `,` separates this item from the one before", as `f(a b)` and `[1 2]` are;
#   `{ a: 2, type }` with "a reserved word field needs the explicit `name: value` form",
#   as `{ s with type }` is.
# Observed: both reported at column 15 (the `,` before the bad field) as
#   "the parser could not continue past this point", with no note.
G=/home/eric/tmp/review-2026-10-19/bin/graphix
D=$(mktemp -d)
for s in '{ a: 2, b c }' '{ a: 2, type }' '{ s with type }'; do
  printf 'let r = %s;\nr\n' "$s" > "$D/p.gx"
  echo "== $s"
  timeout -s KILL 10 "$G" --check "$D/p.gx" 2>&1 | sed -n '5,9p'
done
rm -rf "$D"
