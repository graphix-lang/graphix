#!/bin/sh
# Title: the printer reads a spliced string constant as text (StrPart::of_all), so
#        graphix fmt rewrites `["q"]` into the text `q` and refuses a raw splice.
# Command: sh design/review-2026-10-19/repro/tests-expr-splice-as-text.sh
# Expected: each line formats to itself.
# Observed:
#   let a = "a["b"]["q"]";   ->  let a = "a["b"]q";      (splice silently became text)
#   let b = "["b"][r"q"]";   ->  let b = "b[r"q"]";      (splice silently became text)
#   let c = "[x][r"q"]";     ->  formatter bug: the formatted text changed a string's delimiters
#   let d = "[r"b"][r"q"]";  ->  formatter bug: the formatted text changed a string's delimiters
# The expr_round_trip / expr_pp_round_trip properties cannot see this: check()'s
# acc_strings merges spliced string constants into the neighbouring text, and
# str_forms skips every string constant inside an interpolation.
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
for l in 'let a = "a["b"]["q"]";' 'let b = "["b"][r"q"]";' 'let c = "[x][r"q"]";' 'let d = "[r"b"][r"q"]";'; do
  printf '%s\nnull\n' "$l" > "$D/t.gx"
  echo "== $l"
  timeout -s KILL 30 "$G" fmt --stdout "$D/t.gx" 2>&1 | head -1
done
rm -rf "$D"
