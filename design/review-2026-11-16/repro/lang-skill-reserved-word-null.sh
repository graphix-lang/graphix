#!/bin/bash
# SKILL.md Gotchas: "Reserved words in a name position are reported by the parser
# ("`null` is a reserved word")". For `null` the parser says nothing: it is a literal
# pattern, refused later by the check; the note appears for `any`, `self`, `bytes`...
# run: bash design/review-2026-11-16/repro/lang-skill-reserved-word-null.sh
# observed:
#   let null = 1  -> match error i64 can't be matched by null
#   |null| 1      -> refutable patterns are not allowed in lambda arguments null
#   let any = 2   -> `any` is a reserved word and cannot be used as a name
G=/home/eric/tmp/review-2026-11-16/bin/graphix
d=$(mktemp -d)
for p in 'let null = 1; 1' 'let f = |null| 1; f(1)' 'let any = 2; 1'; do
  printf '%s\n' "$p" > "$d/c.gx"
  echo "== $p"
  timeout -s KILL 30 "$G" --check "$d/c.gx" 2>&1 | grep -v '^$' | tail -1
done
rm -r "$d"
