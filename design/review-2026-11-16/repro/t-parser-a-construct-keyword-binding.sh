#!/bin/sh
# t-parser-a: a construct keyword (`type`, `select`, `let`, ...) written as a
# binding name is refused with the caret AFTER the word, often blaming the
# next token ("Unexpected `|`", "Unexpected `)`"), and with no note naming
# the word; any other reserved word (`abort`) gets the note at the word.
# fname_noting refuses with `because` at the input's point (past the word)
# and notes only non-construct words.
#
# Run: sh design/review-2026-11-16/repro/t-parser-a-construct-keyword-binding.sh
# Expected: each case reported at the word, with "`type` is a reserved word
#           and cannot be used as a name" as the `abort` case has.
# Observed (HEAD e1be2fbf2): caret one past the word, "Unexpected `|`" /
#           "Unexpected `)`", no note.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
probe() {
  printf '%s\n' "$1" > "$d/p.gx"
  echo "== $1"
  timeout -s KILL 10 "$G" --check --no-cache "$d/p.gx" 2>&1 | sed -n '5,$p'
}
probe 'let f = |abort| 1; 2'
probe 'let f = |type| 1; 2'
probe 'let (a, select) = (1, 2); a'
probe 'let f = |a| a; f(#type: 1)'
rm -rf "$d"
