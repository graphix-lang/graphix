#!/bin/sh
# t-parser-a: a bad escape in a """template""" is reported at the `\` as
# "Expected `\"\"\"`", where the same escape in a quoted string names the
# escapes it could be, at the char after the `\`. triple_escape wraps the
# `\` and its char in one attempt, so the failure rolls back to the `\` and
# the template's closer is all that is expected there.
#
# Run: sh design/review-2026-11-16/repro/t-parser-a-template-escape.sh
# Expected: the template cases say "Unexpected `q`" (`]`) at column 14 and
#           list the template's escapes (n r t 0 " \ [), as the quoted case
#           does for its own.
# Observed (HEAD e1be2fbf2): "Unexpected `\` / Expected \"\"\"" at column 13.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
probe() {
  printf '%s\n' "$1" > "$d/p.gx"
  echo "== $1"
  timeout -s KILL 10 "$G" --check --no-cache "$d/p.gx" 2>&1 | sed -n '5,7p'
}
probe 'let s = "a\qb"; s'
probe 'let s = """a\qb"""; s'
probe 'let s = """a\]b"""; s'
rm -rf "$d"
