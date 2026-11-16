#!/bin/sh
# t-parser-a: a syntax error inside a struct literal's field, a connect's
# right side, a seq's trigger, a single-quoted string's splice or a lambda's
# return type is
# reported as "the parser could not continue past this point", with no
# Unexpected/Expected body; the same error anywhere else names what was
# expected. The editor (ParseFailure::summary) shows the same bare text.
#
# Run: sh design/review-2026-11-16/repro/t-parser-a-wide-attempt-diagnostics.sh
# Expected: every case says "Unexpected `}`" (or `;`, `[`) and lists what was
#           expected, as the CONTROL cases do.
# Observed (HEAD e1be2fbf2): the CONTROL cases do; every other case prints
#           only "the parser could not continue past this point" (the splice
#           case adds the note telling the author to escape the `[`).
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
probe() {
  printf '%s\n' "$2" > "$d/p.gx"
  echo "== $1: $2"
  timeout -s KILL 10 "$G" --check --no-cache "$d/p.gx" 2>&1 | sed -n '5,7p'
}
probe CONTROL 'let x = (1 + ;'
probe CONTROL 'let s = """a \[1 + ] b"""; s'
probe struct  'let s = {a: (1 + }; s'
probe struct  'let x = {a: {a: 1 +}}'
probe connect 'let x = 1; x <- (1 + ; x'
probe connect 'let x = 1; x <- f(1,, 2); x'
probe splice  'let s = "a [1 + ] b"; s'
probe rtype   'let f = |x| -> Array<i64 [x]; f(1)'
probe trigger 'let t = 1; let s = seq (t + ) { 1 }; s'
rm -rf "$d"
