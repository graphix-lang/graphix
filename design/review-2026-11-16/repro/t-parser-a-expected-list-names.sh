#!/bin/sh
# t-parser-a: where an expression is expected, the report's Expected list
# names `r` (raw_string's first char, as if `r` alone were a token), shows a
# variant's backtick as ``` (Info::Token's "`{c}`" around a backtick), and
# names `package`, `self` and `super` but never a name, the commonest thing
# that can stand there: ident_str's take_while1 fails with mismatch(),
# which expects nothing.
#
# Run: sh design/review-2026-11-16/repro/t-parser-a-expected-list-names.sh
# Expected: the list names "a name" (or "an identifier"), "a raw string"
#           rather than `r`, and a backtick that reads as one.
# Observed (HEAD e1be2fbf2): "... `!`, `r`, [<, `[`, ..., seq, ```, cast,
#           ..., package, self or super".
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
printf 'let x = 1 + ;\n' > "$d/p.gx"
timeout -s KILL 10 "$G" --check --no-cache "$d/p.gx" 2>&1 | sed -n '5,7p'
rm -rf "$d"
