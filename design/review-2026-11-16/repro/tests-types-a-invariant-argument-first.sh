#!/bin/sh
# Title: CLAUDE.md says a call's arguments are met in parameter order and no
#   rule may make a verdict independent of that order; the tree checks an
#   argument whose formal holds the variable invariantly (an abstract type, a
#   type application, a writable reference) first, whatever the order
#   (tvar_constraints.md, "A call's arguments are met in order"; pin
#   lang::types::an_invariant_argument_decides_first).
# Command: sh design/review-2026-11-16/repro/tests-types-a-invariant-argument-first.sh
# Expected under CLAUDE.md's text: line 1 refused ("aa" decides 'a := string
#   first, and Bx<string> cannot hold Bx<[i64, string]>, line 2).
# Observed (HEAD e1be2fbf2): line 1 accepted; line 2 refused
#   "Bx<string> does not contain Bx<[i64, string]>".
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
i=0
while IFS= read -r p; do
  i=$((i+1)); printf '%s\n' "$p" > $d/p.gx
  echo "== $i: $p"
  timeout -s KILL 30 $G --no-cache --check $d/p.gx 2>&1 | grep -v ' at: ' | tail -1
done <<'PROGS'
{ type Bx<'a> = Abstract<Array<'a>>; let f = |k: 'a, b: Bx<'a>| -> i64 1; let b: Bx<[string, i64]> = Bx(["x"]); f("aa", b) }
{ type Bx<'a> = Abstract<Array<'a>>; let b: Bx<[string, i64]> = Bx(["x"]); let c: Bx<string> = b; 1 }
PROGS
rm -rf $d
