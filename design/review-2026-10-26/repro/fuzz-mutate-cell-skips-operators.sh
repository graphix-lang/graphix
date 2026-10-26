#!/bin/bash
# ExprTypeSite::cell is true for any node whose type is a TVar, bound or
# not, so open_init's fallback `types.cell(value)` calls an operator's,
# a negation's or a `$`/`?` result "open" and the families that use it
# (retype, widen-hop, shared-var, ref-widen, abstract-widen) skip every
# let over such an initializer, the commonest let in generated programs.
# command: bash design/review-2026-10-26/repro/fuzz-mutate-cell-skips-operators.sh
# expected: retype#3 (a writer of a disjoint type after `let v = ..`) for every line
# observed:
#   a + 1 =>
#   cast<i64>(2.5)$ =>
#   [a][0]$ =>
#   -a =>
#   (a) => retype#3
#   str::len("s") => retype#3
#   a == 1 => retype#3
d=$(dirname "$0"); t=$(mktemp --suffix=.gx)
for init in 'a + 1' 'cast<i64>(2.5)$' '[a][0]$' '-a' '(a)' 'str::len("s")' 'a == 1'; do
  printf '{ let a = 1; let v = %s; v }\n' "$init" > "$t"
  echo "$init => $("$d"/fuzz-mutate-run-typemorph-one.sh "$t" | grep -o 'retype#3' | tr '\n' ' ')"
done
rm -f "$t"
