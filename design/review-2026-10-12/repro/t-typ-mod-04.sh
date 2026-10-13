#!/bin/bash
# t-typ-mod-04: block-local Abstract types in two REPL inputs share one AbstractId
# (AbstractId::declared keys a body-declared type by `Source`'s Display, which is
# "expr" for every rt.compile input, plus line:column; two inputs that declare
# `type T` in a block at the same column mint the same nominal type).
#
# command: bash design/review-2026-10-12/repro/t-typ-mod-04.sh [path-to-graphix]
# expected: case 1 prints 0 (a's T is not f's T); case 2 accepts b (a fresh type)
# observed: case 1 prints 42; case 2 refuses with
#   "T is already defined as Abstract<i64>: a value of it may still be held,
#    so its representation cannot change"
# (the REPL may not exit at end of input: the timeout ends it)
# control: shifting line 2's `type T` one column right makes case 1 print 0.
G=${1:-graphix}
echo "case 1 (identity):"
printf '%s\n' \
  'let a = { type T = Abstract<i64>; T(41) }' \
  'let f = { type T = Abstract<i64>; |x: Any| select x { T as o => o.0 + 1, _ => 0 } }' \
  'f(a)' | timeout -s KILL 15 "$G" --no-cache --no-netidx 2>&1
echo "case 2 (spurious refusal):"
printf '%s\n' \
  'let a = { type T = Abstract<i64>; T(1) }' \
  'let b = { type T = Abstract<string>; T("x") }' \
  'b' | timeout -s KILL 15 "$G" --no-cache --no-netidx 2>&1
