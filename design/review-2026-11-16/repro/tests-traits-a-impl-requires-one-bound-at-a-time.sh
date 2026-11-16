#!/bin/sh
# Title: "the implementation requires 'a: X, which the signature does not
#   declare" names one missing conjunct at a time: an arithmetic method or val
#   needs 'a: Number + Singleton, the first refusal says Number, and following it
#   gives a second refusal for Singleton.
# Command: sh design/review-2026-11-16/repro/tests-traits-a-impl-requires-one-bound-at-a-time.sh
# Expected: (1) names every bound the implementation needs, 'a: Number + Singleton.
# Observed: (1) "requires 'a: Number"; (2), which declares exactly that, is refused
#   "requires 'a: Singleton"; (3) passes. The same holds for a .gxi val.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
body='impl Twice for Counter { let twice = |c, x| x + x };
Twice::twice(Counter(1), 2)'
for v in "1:fn(self, x: 'a) -> 'a" "2:fn<'a: Number>(self, x: 'a) -> 'a" "3:fn<'a: Number + Singleton>(self, x: 'a) -> 'a"; do
  n=${v%%:*}; sig=${v#*:}
  printf 'type Counter = Abstract<i64>;\ntrait Twice { val twice: %s };\n%s\n' "$sig" "$body" > $d/$n.gx
  echo "== ($n) val twice: $sig"
  out=$(timeout -s KILL 30 $G --no-cache --check $d/$n.gx 2>&1); rc=$?
  echo "$out" | tail -1; echo "exit $rc"
done
rm -rf $d
