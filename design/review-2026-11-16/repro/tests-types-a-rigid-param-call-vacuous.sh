#!/bin/sh
# Title: unsound_acceptances::RIGID_PARAM_CALL pins "does not contain", which
#   the program without rigidity gives too, at the call.
# Command: sh design/review-2026-11-16/repro/tests-types-a-rigid-param-call-vacuous.sh
# Expected: only line 1 (the fixture) and line 3 refused by rigidity.
# Observed (HEAD e1be2fbf2):
#   1 (the fixture):            'a: unbound does not contain string
#   2 (same body, 'a := string, i.e. rigidity lost): string does not contain i64
#     -> also contains "does not contain": the fixture passes either way
#   3 (a call only rigidity refuses): 'a: unbound does not contain string
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
i=0
while IFS= read -r p; do
  i=$((i+1)); printf '%s\n' "$p" > $d/p.gx
  echo "== $i: $p"
  timeout -s KILL 30 $G --no-cache --check $d/p.gx 2>&1 | grep -v ' at: ' | tail -1
done <<'PROGS'
{ let m = |a: 'a, f: fn(x: 'a) -> 'b| -> 'b f("a"); m(1, |x| x * 2) }
{ let m = |a: string, f: fn(x: string) -> 'b| -> 'b f("a"); m(1, |x| x * 2) }
{ let m = |a: 'a, f: fn(x: 'a) -> 'b| -> 'b f("a"); m("s", |x| str::len(x)) }
PROGS
rm -rf $d
