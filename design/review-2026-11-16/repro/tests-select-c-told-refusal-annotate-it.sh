#!/bin/sh
# Title: the "may share ...'s runtime form: annotate it" refusal names no
#   binding and no annotation; the fix differs by case and is never shown.
# Command: sh tests-select-c-told-refusal-annotate-it.sh
# Expected: each refusal names what to write (annotate `a`; bound 'a), as the
#   pattern.rs refusals do ("bound 'a by the type it tests, `'a: T`, ...").
# Observed (HEAD e1be2fbf2):
#   == rest-unannotated: this pattern tests for `Keep('_N: unbound) among '_M: unbound,
#        but a type variable there isn't known here and may share `Keep('_N: unbound)'s
#        runtime form: annotate it
#   == rest-annotated:   (checks: annotating the parameter `a` is the fix)
#   == written:          ... among 'a: unbound ... : annotate it
#   == written-bounded:  (checks: bounding 'a is the fix, not an annotation)
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/rest-unannotated.gx <<'GX'
let f = |a| select a { [`Keep(r), ..] => r, _ => null };
f([`Keep(1)])
GX
cat > $D/rest-annotated.gx <<'GX'
let f = |a: Array<`Keep(i64)>| select a { [`Keep(r), ..] => r, _ => null };
f([`Keep(1)])
GX
cat > $D/written.gx <<'GX'
let f = |v: 'a| select v { Array<'b> as xs => xs, _ => never() };
f([1])
GX
cat > $D/written-bounded.gx <<'GX'
let f = 'a: [Array<i64>, i64] |v: 'a| select v { Array<i64> as xs => xs, _ => never() };
f([1])
GX
for f in rest-unannotated rest-annotated written written-bounded; do
  echo "== $f"
  timeout -s KILL 60 $G --no-cache --check $D/$f.gx 2>&1 | tail -1 | grep -v '^$' | sed 's/^ *[0-9]*: //'
done
rm -rf $D
