#!/bin/sh
# Title: a pattern that leaves struct fields out inside an abstract type's
#        payload is taken for one that needs the scrutinee to complete it:
#        a let is refused "match error W can't be matched by W", a select
#        over Any "has no struct type to complete it", a select over an
#        unannotated parameter "a type not decided at this select". The
#        payload's struct is the declared representation; the same patterns
#        naming every field check.
#
# Command: sh design/review-2026-11-16/repro/c-pattern-partial-struct-under-abstract.sh
#
# Expected: all ten check (each odd line differs from the line below it
#   only in `..` against `b: _`).
# Observed:
#   1 match error W can't be matched by W
#   2 ok
#   3 match error (W, i64) can't be matched by (W, '_NNNN: unbound)
#   4 ok
#   5 the partial struct pattern W({ a, .. }) over Any has no struct type to
#     complete it: test the struct's type first (`{a: i64, b: string} as {a, ..}`)
#   6 ok
#   7 W({ a, .. }) tests a value of 'x, a type not decided at this select, ...
#   8 ok
#   9 ok
#   10 ok
G=/home/eric/tmp/review-2026-11-16/bin/graphix
d=$(mktemp -d)
n=0
while IFS= read -r p; do
  n=$((n+1))
  printf 'type W = Abstract<{a: i64, b: i64}>;\n%s\n' "$p" > "$d/p.gx"
  if out=$(timeout -s KILL 30 "$G" --no-cache --check "$d/p.gx" 2>&1); then
    echo "$n ok"
  else
    echo "$n $(printf '%s\n' "$out" | grep -E '^    [0-9]+: ' | grep -v ': at: ' | tail -1 | sed -E 's/^ +[0-9]+: //')"
  fi
done <<'PROGS'
let w = W({a: 1, b: 2}); let W({a, ..}) = w; a
let w = W({a: 1, b: 2}); let W({a, b: _}) = w; a
let w = W({a: 1, b: 2}); let (W({a, ..}), n) = (w, 1); a
let w = W({a: 1, b: 2}); let (W({a, b: _}), n) = (w, 1); a
|x: Any| select x { W({a, ..}) => a, _ => 0 }
|x: Any| select x { W({a, b: _}) => a, _ => 0 }
let f = |x| select x { W({a, ..}) => a }; f(W({a: 1, b: 2}))
let f = |x| select x { W({a, b: _}) => a }; f(W({a: 1, b: 2}))
let w = W({a: 1, b: 2}); select w { W({a, ..}) => a }
let f = |W({a, ..})| a; f(W({a: 1, b: 2}))
PROGS
rm -rf "$d"
