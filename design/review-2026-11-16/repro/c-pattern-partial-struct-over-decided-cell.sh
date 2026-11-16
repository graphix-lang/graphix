#!/bin/sh
# Title: a partial struct pattern over a struct type a decided cell holds is
#        refused, "non exhaustive struct matches require type annotations",
#        though the type is known: a callback's `|{a, ..}| a`, a `let {a, ..}
#        = x` in its body, an abstract's payload, where even the written test
#        the message asks for is refused. The same pattern naming every
#        field, or standing where the type holds the struct directly, checks.
#
# Command: sh design/review-2026-11-16/repro/c-pattern-partial-struct-over-decided-cell.sh
#
# Expected: all eleven check (each struct type is known where the pattern
#   compiles; the even lines 2, 4, 6, 8 and line 11 show the check has it).
# Observed:
#   1 non exhaustive struct matches require type annotations
#   2 ok
#   3 non exhaustive struct matches require type annotations
#   4 ok
#   5 non exhaustive struct matches require type annotations
#   6 ok
#   7 non exhaustive struct matches require type annotations
#   8 ok
#   9 non exhaustive struct matches require type annotations
#   10 non exhaustive struct matches require type annotations
#   11 ok
G=/home/eric/tmp/review-2026-11-16/bin/graphix
d=$(mktemp -d)
n=0
while IFS= read -r p; do
  n=$((n+1))
  printf '%s\n' "$p" > "$d/p.gx"
  if out=$(timeout -s KILL 30 "$G" --no-cache --check "$d/p.gx" 2>&1); then
    echo "$n ok"
  else
    echo "$n $(printf '%s\n' "$out" | grep -E '^    [0-9]+: ' | grep -v ': at: ' | tail -1 | sed -E 's/^ +[0-9]+: //')"
  fi
done <<'PROGS'
let xs = [{a: 1, b: 2}]; array::map(xs, |{a, ..}| a)
let xs = [{a: 1, b: 2}]; array::map(xs, |{a, b: _}| a)
let xs = [{a: 1, b: 2}]; array::map(xs, |x| { let {a, ..} = x; a })
let xs = [(1, {a: 1, b: 2})]; array::map(xs, |(n, {a, ..})| a)
let m = {"k" => {a: 1, b: 2}}; map::map(m, |(k, {a, ..})| (k, a))
let m = {"k" => {a: 1, b: 2}}; map::map(m, |(k, {a, b: _})| (k, a))
let id = |x| x; let s = id({a: 1, b: 2}); let xs = [(1, s)]; array::map(xs, |(n, {a, ..})| a)
let id = |x| x; let s = id({a: 1, b: 2}); let xs = [(1, s)]; array::map(xs, |(n, {a, b: _})| a)
type W<'a> = Abstract<'a>; let w = W({a: 1, b: 2}); select w { W({a, ..}) => a }
type W<'a> = Abstract<'a>; let w = W({a: 1, b: 2}); select w { W<{a: i64, b: i64}> as W({a, ..}) => a }
type W<'a> = Abstract<'a>; let w = W({a: 1, b: 2}); select w { W({a, b: _}) => a }
PROGS
rm -rf "$d"
