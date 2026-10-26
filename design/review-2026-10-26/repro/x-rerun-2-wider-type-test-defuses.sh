#!/bin/sh
# Title: tests-select-03 variant: a type test that every value of the scrutinee passes,
#   but whose predicate is WIDER than the scrutinee's type, still de-fuses the select.
#   type_test_is_trivial (fusion/emit/select.rs) compares the predicate to the
#   scrutinee's type by equality, so only `string as t` over a string is trivial.
# Command: sh x-rerun-2-wider-type-test-defuses.sh [graphix]
# Expected: no output (every #[native] builds, as `i64 as x` over an i64 does).
# Observed:
#   emit_clif: type predicate Number not lowerable
#   emit_clif: select type predicate Any doesn't freeze concrete
#   emit_clif: type predicate [i64, string] not lowerable
G=${1:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
for prog in \
  'let f = |s: i64| -> i64 #[native] select s { Number as x => 1 }; f(1)' \
  'let f = |s: i64| -> i64 #[native] select s { Any as x => 1 }; f(1)' \
  'let f = |s: i64| -> i64 #[native] select s { [i64, string] as x => 1 }; f(1)' \
  'let f = |s: i64| -> i64 #[native] select s { i64 as x => x }; f(1)'
do
  printf '%s\n' "$prog" > "$D/p.gx"
  timeout -s KILL 30 "$G" --no-cache --expand "$D/p.gx" 2>&1 | grep 'emit_clif'
done
rm -rf "$D"
