#!/bin/sh
# A type test that every value of the scrutinee passes (`string as t` over a
# string, `T as {a, ..}` over a T) de-fuses the select, while the same arm
# without the test fuses.
# run: sh tests-select-trivial-type-test-defuses.sh
# expected: no output (every `#[native]` builds)
# observed:
#   emit_clif: type predicate string over scrutinee string not lowerable
#   emit_clif: type predicate { a: i64, b: i64 } not lowerable
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
for prog in \
  'let f = |s: string| -> i64 #[native] select s { string as t => str::len(t) }; f("ab")' \
  'type T = {a: i64, b: i64}; let f = |s: T| -> i64 #[native] select s { T as {a, ..} => a }; f({a: 1, b: 2})' \
  'let f = |s: {a: i64, b: i64}| -> i64 #[native] select s { {a, ..} => a }; f({a: 1, b: 2})'
do
  printf '%s\n' "$prog" > "$D/p.gx"
  timeout -s KILL 30 "$G" --no-cache --expand "$D/p.gx" 2>&1 | grep 'emit_clif'
done
rm -rf "$D"
