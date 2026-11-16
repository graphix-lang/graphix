#!/bin/sh
# Title: a val missing two bounds its implementation needs is told one of them;
#   declaring that one is refused for the next (two round trips, and the first
#   refusal names a declaration that does not make the module check).
# Command: sh design/review-2026-11-16/repro/tests-interfaces-image-one-bound-at-a-time.sh
# Expected: the first refusal names every bound the implementation needs,
#   'a: Number + Singleton (arithmetic's bound).
# Observed: step 1 says "the implementation requires 'a: Number, which the
#   signature does not declare"; step 2, with 'a: Number declared, says
#   "requires 'a: Singleton"; step 3 with both checks.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
run() {
  mkdir -p "$D/$1"; cd "$D/$1"
  printf '%s\n' "$2" > main.gx; printf '%s\n' "$3" > m.gxi; printf '%s\n' "$4" > m.gx
  echo "== $1"; timeout -s KILL 30 "$G" --check main.gx 2>&1 | tail -1 | sed 's/.*: the implementation/the implementation/'
}
run 1-no-bound 'mod m; m::double(2)' "val double: fn(x: 'a) -> 'a" "let double = |x| x + x"
run 2-number-as-told 'mod m; m::double(2)' "val double: fn<'a: Number>(x: 'a) -> 'a" "let double = |x| x + x"
run 3-both 'mod m; m::double(2)' "val double: fn<'a: Number + Singleton>(x: 'a) -> 'a" "let double = |x| x + x"
rm -rf "$D"
