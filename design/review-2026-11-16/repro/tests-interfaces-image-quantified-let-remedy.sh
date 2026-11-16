#!/bin/sh
# Title: a val over a written let is told to "annotate the implementation's
#   let with a quantified type (fn<'a: ..>)", which no program can do.
# Command: sh design/review-2026-11-16/repro/tests-interfaces-image-quantified-let-remedy.sh
# Expected: following either remedy the refusal names makes the module check.
# Observed: case 1 (the fixture rank2_val_over_a_written_let_names_the_annotation)
#   is refused naming the quantified annotation; case 2 (that annotation on the
#   let) is refused "a function type with type variables of its own ... stands
#   only as a definition's signature"; case 3 (the variables declared on the
#   definition) is refused with the same advice again; only case 4 (the val at
#   one type, the first remedy) checks.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
run() {
  mkdir -p "$D/$1"; cd "$D/$1"
  printf '%s\n' "$2" > main.gx; printf '%s\n' "$3" > m.gxi; printf '%s\n' "$4" > m.gx
  echo "== $1"; timeout -s KILL 30 "$G" --check main.gx 2>&1 | tail -1
}
run 1-as-pinned 'mod m; (m::h(1), m::h("s"))' "val h: fn<'a: Any>(x: 'a) -> 'a" "let h = |x| x; h <- |x| x"
run 2-quantified-let 'mod m; (m::h(1), m::h("s"))' "val h: fn<'a: Any>(x: 'a) -> 'a" "let h: fn<'a: Any>(x: 'a) -> 'a = |x| x; h <- |x| x"
run 3-declared-on-lambda 'mod m; (m::h(1), m::h("s"))' "val h: fn<'a: Any>(x: 'a) -> 'a" "let h = 'a: Any |x: 'a| -> 'a x; h <- |x| x"
run 4-val-at-one-type 'mod m; m::h(1)' "val h: fn(x: i64) -> i64" "let h = |x| x; h <- |x| x"
rm -rf "$D"
