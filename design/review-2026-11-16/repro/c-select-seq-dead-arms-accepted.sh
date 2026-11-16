#!/bin/sh
# Title: the check accepts arms nothing reaches: a written test repeated with a
#   name or `_`, an inferred pattern repeated under a written test (or the
#   reverse), and a catch-all behind written tests the check itself judges
#   exhaustive.
# Command: sh design/review-2026-11-16/repro/c-select-seq-dead-arms-accepted.sh
# Expected (CLAUDE.md "Select: exhaustive, no dead arms"; select_coverage.md: an
#   arm "that tests what an earlier unguarded arm tests ... is dead"): every
#   program marked DEAD is refused, as its twin marked TWIN is.
# Observed: every DEAD program checks (exit 0, no output); the twins are refused.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
n=0
t() {
    n=$((n + 1))
    printf '%s\n' "$2" > "$D/p$n.gx"
    out=$(timeout -s KILL 30 "$G" --check "$D/p$n.gx" 2>&1 | tail -1)
    printf '%s: %s\n    -> %s\n' "$1" "$2" "${out:-accepted}"
}
# check_repeats skips an atom that covers its test (a name or `_`), and
# compares arms only under EQUAL type tests (None against Some never is)
t DEAD '|x: Array<[i64, bool]>| select x { Array<i64> as _ => 0, Array<i64> as b => 1, _ => 2 }'
t TWIN '|x: Array<[i64, bool]>| select x { Array<i64> as [a, ..] => 0, Array<i64> as [b, ..] => 1, _ => 2 }'
t DEAD '|x: (([i64, bool], i64), bool)| select x { ((bool, i64), bool) as _ => 0, ((bool, i64), bool) as b => 1, _ => 2 }'
t DEAD '|x: bool| select x { true => 0, bool as true => 1, _ => 2 }'
t DEAD '|x: bool| select x { bool as true => 0, true => 1, _ => 2 }'
t DEAD '|x: [`A(i64), `B]| select x { `A(1) => 0, `A(i64) as `A(1) => 1, _ => 2 }'
t TWIN '|x: [`A(i64), `B]| select x { `A(1) => 0, `A(1) => 1, _ => 2 }'
# Reach::arm finds a catch-all dead only where Type::diff emptied what reaches;
# Reach::exhausted then judges the same arms exhaustive by containment
t DEAD '|x: (([i64, bool], i64), bool)| select x { ((bool, i64), bool) as _ => 0, ((i64, i64), bool) as _ => 1, _ => 2 }'
t 'the same without the catch-all (exhaustive)' '|x: (([i64, bool], i64), bool)| select x { ((bool, i64), bool) as _ => 0, ((i64, i64), bool) as _ => 1 }'
t DEAD '|x: {a: {b: [i64, bool]}}| select x { {a: {b: i64}} as _ => 0, {a: {b: bool}} as _ => 1, _ => 2 }'
t TWIN '|x: ([i64, bool], i64)| select x { (i64, i64) as _ => 0, (bool, i64) as _ => 1, _ => 2 }'
rm -rf "$D"
