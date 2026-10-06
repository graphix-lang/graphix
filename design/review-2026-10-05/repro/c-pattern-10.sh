#!/usr/bin/env bash
# c-pattern-10: an or-alternative that matches anything is read as covering
# every member of the arm's inferred Set, not just its own.
#
# Under an inferred predicate each alternative compiles against its own
# member (alt_types, graphix-compiler/src/node/pattern.rs:472-488), and
# matches_anything only means "any value of that member". Two sites ignore
# that:
#  1. the dead-alternative check (pattern.rs:496-503) refuses every later
#     alternative, whatever its member (case refused_or);
#  2. matches_anything(Or) (pattern.rs:1052) is "any alternative does", so
#     matches_every (pattern.rs:1148-1152) calls the arm a wildcard and
#     check_coverage (select.rs:262-304) returns early; a non-exhaustive
#     select is accepted (case gap_or).
# The same selects written as separate arms are judged correctly (controls).
# struct_or is the reviewer's case: refused the same way. The refusal there
# also hides a second bug: the runtime Or (is_match :906, bind :787) picks
# an alternative by structure only, so {x: 1, y: 2}, a 2-element array of
# pairs, would match (x, _) and bind x to ["x", 1].
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/c-pattern-10.sh
#
# expected:
#   refused_or  accepted, prints [5, 7]
#   two_arms    (control) accepted, prints [5, 7]
#   struct_or   accepted (once the runtime Or tests member types), [5, 1]
#   gap_or      refused: missing match cases ((7, 8, 9) matches no arm)
#   gap_arms    (control) refused: missing match cases
# observed (HEAD c722befe, debug build; the same with --no-fusion):
#   refused_or  exit 1: unreachable or-pattern alternative: an earlier
#               alternative already matches anything
#   two_arms    [5, 7]
#   struct_or   exit 1: the same message
#   gap_or      accepted; prints "a 5" and "b 7", never a line for c
#   gap_arms    exit 1: missing match cases type mismatch ('_..: i64, Any)
#               does not contain [(i64, i64), (i64, i64, i64)]
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
U='type U = [(i64, i64), (i64, i64, i64)];'
S='type U = [(i64, i64), {x: i64, y: i64}];'
EXIT='sys::exit(sys::time::after_idle(duration:100.ms, 0))'
declare -A progs=(
    [refused_or]="$U let f = |t: U| select t { (x, _) | (x, _, _) => x }; println(\"[[f((5, 6)), f((7, 8, 9))]]\"); $EXIT"
    [two_arms]="$U let f = |t: U| select t { (x, _) => x, (x, _, _) => x }; println(\"[[f((5, 6)), f((7, 8, 9))]]\"); $EXIT"
    [struct_or]="$S let f = |t: U| select t { (x, _) | {x, ..} => x }; println(\"[[f((5, 6)), f({x: 1, y: 2})]]\"); $EXIT"
    [gap_or]="$U let f = |t: U| select t { (x, 0, _) | (x, _) => x }; println(\"a [f((5, 6))]\"); println(\"b [f((7, 0, 9))]\"); println(\"c [f((7, 8, 9))]\"); $EXIT"
    [gap_arms]="$U let f = |t: U| select t { (x, 0, _) => x, (x, _) => x }; println(\"c [f((7, 8, 9))]\"); $EXIT"
)
for name in refused_or two_arms struct_or gap_or gap_arms; do
    printf '%s\n' "${progs[$name]}" > "$dir/$name.gx"
    out=$(timeout -s KILL 30 "$GRAPHIX" --no-cache "$dir/$name.gx" 2>&1)
    code=$?
    echo "== $name (exit $code)"
    if [ $code -eq 0 ]; then echo "$out"; else echo "$out" | tail -1; fi
done
