#!/usr/bin/env bash
# x-typecheck-patterns-06: coverage and dead-arm checks disagree; an
# exhaustive select is refused with or without a catch-all.
#
# check_dead_arms (graphix-compiler/src/node/select.rs:478) feeds every
# unguarded atom to the literal pool. check_coverage feeds it only
# refutable atoms (:268): a composite pattern whose only heads are
# payload-less variants, `(b, `B)` or `(`X, `A)`, is irrefutable, so it
# adds just its type to mtypes and the pool never sees it. And the itype
# pre-check (:308) runs before the pool and distributes one position
# only, so arms that split two variant positions are refused there.
#
#   mixed:  (true, `A), (b, `B), (false, `A) over (bool, [`A, `B]):
#           itype passes; the pool sees only the two bool arms and does
#           not complete; mtypes holds ('_, `B) alone (:339 refuses).
#   grid:   all four (`X|`Y, `A|`B) over ([`X, `Y], [`A, `B]): the
#           itype pre-check refuses the full enumeration.
# In both, check_dead_arms pools every arm, completes, and refuses the
# catch-all as unreachable.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/x-typecheck-patterns-06.sh
#
# expected: mixed and grid pass --check (every value matches an arm);
#   mixed_with and grid_with are refused for the dead `_` arm.
#
# observed (HEAD c722befe, debug build):
#   mixed:      exit 1, missing match cases type mismatch
#               ('_: bool, `B) does not contain (bool, [`A, `B])
#   mixed_with: exit 1, unreachable arm: the earlier arms already cover
#               the whole scrutinee, unused match cases
#   grid:       exit 1, missing match cases type mismatch [(`X, `A),
#               (`X, `B), (`Y, `A), (`Y, `B)] does not contain
#               ([`X, `Y], [`A, `B])
#   grid_with:  exit 1, unreachable arm: ... (as mixed_with)
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
mixed='(true, `A) => 1, (b, `B) => 2, (false, `A) => 3'
grid='(`X, `A) => 1, (`X, `B) => 2, (`Y, `A) => 3, (`Y, `B) => 4'
declare -A progs=(
    [mixed]="let f = |v: (bool, [\`A, \`B])| -> i64 select v { $mixed }; f((false, \`B))"
    [mixed_with]="let f = |v: (bool, [\`A, \`B])| -> i64 select v { $mixed, _ => 4 }; f((false, \`B))"
    [grid]="let f = |v: ([\`X, \`Y], [\`A, \`B])| -> i64 select v { $grid }; f((\`Y, \`B))"
    [grid_with]="let f = |v: ([\`X, \`Y], [\`A, \`B])| -> i64 select v { $grid, _ => 5 }; f((\`Y, \`B))"
)
for name in mixed mixed_with grid grid_with; do
    printf '%s\n' "${progs[$name]}" > "$dir/$name.gx"
    timeout -s KILL 60 "$GRAPHIX" --check "$dir/$name.gx" > "$dir/out" 2>&1
    echo "$name: exit $?"
    grep -E 'missing match cases|unreachable arm' "$dir/out" | sed 's/^ *[0-9]*: /    /'
done
