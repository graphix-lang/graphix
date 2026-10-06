#!/usr/bin/env bash
# x-stack-03: Node does not shadow typecheck0_instance, so checking an
# instance body (instances by substitution) recurses through the vtable
# with no stack::ensure_sufficient guard, as deep as the body.
#
# The body is LEVELS nested parenthesized 1000-term `+` chains,
# `((x + .. + x) + x + .. + x) + x + .. + x`, about 1000 * LEVELS AST
# levels deep. The parser admits it: each chain stays under max_nesting
# operators and a paren level costs a few of the 1000 parser knots
# (`--check` accepts LEVELS=300, ~300k levels).
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/x-stack-03.sh
#
# expected: every run but --check and --expand prints 30970 (f(1) over
#   30970 terms of x), as the two controls do; none aborts.
# observed (HEAD c722befe, debug build):
#   check                 rc=0
#   expand                rc=134 "thread '<unknown>' has overflowed its stack"
#   call f(1)             rc=134, same
#   call, default flags   rc=134, same
#   GRAPHIX_NO_SUBST=1    30970 (the instance re-checks through the guarded
#                         Node::typecheck0)
#   same sum at top level 30970 (no instance: update/delete/drop are guarded)
#   gdb: ~26k frames of Add::typecheck0_instance calling itself with no
#   stacker frame between, entered from GXLambda::typecheck0 <-
#   CallSite::bind_instance <- CallSite::typecheck1, on a 2 MiB
#   tokio-rt-worker (run) or rayon worker (--expand) stack.
#   LEVELS=20 prints 20980 node-walk only; LEVELS=30, 50 and 100 abort.
#   (Aside: a run with fusion on is slow on such a body, 3.6 s at
#   LEVELS=2, 14 s at 5, 60 s at 10, over 120 s at 20; node-walk only
#   LEVELS=10 takes 0.8 s.)
set -u
GRAPHIX=${GRAPHIX:-graphix}
LEVELS=${LEVELS:-30}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
ulimit -c 0

chain() {
    local s=x i
    for ((i = 0; i < $1; i++)); do s+=" + x"; done
    printf '%s' "$s"
}
e=$(chain 999)
tail=$(chain 998)
for ((l = 0; l < LEVELS; l++)); do e="($e) + $tail"; done
exit_stmt='sys::exit(sys::time::after_idle(duration:100.ms, 0));'
printf 'let f = |x| %s;\n%s\nf(1)\n' "$e" "$exit_stmt" > "$dir/fn.gx"
printf 'let x = 1;\n%s\n%s\n' "$exit_stmt" "$e" > "$dir/top.gx"

run() {
    local label=$1
    shift
    printf '%-22s ' "$label"
    out=$(timeout -s KILL 120 "$@" 2>&1)
    rc=$?
    printf 'rc=%s %s\n' "$rc" "$(printf '%s' "$out" | grep -a -m1 -E 'overflow|^[0-9]+$')"
}

run "check" "$GRAPHIX" --check "$dir/fn.gx"
run "expand" "$GRAPHIX" --expand "$dir/fn.gx"
run "call f(1)" "$GRAPHIX" --no-cache --no-fusion "$dir/fn.gx"
run "call, default flags" "$GRAPHIX" --no-cache "$dir/fn.gx"
run "GRAPHIX_NO_SUBST=1" env GRAPHIX_NO_SUBST=1 "$GRAPHIX" --no-cache --no-fusion "$dir/fn.gx"
run "same sum at top level" "$GRAPHIX" --no-cache --no-fusion "$dir/top.gx"
