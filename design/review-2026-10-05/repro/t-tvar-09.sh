#!/usr/bin/env bash
# t-tvar-09: normalizing a type rewrites the cells it reaches through
# TVar::bind (graphix-types/src/typ/tvar.rs:996), which goes through
# `decided`. In a module check (a module with an interface runs in its
# own compile task under OwnWrites) a select over a parent value whose
# cell holds a non-canonical binding (a union formed while a member was
# still open, e.g. `[y, i64]` with `y` written `i64` later) is recorded
# as a foreign decision, and the module is refused with "the check
# decides a type the code around the module left open", although the
# parent decided the type completely.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-tvar-09.sh
#
# expected: every case checks (exit 0); the parent's statements fix
#   x: i64 before `mod m`, and the module decides nothing.
# observed (HEAD c722befe, debug build):
#   1-module-select        exit 1: compiling module m: the check decides a
#                          type the code around the module left open
#   2-no-interface         exit 0  (same module without m.gxi: no task)
#   3-single-file          exit 0  (same program, one file)
#   4-parent-select-before exit 0  (the parent selects on x before `mod m`,
#                          normalizing its own cell)
#   5-parent-select-after  exit 1: same refusal (only the statement order
#                          differs from case 4)
#   6-array-literal        exit 1: same refusal (`let x = [y, 1]`)
# GRAPHIX_TASK_AUDIT=1 on case 1 prints two FOREIGN-WRITE backtraces,
# both TVar::bind <- TVar::normalize_int <- Type::normalize <-
# Select::typecheck0_with (one through Select::check_dead_arms).
# Related symptom, a different path: 7-sig-unnormalized, `let z =
# super::x + 1` against `val z: i64`, is refused by check_sig ("signature
# has i64, implementation has [i64, '_: i64]"), sig_matches compares the
# unnormalized binding; it checks when the parent selects on x first.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

# case <name> <main.gx> [<m.gx> [<m.gxi>]]
case_() {
    local d="$dir/$1"
    mkdir -p "$d"
    printf '%s\n' "$2" > "$d/main.gx"
    [ $# -ge 3 ] && printf '%s\n' "$3" > "$d/m.gx"
    [ $# -ge 4 ] && printf '%s\n' "$4" > "$d/m.gxi"
    local out rc
    out=$(cd "$d" && timeout -s KILL 30 "$GRAPHIX" --check main.gx 2>&1)
    rc=$?
    printf '%-24s exit %s %s\n' "$1" "$rc" "$(printf '%s' "$out" | tail -1 | sed 's/^ *//')"
}

PARENT='let b = true;
let y = never();
let x = select b { true => y, false => 1 };
y <- 2;'

case_ 1-module-select "$PARENT
mod m;
m::z" 'let z = select super::x { v => v }' 'val z: i64;'

case_ 2-no-interface "$PARENT
mod m;
m::z" 'let z = select super::x { v => v }'

case_ 3-single-file "$PARENT
let z = select x { v => v };
z"

case_ 4-parent-select-before "$PARENT
let w = select x { v => v };
mod m;
(m::z, w)" 'let z = select super::x { v => v }' 'val z: i64;'

case_ 5-parent-select-after "$PARENT
mod m;
let w = select x { v => v };
(m::z, w)" 'let z = select super::x { v => v }' 'val z: i64;'

case_ 6-array-literal 'let y = never();
let x = [y, 1];
y <- 2;
mod m;
m::z' 'let z = select super::x { [a, ..] => a, _ => 0 }' 'val z: i64;'

case_ 7-sig-unnormalized "$PARENT
mod m;
m::z" 'let z = super::x + 1' 'val z: i64;'
