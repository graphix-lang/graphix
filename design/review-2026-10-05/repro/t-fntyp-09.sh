#!/usr/bin/env bash
# t-fntyp-09: union normalization depends on member order, so an
# implementation whose union is written or inferred in another order is
# refused against its interface.
#
# Type::flatten_set_tracked (graphix-types/src/typ/normalize.rs:97-165)
# merges members greedily in arrival order and sorts only afterwards, so
# one member set has several normal forms:
#   [(i64, bool), (string, bool), (i64, f64)] -> [(i64, f64), ([i64, string], bool)]
#   [(i64, f64), (i64, bool), (string, bool)] -> [(i64, [f64, bool]), (string, bool)]
# The two are the same type (contains holds both ways, and both admit
# ("a", true), (1, true) and (1, 2.0)), but the interface checks compare
# them by position: Type::sig_matches pairs Set members in order
# (graphix-types/src/typ/matches.rs:287) and a typedef body is compared
# with `!=` (graphix-compiler/src/node/module.rs:477). Each
# implementation alone, without m.gxi, prints (1, true).
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-fntyp-09.sh
#
# expected: every case prints (1, true).
# observed (HEAD c722befe, debug build):
#   1-typedef-reordered    signature mismatch in T, expected type T =
#                          [(i64, f64), ([i64, string], bool)], found type T =
#                          [(i64, [f64, bool]), (string, bool)]
#   2-val-reordered        signature mismatch "val x: ...", signature has type
#                          [(i64, f64), ([i64, string], bool)], implementation has
#                          type [(i64, [f64, bool]), (string, bool)]
#   3-val-inferred-select  signature mismatch "val x: ...", ... type mismatch:
#                          signature has f64, implementation has [f64, bool]
#                          (the select's arms are the three tuples in another order)
#   4-control-same-order   (1, true)
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

case_() {
    local name=$1 gxi=$2 body=$3
    local d="$dir/$name"
    mkdir -p "$d"
    printf '%s\n' "$gxi" > "$d/m.gxi"
    printf '%s\n' "$body" > "$d/m.gx"
    printf 'mod m;\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\nm::x\n' > "$d/main.gx"
    printf '%-24s ' "$name"
    (cd "$d" && timeout -s KILL 30 "$GRAPHIX" --no-cache main.gx 2>&1) \
        | grep -v '^\s*$' | tail -n 2 | tr '\n' ' '
    echo
}

case_ 1-typedef-reordered \
'type T = [(i64, bool), (string, bool), (i64, f64)];
val x: T;' \
'type T = [(i64, f64), (i64, bool), (string, bool)];
let x: T = (1, true)'

case_ 2-val-reordered \
'val x: [(i64, bool), (string, bool), (i64, f64)];' \
'let x: [(i64, f64), (i64, bool), (string, bool)] = (1, true)'

case_ 3-val-inferred-select \
'val x: [(i64, bool), (string, bool), (i64, f64)];' \
'let c = 1;
let x = select c { 0 => (1, 2.0), 1 => (1, true), _ => ("a", true) }'

case_ 4-control-same-order \
'val x: [(i64, bool), (string, bool), (i64, f64)];' \
'let c = 1;
let x = select c { 1 => (1, true), 2 => ("a", true), _ => (1, 2.0) }'
