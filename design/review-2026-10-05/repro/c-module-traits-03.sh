#!/usr/bin/env bash
# c-module-traits-03: an interface `val` proxies EVERY top-level binding
# of its name, not the effective (last) one.
#
# check_sig (graphix-compiler/src/node/module.rs:379-402) pairs each name
# a body `let` binds with the sig binding of that name by name lookup, so
# a body that shadows an exported name (`let x = ..; let x = ..`: legal,
# book/src/core/let_binds.md; "idiomatic", design/module_system.md) wires
# both bindings to the one exported id. Module::update copies both
# productions out and a write to the export into both;
# proxy_lambda_defs maps the export to the first binding's lambda when
# the last is not a lambda literal, so calls resolve statically to the
# shadowed function; and the sig is checked against the shadowed
# binding's type too. A dynamic module's sig check is the same code
# (cases 2 and 4 reproduce with `mod m dynamic { .. }`).
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/c-module-traits-03.sh
#
# Each case runs one m.gx with its m.gxi and without it (the control).
# expected: both columns equal; the interface only declares what m
# already exports (its effective x / f):
#   1. m::f(1), body `let f = |..| x + 1; let g = |..| x + 100; let f = g`: 101
#   2. m::x, body `let x = timer(100.ms, true) ~ 1; let x = 100`: 100
#   3. m::y, body `let x = 0; let y = x + 1000; let x = 5`, main writes
#      m::x <- 7 at 100 ms: 1000 (the write reaches the last x only)
#   4. val x: string, body `let x = 1; let x = "a"`: "a"
# observed (HEAD c722befe, debug build; the same with --no-fusion, with a
# cold or a warm image, and with GRAPHIX_PAR=force):
#   1. with gxi: 2 (the shadowed f is called)       control: 101
#   2. with gxi: 100, then 1 at every tick (the shadowed x wins)  control: 100
#   3. with gxi: 1000 1007 (the write reached the shadowed x)  control: 1000
#   4. with gxi: compile error `signature mismatch "val x: ...", signature
#      has type string, implementation has type i64`  control: "a"
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

# case <name> <gxi> <m.gx> <main tail>: run with and without the gxi
case_() {
    local name=$1 gxi=$2 body=$3 tail=$4
    for v in gxi control; do
        local d="$dir/$name/$v"
        mkdir -p "$d"
        printf '%s\n' "$body" > "$d/m.gx"
        [ "$v" = gxi ] && printf '%s\n' "$gxi" > "$d/m.gxi"
        printf 'mod m;\nsys::exit(sys::time::timer(duration:550.ms, false) ~ 0);\n%s\n' \
            "$tail" > "$d/main.gx"
        printf '%-28s %-8s ' "$name" "$v"
        (cd "$d" && timeout -s KILL 30 "$GRAPHIX" --no-cache main.gx 2>&1) \
            | grep -v '^\s*$' | tr '\n' ' '
        echo
    done
}

case_ 1-call-shadowed-fn 'val f: fn(x: i64) -> i64;' \
'let f = |x: i64| -> i64 x + 1;
let g = |x: i64| -> i64 x + 100;
let f = g' \
'm::f(1)'

case_ 2-value-shadowed-x 'val x: i64;' \
'let x = sys::time::timer(duration:100.ms, true) ~ 1;
let x = 100' \
'm::x'

case_ 3-write-reaches-shadowed 'val x: i64;
val y: i64;' \
'let x = 0;
let y = x + 1000;
let x = 5' \
'm::x <- sys::time::timer(duration:100.ms, false) ~ 7;
m::y'

case_ 4-sig-vs-shadowed-type 'val x: string;' \
'let x = 1;
let x = "a"' \
'm::x'
