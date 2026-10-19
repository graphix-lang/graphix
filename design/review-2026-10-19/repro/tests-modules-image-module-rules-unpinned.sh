#!/bin/bash
# Title: module_system.md resolution rules with no pin in any test
# Command: bash design/review-2026-10-19/repro/tests-modules-image-module-rules-unpinned.sh
# Expected (the design doc's rules): each case below is refused with the
#   message shown, except letarray, which runs (path roots consult
#   module-kind entries only).
# Observed (HEAD 9445a1d21): exactly that; but no test in the tree checks any
#   of these messages (rg over stdlib/graphix-tests, graphix-shell/tests,
#   graphix-types/src finds none), so a regression in any rule passes the gate.
#   fwd:      top not defined
#   twoexp:   `len` is already imported here (from `array`); rename one
#   collide:  `len` is already defined in this scope; use `as` to rename
#   pubuse:   re-exports (`pub use`) are not yet supported
#   climb:    `super` goes above the package root
#   gxi_use:  undefined type m::U  (a gxi `use` is a private import)
#   letarray: compiles
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d); cd "$d"
run() { printf '%s\n' "$2" > "$1.gx"; echo "== $1"; timeout -s KILL 30 "$G" --check "$1.gx" 2>&1 | tail -1; }
printf 'use super::top;\nlet x = top\n' > a.gx
run fwd 'mod a; let top = 1; a::x'
run twoexp 'use array::len; use str::len; len([1])'
run collide 'let len = 1; use array::len; len'
run pubuse 'pub use array::len; 1'
run climb 'use super::x; 1'
printf 'type T = i64;\n' > ta.gxi; printf 'let unused = 0\n' > ta.gx
printf 'use super::ta::T as U;\nval f: fn(x: U) -> U;\n' > m.gxi; printf 'let f = |x: U| -> U x\n' > m.gx
run gxi_use 'mod ta; mod m; let q: m::U = 1; m::f(q)'
run letarray 'let array = 1; array::len([1, 2]) + array'
rm -rf "$d"
