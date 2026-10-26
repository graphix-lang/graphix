#!/bin/sh
# Title: a module's foreign-write refusal names a sibling module's local
#        binding, by line and column only, as if it were in the parent file
# Command: sh t-tvar-decided-binds-sibling.sh
# Expected: the refusal of `mod n` names `x` (main.gx line 1) alone: `v` is a
#   local of the sibling module m, which n cannot see and need not annotate.
# Observed: "compiling module n ... annotate `x` (line 1, column 5), `v`
#   (line 1, column 15)": `v` is at m.gx line 1, column 15; main.gx line 1
#   column 15 is past the end of `let x = never();`.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
dir=$(mktemp -d)
cd "$dir" || exit 1
printf 'let x = never();\nmod m;\nmod n;\nx <- 1;\n(m::y, n::z)\n' > main.gx
printf 'val y: i64;\n' > m.gxi
printf 'let y = { let v = super::x; 1 };\n' > m.gx
printf 'val z: string;\n' > n.gxi
printf 'let z = { super::x <- "a"; "b" };\n' > n.gx
timeout -s KILL 30 "$G" --check main.gx
rm -rf "$dir"
