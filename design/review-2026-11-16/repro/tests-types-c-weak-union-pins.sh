#!/bin/bash
# Title: two refusal pins in lang::types that the claimed case never reaches.
# Command: bash design/review-2026-11-16/repro/tests-types-c-weak-union-pins.sh
# (1) decided_argument_into_a_typedef_union_no_member_holds_it_refused: x stands first, so
#     f([1.5], 1, "s") gets the same "could go to Array<'a: unbound> or Array<'b: unbound>"
#     refusal as f([1], 1, "s"); with the deciders first the claimed case is a plain mismatch.
# (2) union_bound_one_admitting_member_decides_the_cell pins "cannot hold f64"; the write
#     that tells "z is i64" from "z is [i64, string]" is z <- "s".
# Observed (HEAD e1be2fbf2): see output.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
run() { printf '%s\n' "$2" > $d/p.gx; printf '%-34s -> ' "$1";
  if timeout -s KILL 60 $G --check --no-cache $d/p.gx > $d/out 2>&1; then echo accepted; else tail -1 $d/out | sed 's/^ *[0-9]*: //' | cut -c1-170; fi; }
run "(1) fixture f([1.5], 1, \"s\")" "type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: P<'a, 'b>, y: 'a, z: 'b| x; f([1.5], 1, \"s\")"
run "(1) same with [1]" "type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: P<'a, 'b>, y: 'a, z: 'b| x; f([1], 1, \"s\")"
run "(1) deciders first, [1.5]" "type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |y: 'a, z: 'b, x: P<'a, 'b>| x; f(1, \"s\", [1.5])"
run "(2) z <- 2.5 (fixture)" "let f = 'a: [(i64, i64), (string, string)] |v: 'a| v; let z = never(); let r = f((z, 1)); z <- 2.5; r"
run "(2) z <- \"s\"" "let f = 'a: [(i64, i64), (string, string)] |v: 'a| v; let z = never(); let r = f((z, 1)); z <- \"s\"; r"
rm -rf $d
