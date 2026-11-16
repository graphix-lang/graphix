#!/bin/bash
# Title: node::write_mismatch offers "declare z: <held ∪ written> where it is bound"
# without asking whether the use that decided z holds the wider type; where a bound,
# an invariant &mut formal or an operator decided it, following the advice is refused.
# Command: bash design/review-2026-11-16/repro/tests-types-c-declare-remedy.sh
# Expected: the named declaration makes the program pass, or no declaration is named.
# Observed (HEAD e1be2fbf2): see output -- each "(as told)" line is refused anew.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
run() { printf '%s\n' "$2" > $d/p.gx; printf '%-26s -> ' "$1";
  if timeout -s KILL 60 $G --check --no-cache $d/p.gx > $d/out 2>&1; then echo accepted; else tail -1 $d/out | sed 's/^ *[0-9]*: //' | cut -c1-190; fi; }
run "first write decides"   'let z = never(); z <- 1; z <- "s"; z'
run "  (as told)"           'let z: [i64, string] = never(); z <- 1; z <- "s"; z'
run "&mut bound decides"    "let f = 'a: &mut [i64, null] |v: 'a| v; let z = never(); let r = f(&mut z); z <- \"hello\"; *r"
run "  (as told)"           "let f = 'a: &mut [i64, null] |v: 'a| v; let z: [i64, null, string] = never(); let r = f(&mut z); z <- \"hello\"; *r"
run "union bound decides"   "let f = 'a: [(i64, i64), (string, string)] |v: 'a| v; let z = never(); let r = f((z, 1)); z <- 2.5; r"
run "  (as told)"           "let f = 'a: [(i64, i64), (string, string)] |v: 'a| v; let z: [i64, f64] = never(); let r = f((z, 1)); z <- 2.5; r"
run "arithmetic decides"    'let x = never(); let y = x + 1; x <- "s"; y'
run "  (as told)"           'let x: [i64, string] = never(); let y = x + 1; x <- "s"; y'
rm -rf $d
