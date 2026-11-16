#!/bin/bash
# Title: callsite::ambiguous_parameter tells the author to "declare the parameters
# that decide them before this one" where no such move exists.
# Command: bash design/review-2026-11-16/repro/tests-types-c-ambiguous-remedy.sh
# Expected: a remedy that makes each program pass: (1) pass #y/#z, or write x's
#   type; (2) and (3) write x's type -- no parameter of l holds 'b or 'c, none of f's holds 'a or 'b.
# Observed (HEAD e1be2fbf2): all three end ".. a call's arguments are met in the
#   order of the parameters, so declare the parameters that decide them before this
#   one, or write the type of the parameter this argument is passed to, ..";
#   in (1) #y and #z already stand before x, and (4), passing them, checks.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
run() { printf '%s\n' "$2" > $d/p.gx; printf '%s -> ' "$1";
  if timeout -s KILL 60 $G --check --no-cache $d/p.gx > $d/out 2>&1; then echo accepted; else tail -1 $d/out | sed 's/^ *[0-9]*: //' | grep -o "a call's arguments.*"; fi; }
run "(1) omitted defaults declared first" "type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |#y: 'a = 1, #z: 'b = \"s\", x: P<'a, 'b>| x; f([1])"
run "(2) no parameter holds 'b or 'c" "type P3<'p, 'q, 'r> = [Array<'p>, Array<'q>, Array<'r>]; let l = |y: 'a, x: P3<'a, 'b, 'c>| x; l(\"s\", [1])"
run "(3) no other parameter at all" "type E<'p, 'q> = ['p, 'q]; let f = |x: E<'a, 'b>| -> i64 0; f(1)"
run "(4) the defaults passed" "type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |#y: 'a = 1, #z: 'b = \"s\", x: P<'a, 'b>| x; f(#y: 1, #z: \"s\", [1])"
rm -rf $d
