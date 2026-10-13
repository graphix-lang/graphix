#!/bin/sh
# t-contains-04: probes ignore rigid cells while commits enforce them:
# set_commit commits a probe-admitted member, fails on the rigid cell and
# leaves its binds behind; place_members gives up instead of taking the residue.
#
# run: sh design/review-2026-10-12/repro/t-contains-04.sh [path/to/graphix]
#
# expected: every case prints rc=0 (each has a valid typing; each control,
#   which writes that typing by hand, is accepted).
# observed (HEAD 7324207f):
#   leftover   rc=1  `f(1, "str")`: type mismatch i64 does not contain string
#              (the def is accepted, but y was bound to i64 by the failed
#              first member; the [(i64, i64), ..] twin and the annotated twin
#              are accepted)
#   setcommit  rc=1  [(i64, 'a), (string, string)] does not contain (i64, string)
#   placemem   rc=1  ['c, Array<'d>] does not contain [string, Array<i64>]
#   controls   rc=0
G=${1:-graphix}
D=$(mktemp -d)
run() {
    printf '%s\n' "$2" > "$D/$1.gx"
    timeout -s KILL 60 "$G" --check "$D/$1.gx" > "$D/$1.out" 2>&1
    echo "$1 rc=$? $(grep mismatch "$D/$1.out" | sed 's/^ *[0-9]*: //')"
}
run leftover 'let f = |x: '"'"'a, y| -> [(i64, '"'"'a), ([i64, string], string)] (y, "s");
f(1, "str")'
run leftover_concrete_twin 'let f = |x: '"'"'a, y| -> [(i64, i64), ([i64, string], string)] (y, "s");
f(1, "str")'
run leftover_annotated_twin 'let f = |x: '"'"'a, y: [i64, string]| -> [(i64, '"'"'a), ([i64, string], string)] (y, "s");
f(1, "str")'
run setcommit 'let f = |x: '"'"'a, y| -> [(i64, '"'"'a), (string, string)] (y, "s");
f(1, "str")'
run setcommit_annotated_twin 'let f = |x: '"'"'a, y: string| -> [(i64, '"'"'a), (string, string)] (y, "s");
f(1, "str")'
run placemem 'let g = |w: '"'"'d, v: ['"'"'c, Array<'"'"'d>]| 0;
let f = |x: '"'"'a, t: bool| -> i64 g(x, select t { true => [1, 2], false => "s" });
f(1, true)'
run placemem_string_twin 'let g = |w: '"'"'d, v: ['"'"'c, Array<'"'"'d>]| 0;
let f = |x: string, t: bool| -> i64 g(x, select t { true => [1, 2], false => "s" });
f("a", true)'
rm -rf "$D"
