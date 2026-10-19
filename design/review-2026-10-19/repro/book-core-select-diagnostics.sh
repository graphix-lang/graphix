#!/bin/bash
# Title: select.md and error.md quote coverage diagnostics the checker no longer prints
# Command: bash design/review-2026-10-19/repro/book-core-select-diagnostics.sh
# Expected (the book): "missing match cases type mismatch [i64, Array<i64>] does not contain
#   [[i64, null], Array<i64>]" (select.md, twice), "pattern `Wrong will never match [..],
#   unused match cases" (select.md), "missing match cases type mismatch `ArithError(..) does
#   not contain .." (error.md)
# Observed: "missing match cases: no arm covers null", "missing match cases: no arm covers i64",
#   "unreachable arm: the earlier arms already cover the whole scrutinee, unused match cases",
#   "missing match cases: no arm covers `ArrayIndexError(string)"
D=$(mktemp -d); G=/home/eric/tmp/review-2026-10-19/bin/graphix
H='let x: [Array<i64>, i64, null] = null;
x <- sys::time::timer(duration:1.s, false) ~ [1, 2, 3, 4, 5];
x <- sys::time::timer(duration:2.s, false) ~ 7;'
printf '%s\nselect x {\n  Array<i64> as a => array::fold(a, 0, |s, x| s + x),\n  i64 as n => n\n}\n' "$H" > $D/a.gx
printf '%s\nselect x {\n  Array<i64> as a => array::fold(a, 0, |s, x| s + x),\n  i64 as n if n > 10 => n,\n  null as _ => 42\n}\n' "$H" > $D/b.gx
cat > $D/c.gx <<'G'
let v: [`Bare, `Arg(i64), `MoreArg(string, i64)] = `Bare;
select v {
  `Bare => "it's bare, no argument",
  `Arg(i) => "it has an arg [i]",
  x@ `MoreArg(s, n) => "it's big [x] with args \"[s]\" and [n]",
  `Wrong => "this won't compile"
}
G
cat > $D/d.gx <<'G'
let a = [0, 1, 2, 3];
catch(e) select (e.0).error {
    `ArithError(s) => println("arithmetic operation error [s]")
};
(a[0]? +? a[1]?)?
G
for f in a b c d; do timeout -s KILL 10 $G --check $D/$f.gx 2>&1 | tail -1; done
rm -rf $D
