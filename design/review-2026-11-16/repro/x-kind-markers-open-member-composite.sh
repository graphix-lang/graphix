#!/bin/sh
# Title: an open member beside a tuple/struct/payload-variant test is refused
# though no other type shares the tested composite's runtime form.
# Command: sh design/review-2026-11-16/repro/x-kind-markers-open-member-composite.sh
# Expected (design/distinct_composite_forms.md: an open member is refused only
#   beside a reference, function, error, map, array or Rust-backed abstract;
#   composites collide only where their parts may): 3, 4 and 5 check, as
#   1 (string) and 2 (bare variant) do. A written test checks every part at
#   run time (`Any` holding `A("s") does not pass `A(i64)`).
# Observed: 3, 4, 5 refused "... may share (i64, i64)'s runtime form: annotate it",
#   with nothing a declared 'a can be annotated with short of fixing it.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
echo '|x: ['"'"'a, string]| select x { string as s => 0, _ => 1 }' > $d/1.gx
echo '|x: ['"'"'a, `A]| select x { `A as a => 0, _ => 1 }' > $d/2.gx
echo '|x: ['"'"'a, (i64, i64)]| select x { (i64, i64) as (a, b) => a + b, _ => 1 }' > $d/3.gx
echo '|x: ['"'"'a, {n: i64}]| select x { {n: i64} as s => s.n, _ => 1 }' > $d/4.gx
echo '|x: ['"'"'a, `Tick(i64)]| select x { `Tick(i64) as `Tick(n) => n, _ => 1 }' > $d/5.gx
for f in 1 2 3 4 5; do echo "== $f: $(cat $d/$f.gx)"; timeout -s KILL 30 $G --no-cache --check $d/$f.gx 2>&1 | tail -1; done
rm -rf $d
