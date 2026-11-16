#!/bin/sh
# Title: a lambda formal's tuple/struct pattern over a union of same-shape
# tuples is refused as refutable, while a let over the same value accepts it.
# Command: sh design/review-2026-11-16/repro/x-kind-markers-formal-union-of-tuples.sh
# Expected: all three programs check (each pattern is irrefutable: every
#   member of the element type is a 2-tuple / a struct with fields a, b).
# Observed: (1) checks; (2) and (3) are refused:
#   "refutable patterns are not allowed in lambda arguments (i, v)"
#   "refutable patterns are not allowed in lambda arguments { a, b }"
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
echo 'let xs = [(1, `A), (2, `B(1))]; array::map(xs, |p| { let (i, v) = p; i })' > $d/1.gx
echo 'let xs = [(1, `A), (2, `B(1))]; array::map(xs, |(i, v)| i)' > $d/2.gx
echo 'let xs = [{a: 1, b: `A}, {a: 2, b: `B(1)}]; array::map(xs, |{a, b}| a)' > $d/3.gx
for f in 1 2 3; do echo "== $f"; timeout -s KILL 30 $G --no-cache --check $d/$f.gx 2>&1 | tail -1; done
rm -rf $d
