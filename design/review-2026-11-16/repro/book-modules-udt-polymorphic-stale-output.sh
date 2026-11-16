#!/bin/bash
# Title: book/src/udt/polymorphic.md shows outputs the binary no longer prints; its programs sit
# in plain ``` fences, which examples_compile.rs never checks (variants.md and named.md too).
# Command: bash book-modules-udt-polymorphic-stale-output.sh
# Expected per the book: {x: 1, y: 1, z: 3.14}
#   and "type mismatch Point3<'a: [Int, Real]> does not contain {x: string, y: string, z: string}"
# Observed: {x: 1.0, y: 1.0, z: 3.14}
#   and "type mismatch Point3<'a: unbound within Number> does not contain { x: string, y: string, z: string }"
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d /home/eric/tmp/review-2026-11-16/work/book-modules-udt/poly.XXXX)
printf "type Point3<'a: Number> = {x: 'a, y: 'a, z: 'a};\nlet f = |p: Point3<'a>, x: 'a| {p with x: p.x + x};\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\nf({x: 0., y: 1., z: 3.14}, 1.)\n" > $D/a.gx
printf "type Point3<'a: Number> = {x: 'a, y: 'a, z: 'a};\nlet v: Point3<'a> = {x: \"foo\", y: \"bar\", z: \"baz\"};\nv\n" > $D/b.gx
timeout -s KILL 20 $G --no-cache $D/a.gx 2>&1 | tail -1
timeout -s KILL 20 $G --no-cache --check $D/b.gx 2>&1 | tail -1
