#!/bin/sh
# Title: three "tests inside a value of ..." refusals in lang::traits tell a
#   remedy no fixture pins: written `T as [`J]` over a default's receiver, over a
#   trait-typed parameter, and over an impl's head variable met through a handle.
# Command: sh design/review-2026-11-16/repro/tests-traits-a-told-remedies-unpinned.sh
# Expected: each told remedy checks and runs (2, "o", 2).
# Observed: they do today; nothing in the tree pins them.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
cat > $d/default-receiver.gx <<'GX'
trait Tag { val tag: fn(self) -> i64 = |x| select [x] { Array<[`J, i64]> as [`J] => 1, _ => 2 } };
impl Tag for i64 {};
Tag::tag(1)
GX
cat > $d/impl-variable.gx <<'GX'
trait Show { val show: fn(self) -> string };
impl<'a> Show for Array<'a> { let show = |x| select [x[0]$] { Array<[`J, i64]> as [`J] => "j", _ => "o" } };
Show::show([1])
GX
cat > $d/trait-argument.gx <<'GX'
trait Show { val show: fn(self) -> string };
impl Show for i64 { let show = |x| "i" };
let f = |s: Show| select [s] { Array<[`J, i64]> as [`J] => 1, _ => 2 };
f(1)
GX
for f in default-receiver impl-variable trait-argument; do
  echo "== $f"; timeout -s KILL 10 $G --no-cache $d/$f.gx
done
rm -rf $d
