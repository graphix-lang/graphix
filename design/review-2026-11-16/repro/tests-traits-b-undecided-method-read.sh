#!/bin/sh
# Title: a method whose value reads a binding still undecided at the impl is
#   refused with "'_<n> is not a function", naming neither the binding nor
#   that annotating it is the fix.
# Command: sh design/review-2026-11-16/repro/tests-traits-b-undecided-method-read.sh
# Expected: a refusal saying `h` is undecided here and to annotate `h` (as
#   lang::traits::method_return_default_undecided_under_a_declared_variable_refused
#   says for `k`); the annotated program checks.
# Observed: leaf "'_8187 is not a function" (a raw generated variable, and
#   h is a function); the annotated program checks.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/undecided.gx <<'G1'
let h = never();
trait T { val m: fn(self) -> i64 };
impl T for i64 { let m = h };
h <- once(|s: i64| s + 1);
T::m(1)
G1
sed 's/^let h = never()/let h: fn(s: i64) -> i64 = never()/' $D/undecided.gx > $D/annotated.gx
for f in undecided annotated; do
  echo "== $f"
  timeout -s KILL 30 $G --check --no-cache $D/$f.gx 2>&1 | grep -v '^ *[0-9]*: at' | tail -3
  echo "exit=$?"
done
rm -rf $D
