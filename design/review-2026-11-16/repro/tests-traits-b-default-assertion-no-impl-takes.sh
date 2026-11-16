#!/bin/sh
# Title: lang::traits::uncalled_trait_default_must_be_pure says the default
#   is held "though no impl or call takes it", but its impl takes `bump`'s
#   default; the cases it claims (no impl, or every impl overriding it) are
#   refused today and unpinned.
# Command: sh design/review-2026-11-16/repro/tests-traits-b-default-assertion-no-impl-takes.sh
# Expected: all three builds refused "#[pure]: this function is not a pure function".
# Observed: all three refused as expected (the fixture holds only `taken`).
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
T='trait Score {
    #[pure] val score: fn(self) -> i64 = |x| 1;
    #[pure] val bump: fn(self) -> i64 = |x| count(x)
};'
printf '%s\nimpl Score for i64 { let score = |x| x };\n0\n' "$T" > $D/taken.gx
printf '%s\nimpl Score for i64 { let score = |x| x; let bump = |x| x };\n0\n' "$T" > $D/overridden.gx
printf '%s\n0\n' "$T" > $D/no_impl.gx
for f in taken overridden no_impl; do
  echo "== $f"
  timeout -s KILL 20 $G --no-cache $D/$f.gx 2>&1 | grep -o '#\[pure\]: this function is not a pure function' | head -1
done
rm -rf $D
