#!/bin/sh
# Title: a trait method that returns a shared binding's value is told to
#   "give <b> a type that holds every 'a", which cannot make it pass.
# Command: sh design/review-2026-11-16/repro/tests-traits-b-remedy-does-not-pass.sh
# Expected: the refusal's remedy, applied (y: Any / k: fn(s: C, x: Any) -> Any),
#   makes each program check, or the refusal names a remedy that does.
# Observed: each refused program names that remedy (pinned by
#   lang::traits::method_over_a_written_lambda_names_it and
#   method_over_a_struct_of_lambdas_names_what_they_write); with it applied
#   the check refuses again: "type mismatch 'a: unbound does not contain Any".
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/written.gx <<'G1'
type C = Abstract<i64>;
let k = |s, x| x;
k <- |s, x| x;
trait Put { val put: fn(self, x: 'a) -> 'a };
impl Put for C { let put = |s, x| k(s, x) };
let c = C(1);
(Put::put(c, 1), Put::put(c, "s"))
G1
sed 's/^let k = /let k: fn(s: C, x: Any) -> Any = /' $D/written.gx > $D/written_fixed.gx
cat > $D/struct.gx <<'G2'
type C = Abstract<i64>;
trait Put { val put: fn(self, x: 'a) -> 'a };
let y = never();
let st = {k: |x| { y <- x; y }};
impl Put for C { let put = |s, x| st.k(x) };
let c = C(1);
(Put::put(c, 1), Put::put(c, "s"))
G2
sed 's/^let y = never()/let y: Any = never()/' $D/struct.gx > $D/struct_fixed.gx
for f in written written_fixed struct struct_fixed; do
  echo "== $f"
  timeout -s KILL 30 $G --check --no-cache $D/$f.gx 2>&1 | grep -v '^ *[0-9]*: at' | tail -1
done
rm -rf $D
