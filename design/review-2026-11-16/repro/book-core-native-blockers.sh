#!/bin/sh
# Title: recursion.md lists reading a reference, an outward `<-` and a builtin without a fast call as
#   "the things that keep a body out of native code"; a `~` or an Any in the body does too
# Command: sh design/review-2026-11-16/repro/book-core-native-blockers.sh
# Expected (the book's list): both programs build, since neither body reads a reference, connects
#   outward, or calls a builtin without a fast call.
# Observed: both are refused: "#[native] expression did not fully fuse to native code:
#   the node-walk computes `n ~ acc + n`" and "the cast source's type Any has no kernel ABI form".
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d /home/eric/tmp/review-2026-11-16/work/book-core/natblk.XXXX)
cat > "$D/sample.gx" <<'GX'
let rec sum_to = |n: i64, acc: i64| -> i64 select n {
  0 => acc,
  _ => sum_to(n - 1, n ~ acc + n)
};
let total = #[native] sum_to(1000, 0);
total
GX
cat > "$D/any.gx" <<'GX'
let rec sum_to = |n: i64, acc: i64| -> i64 select n {
  0 => acc,
  _ => { let a: Any = acc; sum_to(n - 1, cast<i64>(a)$ + n) }
};
let total = #[native] sum_to(1000, 0);
total
GX
for f in sample any; do echo "== $f"; timeout -s KILL 10 $G --no-cache "$D/$f.gx" 2>&1 | tail -3; done
