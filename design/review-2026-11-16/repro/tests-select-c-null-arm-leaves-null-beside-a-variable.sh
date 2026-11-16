#!/bin/sh
# Title: an arm taking the null at a part that holds a declared variable beside
#   null subtracts nothing: the next arm's bind still holds null.
# Command: sh tests-select-c-null-arm-leaves-null-beside-a-variable.sh
# Expected: all four programs check (no output after each "==").
# Observed (HEAD e1be2fbf2):
#   == variable: missing match cases: no arm covers [null, 'b: unbound within [i64, string]]
#   == variable-result: type mismatch [i64, string] does not contain [i64, null, 'b: [i64, string]]
#   == concrete:            (checks)
#   == variable-and-i64:    (checks)
# Cause (read): Type::diff_one_part takes parts_of(&d) of the part's difference;
#   ['b, null] less null is the lone open cell 'b, for which parts_of
#   (deref_cloned) is None, so diff_one_part gives up and the residual keeps
#   the whole part. With a second member left (['b, i64]) the difference is a
#   Set and the null is subtracted.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/variable.gx <<'GX'
let u = 'b: [i64, string] |x: `B(['b, null])| select x {
    `B(null) => 1,
    `B(v) => select v { i64 as n => n, string as s => str::len(s) }
};
let w: [i64, string] = 7;
[u(`B(w)), u(`B(null))]
GX
cat > $D/variable-result.gx <<'GX'
let u = 'b: [i64, string] |x: `B(['b, null])| select x {
    `B(null) => 1,
    `B(v) => v
};
let w: [i64, string] = 7;
let r: [i64, string] = u(`B(w));
r
GX
cat > $D/concrete.gx <<'GX'
let u = |x: `B([i64, string, null])| select x {
    `B(null) => 1,
    `B(v) => select v { i64 as n => n, string as s => str::len(s) }
};
[u(`B(7)), u(`B(null))]
GX
cat > $D/variable-and-i64.gx <<'GX'
let u = 'b: [f64, string] |x: `B(['b, i64, null])| select x {
    `B(null) => 1,
    `B(v) => select v { i64 as n => n, f64 as f => 2, string as s => str::len(s) }
};
let w: [f64, string] = "s";
[u(`B(w)), u(`B(null)), u(`B(4))]
GX
for f in variable variable-result concrete variable-and-i64; do
  echo "== $f"
  timeout -s KILL 60 $G --no-cache --check $D/$f.gx 2>&1 | tail -1 | grep -v '^$' | sed 's/^ *[0-9]*: //'
done
rm -rf $D
