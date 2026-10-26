#!/bin/sh
# Title: a written type variable inside a lambda's BODY (a let annotation,
# or the signature of a lambda nested two deep) does not name the variable
# an enclosing definition declares: make_init takes the enclosing
# signatures from ctx.body when the instance is built, which is empty at a
# nested literal's definition check, not from the literal's lexical context.
# Command: sh c-lambda-enclosing-tvars-depth.sh /path/to/graphix
# Rule (CLAUDE.md, `let rec` paragraph): a name a definition around a written
# type declares is that definition's variable.
# Case 1 expected: refused (z: 'a is mk's rigid 'a, y is i64); observed: (1, 2.5)
# Case 2 expected: refused like case 3 ('a: i64 does not contain string);
#   observed: ["s"]
# Case 3 (one level of nesting, the existing pin's shape): refused, as expected.
B=${1:-graphix}
D=$(mktemp -d)
cat > $D/case1.gx <<'GX'
let f = 'a: Number |x: 'a| {
  let g = |y: i64| { let z: 'a = y; z };
  (g(1), x)
};
f(2.5)
GX
cat > $D/case2.gx <<'GX'
let mk = 'a: Number |k: 'a| { let mid = |u: i64| |x: 'a| [x]; (mid(0), k) };
let (g, k) = mk(1);
g("s")
GX
cat > $D/case3.gx <<'GX'
let mk = 'a: Number |k: 'a| { let mid = |x: 'a| [x]; (mid, k) };
let (g, k) = mk(1);
g("s")
GX
for c in case1 case2 case3; do
  echo "== $c"; timeout -s KILL 20 $B --no-cache $D/$c.gx 2>&1 | tail -1
done
rm -rf $D
