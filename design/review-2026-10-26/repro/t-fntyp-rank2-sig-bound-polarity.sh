#!/bin/sh
# Title: interface matching checks a rank-2 parameter's quantifier bound in the wrong direction
# Command: TMPDIR=<scratch> sh design/review-2026-10-26/repro/t-fntyp-rank2-sig-bound-polarity.sh
# Case A: the interface hands f a `g: fn<'b: Number + Singleton>`, the implementation
#   treats g as `fn<'c: Any>` and calls it at a string. Unsound pairing.
#   Expected: the module check refuses the implementation (its quantifier bound Any
#   does not imply the signature's Number + Singleton).
#   Observed: --check exits 0; the run refuses the instance of g("s") at
#   fn(x: string) -> string ("an instance at fn(x: string) -> string of a definition
#   typed fn(x: '_: unbound within Number & Singleton) ..."): check and build disagree.
# Case B: the opposite direction, which is sound (a function polymorphic over Any can be
#   used where one over Number + Singleton is wanted).
#   Expected: accepted, prints (2, 5).
#   Observed: refused: "the implementation requires 'b: Number, which the signature does
#   not declare".
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
T=$(mktemp -d)
mkdir -p $T/a $T/b
cat > $T/a/m.gxi <<'X'
val f: fn(g: fn<'b: Number + Singleton>(x: 'b) -> 'b) -> (i64, string);
X
cat > $T/a/m.gx <<'X'
let f = |g: fn<'c: Any>(x: 'c) -> 'c| (g(1), g("s"));
X
printf 'mod m;\nm::f(|x| x + x)\n' > $T/a/main.gx
cat > $T/b/m.gxi <<'X'
val f: fn(g: fn<'b: Any>(x: 'b) -> 'b) -> (i64, f64);
X
cat > $T/b/m.gx <<'X'
let f = |g: fn<'c: Number + Singleton>(x: 'c) -> 'c| (g(1), g(2.5));
X
printf 'mod m;\nm::f(|x| x)\n' > $T/b/main.gx
echo "== A --check"; timeout -s KILL 30 $G --check $T/a/main.gx; echo "exit $?"
echo "== A run"; timeout -s KILL 30 $G --no-cache $T/a/main.gx 2>&1 | tail -3
echo "== B run"; timeout -s KILL 30 $G --no-cache $T/b/main.gx 2>&1 | tail -2
rm -rf $T
