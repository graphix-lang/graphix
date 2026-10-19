#!/bin/sh
# t-fntyp-nested-bound-lost: a written fn type loses a quantifier's bound where the variable
# occurs twice inside a nested fn type
# usage: sh t-fntyp-nested-bound-lost.sh [path/to/graphix]
# Parsing `fn<'a: Number>(g: fn(x: 'a) -> 'a) -> f64`: the inner `fn(x: 'a) -> 'a` is
# declared first (its own FnType::declaring freezes its first 'a), so the outer declaring's
# alias_tvars cannot alias the inner 'a to the quantifier (TVar::alias returns on a frozen self).
# expected: (1) --check refuses f(|x: string| x); (2) the interface check accepts the identical
#   implementation; (3) fmt keeps `fn<'a: Number>` and its output still checks.
# observed: (1) exit 0; (2) "val f is declared fn(g: fn(x: 'a) -> 'a) -> f64 but implemented
#   fn<'a: Number>(..): the implementation requires 'a: Number, which the signature does not
#   declare"; (3) fmt prints `type F = fn(g: fn(x: 'a) -> 'a, y: 'a) -> f64` and the formatted
#   file is refused "undeclared type variable 'a'".
#   Control: `fn<'a: Number>(g: fn(x: 'a) -> i64) -> f64` (one inner occurrence) keeps its bound.
G=${1:-graphix}
D=$(mktemp -d)
printf "type F = fn<'a: Number>(g: fn(x: 'a) -> 'a) -> f64;\nlet f: F = |g| 0.0;\nf(|x: string| x)\n" > $D/one.gx
echo "== (1) check of a call violating the bound"; timeout -s KILL 60 $G --check $D/one.gx; echo "exit $?"
mkdir $D/m
printf "val f: fn<'a: Number>(g: fn(x: 'a) -> 'a) -> f64;\n" > $D/m/inner.gxi
printf "let f = 'a: Number |g: fn(x: 'a) -> 'a| -> f64 0.0;\n" > $D/m/inner.gx
printf 'mod inner;\ninner::f(|x: i64| x)\n' > $D/m/main.gx
echo "== (2) interface check of the identical implementation"; timeout -s KILL 60 $G --check $D/m/main.gx; echo "exit $?"
printf "type F = fn<'a: Number>(g: fn(x: 'a) -> 'a, y: 'a) -> f64;\nlet f: F = |g, y| 0.0;\nf(|x: i64| x, 1)\n" > $D/fmt.gx
echo "== (3) original check"; timeout -s KILL 60 $G --check $D/fmt.gx; echo "exit $?"
timeout -s KILL 20 $G fmt $D/fmt.gx; echo "fmt exit $?"; cat $D/fmt.gx
echo "== (3) formatted check"; timeout -s KILL 60 $G --check $D/fmt.gx; echo "exit $?"
rm -rf $D
