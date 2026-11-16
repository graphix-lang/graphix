#!/bin/sh
# a-language: what the check refuses in small ordinary programs, and what it tells the author
# command:  sh a-language-check-refusals.sh   (GRAPHIX=/path/to/graphix to override the binary)
# Each line is checked alone with `graphix --no-cache --check`; the last line of the refusal is printed.
# observed at e1be2fbf2 (debug build):
#   1 compose          refused: reading a fn needs its type known here: annotate the binding
#   2 apply_twice      refused: reading a fn needs its type known here: annotate the binding
#   3 swap             refused: reading a tuple needs its type known here: annotate the binding
#   4 field read       refused: reading a struct needs its type known here: annotate the binding
#   5 recursive sum    refused: type mismatch Array<`Leaf(i64)> does not contain Array<[`Leaf(i64), `Node(Array<`Leaf(i64)>)]>
#   6 x == null        refused: cannot compare [i64, null] with null .. both operands must be one type
#   7 nullable + 1     refused: .. both operands must be one numeric type (cast one side explicitly)
#   8 m{"a"} + 1       refused: .. both operands must be one numeric type (cast one side explicitly)
#   9 capture literal  refused: a capture `name@` takes a structure pattern
# 1-5: an ML-family checker (Hindley-Milner, MLsub) accepts each with no annotation.
# 6: the right operand is a member of the left operand's type. 7, 8: the fix is `x$ + 1`, not a cast.
# 9: `k@ pat => x <- k ~ v` is the documented way to act on entering an arm, and a literal arm cannot write it.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
n=0
while IFS= read -r prog; do
  n=$((n + 1))
  printf '%s\n' "$prog" > "$D/p.gx"
  printf '%s: ' "$n"
  timeout -s KILL 20 "$G" --no-cache --check "$D/p.gx" 2>&1 | grep -v '^$' | tail -1 | cut -c1-200
  [ -s "$D/p.gx" ] || echo
done <<'PROGS'
let compose = |f, g| |x| g(f(x)); let h = compose(|x| x + 1, |x| "[x]"); h(1)
let apply_twice = |f, x| f(f(x)); apply_twice(|x| x + 1, 1)
let pair = |a, b| (a, b); let swap = |p| (p.1, p.0); swap(pair(1, "a"))
let total = |xs| array::fold(xs, 0, |acc, x| acc + x.qty); total([{qty: 1}])
let rec sum = |t| select t { `Leaf(n) => n, `Node(kids) => array::fold(array::map(kids, sum), 0, |a, x| a + x) }; sum(`Node([`Leaf(1), `Node([`Leaf(2)])]))
let n: [i64, null] = 3; select n == null { true => 0, false => 1 }
let n: [i64, null] = 3; n + 1
let m = {"a" => 1}; m{"a"} + 1
let n = 0; let b = true; select b { t@ true => n <- t ~ 5, false => never() }; n
PROGS
rm -rf "$D"
