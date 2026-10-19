#!/bin/bash
# Flat programs whose types are as deep as the program is long: only the Array shape checks
# in linear time (the pin flat_type_chain_checks_in_linear_time covers only that shape).
# run: bash x-stack-flat-chain-quadratic.sh
# expected: every shape checks in time linear in the number of lets, like `[x]`
# observed (graphix --check, debug build):
#   [x]            10000 lets 0.7 s, 20000 0.75 s
#   {a: x}         5000 0.85 s, 10000 4.2 s, 20000 17.9 s   (BuildGraph phase: Bind::compile's
#   (x, 1), `A(x)  same as the struct                        let shape check -> TVar occurs check)
#   &x             20000 45.6 s (Typecheck0 5.5 s + BuildGraph 3.3 s at 10000)
#   |a: i64| x     1000 2.5 s, 2000 10.4 s, 2500 18 s, 4000 51 s, 5000 OOM-killed at the 6 GB cap (one run)
#   {"k" => x}     20000 > 120 s
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
gen() { # $1 = the value built from the previous let, X standing for it; $2 = lets
  { echo "let x0 = 1;"; for ((i = 1; i <= $2; i++)); do echo "let x$i = ${1//X/x$((i - 1))};"; done; echo 1; } > $D/p.gx
}
for shape in '[X]' '{a: X}' '(X, 1)' '`A(X)' '&X' '|a: i64| X'; do
  for n in 2500 5000; do
    gen "$shape" $n
    s=$(date +%s.%N); timeout -s KILL 120 $G --no-cache --check $D/p.gx > /dev/null; rc=$?
    echo "$shape $n rc=$rc $(echo "$(date +%s.%N) - $s" | bc) s"
  done
done
rm -rf $D
