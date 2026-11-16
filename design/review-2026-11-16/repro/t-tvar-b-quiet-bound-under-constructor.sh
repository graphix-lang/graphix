#!/bin/bash
# A body adds a bound to a declared variable quietly when the variable sits
# under a tuple, struct or variant in the argument.
#
# Run: bash design/review-2026-11-16/repro/t-tvar-b-quiet-bound-under-constructor.sh
#
# Rule (design/tvar_constraints.md, "A body adds no bound to a declared
# variable"): a body that needs more of a declared quantifier than its bound
# says is refused, "the body needs 'a within .., write 'a: ..": nothing
# tightens the signature quietly.
#
# Expected: definitions 1-7 refused with "the body needs 'a within".
# Observed: 1 and 2 are refused; 3-7 check (exit 0), and 8 shows the
#           signature was tightened: a call of the `'a: Any` function is
#           refused with "'a must be Discernible".
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
n=0
t() {
    n=$((n + 1))
    printf '%s\n' "$1" > "$d/p.gx"
    echo "== $n: $1"
    timeout -s KILL 60 "$G" --no-cache --check "$d/p.gx" 2>&1 | grep -o "the body needs.*\|'a must be Discernible.*" | cut -c1-120
    echo "   exit ${PIPESTATUS[0]}"
}
t "let f = 'a: Any |x: 'a| uniq(x); f(1)"
t "let f = 'a: Any |x: 'a| uniq([x]); f(1)"
t "let f = 'a: Any |x: 'a| uniq((x, 1)); f(1)"
t "let f = 'a: Any |x: 'a| uniq({a: x, b: 1}); f(1)"
t "let f = 'a: Any |x: 'a| json::write_str(\`A(x)); f(1)"
t "let f = 'a: Any |x: 'a| array::dedup([(x, 1)]); f(1)"
t "let f = 'a: Any |x: Array<'a>| uniq(x); f([1])"
t "let f = 'a: Any |x: 'a| uniq({a: x, b: 1}); let g: [fn(x: i64) -> i64, fn(x: string) -> string] = |x: i64| x; f(g)"
rm -rf "$d"
